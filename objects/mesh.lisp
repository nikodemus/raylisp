(in-package :raylisp)

(defclass mesh (scene-object)
  ((indices
    :initform (required-argument :indices)
    :initarg :indices)
   (vertices
    :initform (required-argument :vertices)
    :initarg :vertices)
   (min
    :initform (required-argument :min)
    :initarg :min)
   (max
    :initform (required-argument :max)
    :initarg :max)
   (kd-tree
    :initform nil
    :accessor mesh-kd-tree)))

(defun %mesh-vertex (index vertex-index indices vertices)
  (aref vertices (aref indices index vertex-index)))

;;;; We compute a separate KD tree for each mesh. This tells the tree
;;;; code how to handle meshes.

(defmethod kd-set-size ((mesh mesh))
  (array-dimension (slot-value mesh 'indices) 0))

(defmethod map-kd-set (function (mesh mesh))
  (dotimes (i (kd-set-size mesh))
    (funcall function i)))

(defmethod make-kd-subset (subset (mesh mesh))
  (let ((result (make-array (length subset) :element-type 'fixnum))
        (p -1))
    (dolist (elt subset)
      (setf (aref result (incf p)) elt))
    result))

(defmethod kd-object-min (index (mesh mesh))
  (let* ((indices (slot-value mesh 'indices))
         (vertices (slot-value mesh 'vertices))
         (v1 (%mesh-vertex index 0 indices vertices))
         (v2 (%mesh-vertex index 1 indices vertices))
         (v3 (%mesh-vertex index 2 indices vertices)))
    (vec-min v1 v2 v3)))

(defmethod kd-object-max (index (mesh mesh))
  (let* ((indices (slot-value mesh 'indices))
         (vertices (slot-value mesh 'vertices))
         (v1 (%mesh-vertex index 0 indices vertices))
         (v2 (%mesh-vertex index 1 indices vertices))
         (v3 (%mesh-vertex index 2 indices vertices)))
    (vec-max v1 v2 v3)))

(defun generate-mesh-field (function width x-samples depth z-samples
                            &rest args &key transform
                           &allow-other-keys)
  (let* ((transform (ensure-transform transform))
         (n-triangles (* 2 (- x-samples 1) (- z-samples 1)))
         (n-vertices (* x-samples z-samples))
         (indices (make-array (list n-triangles 3) :element-type 'fixnum))
         (vertices (make-array n-vertices))
         (sx (/ (- (float x-samples) 1.0) (float width)))
         (sz (/ (- (float z-samples) 1.0) (float depth)))
         (v -1)
         (max (vec float-negative-infinity float-negative-infinity float-negative-infinity))
         (min (vec float-positive-infinity float-positive-infinity float-positive-infinity)))
    (dotimes (z z-samples)
      (let ((rz (/ z sz)))
        (dotimes (x x-samples)
          (let* ((rx (/ x sx))
                 (ry (funcall function rx rz))
                 (vertex (transform-point (vec rx ry rz) transform)))
            (setf min (vec-min min vertex)
                  max (vec-max max vertex))
            (setf (aref vertices (incf v)) vertex)))))
    (let ((n-squares-per-row (- x-samples 1))
          (n-rows (- z-samples 1))
          (n -1))
      (dotimes (i n-rows)
        (dotimes (j n-squares-per-row)
          (let* ((lower-left (+ j (* i x-samples)))
                 (lower-right (+ lower-left 1))
                 (upper-left (+ lower-left x-samples))
                 (upper-right (+ lower-right x-samples)))
            (setf (aref indices (incf n) 0) lower-left
                  (aref indices n 1) lower-right
                  (aref indices n 2) upper-right
                  (aref indices (incf n) 0) upper-right
                  (aref indices n 1) upper-left
                  (aref indices n 2) lower-left)))))
    (apply #'make-instance 'mesh
           :vertices vertices
           :indices indices
           :min min
           :max max
           :transform (identity-matrix)
           args)))

(defun build-mesh (triangles &rest initargs &key transform &allow-other-keys)
  (let ((map (make-hash-table :test #'equalp))
        (transform (ensure-transform transform))
        (indices (make-array (list (length triangles) 3) :element-type 'fixnum))
        (p 0)
        (max (vec float-negative-infinity float-negative-infinity float-negative-infinity))
        (min (vec float-positive-infinity float-positive-infinity float-positive-infinity)))
    (labels ((vertex (triangle i)
             (let* ((co (elt triangle i))
                    (vertex (vec (coerce (elt co 0) 'single-float)
                                 (coerce (elt co 1) 'single-float)
                                 (coerce (elt co 2) 'single-float))))
               (when transform
                 (setf vertex (transform-point vertex transform)))
               (setf min (vec-min min vertex)
                     max (vec-max max vertex))
               (setf (aref indices p i)
                     (or (gethash vertex map)
                         (setf (gethash vertex map) (hash-table-count map)))))))
      (sb-sequence:dosequence (tri triangles)
        (vertex tri 0)
        (vertex tri 1)
        (vertex tri 2)
        (incf p))
      (let ((vertices (make-array (hash-table-count map))))
        (maphash (lambda (vertex index)
                   (declare (type vec vertex))
                   (setf (aref vertices index) vertex))
                 map)
        (apply #'make-instance 'mesh
               :indices indices
               :vertices vertices
               :min min
               :max max
               :transform (identity-matrix)
               initargs)))))

(defconstant +mesh-epsilon+ 0.000001)

(defmethod compute-object-properties ((mesh mesh) scene transform &key shading-object)
  (let* ((kd-tree (or (mesh-kd-tree mesh)
                      (build-kd-tree mesh (slot-value mesh 'min) (slot-value mesh 'max))))
         (vertices (slot-value mesh 'vertices))
         (indices (sb-ext:array-storage-vector (slot-value mesh 'indices)))
         ;; KLUDGE!
         (normal black))
    (declare (type (simple-array fixnum (*)) indices)
             (type (simple-array vec (*)) vertices))
    (declare (optimize speed))
    (setf (mesh-kd-tree mesh) kd-tree)
    (list
     :intersection
     (macrolet ((cut (form)
                  `(logand most-positive-fixnum ,form)))
       (lambda (ray)
         (flet ((mesh-intersect (triangles start end)
                  (declare (type (simple-array fixnum (*)) triangles)
                           (type (or null single-float) start end))
                  (let* ((ext (ray-extent ray))
                         (end (if end (min end ext) ext))
                         (start (if start (max start epsilon) epsilon))
                         (best end)
                         (e1 (alloc-vec))
                         (e2 (alloc-vec)))
                    (declare (single-float ext end start best))
                    (dotimes (i (length triangles))
                      (let* ((index (aref triangles i))
                             (a (aref vertices (aref indices (cut (+ (* 3 index) 0)))))
                             (b (aref vertices (aref indices (cut (+ (* 3 index) 1)))))
                             (c (aref vertices (aref indices (cut (+ (* 3 index) 2)))))
                             (dir (ray-direction ray))
                             ;; Find edges from A
                             (edge1 (vec- b a))
                             (edge2 (vec- c a))
                             ;; Begin calculating the determinant - also used to calculate U parameter.
                             (pvec (cross-product dir edge2))
                             ;; If determinant is near zero, ray lies in plane of the triangle
                             (det (dot-product edge1 pvec)))
                        (declare (dynamic-extent edge1 edge2 pvec))
                        (when (< (- +mesh-epsilon+) det +mesh-epsilon+)
                          (go :next))
                        (let* ((inv-det (/ 1.0 det))
                               ;; Calculate distance from A to ray origin
                               (tvec (vec- (ray-origin ray) a))
                               ;; Calculate U parameter and test bounds
                               (u (* (dot-product tvec pvec) inv-det)))
                          (declare (dynamic-extent tvec))
                          (unless (<= 0.0 u 1.0)
                            (go :next))
                          (let* ( ;; Prepare to test V parameter
                                 (qvec (cross-product tvec edge1))
                                 ;; Calculate V parameter and test bounds
                                 (v (* (dot-product dir qvec) inv-det)))
                            (declare (dynamic-extent qvec))
                            (when (or (< v 0.0) (> (+ v u) 1.0))
                              (go :next))
                            ;; Calculate intersection distance
                            (let ((s (* (dot-product edge2 qvec) inv-det)))
                              (when (< start s best)
                                (%copy-vec e1 edge1)
                                (%copy-vec e2 edge2)
                                (setf best s))))))
                      :next)
                    (when (< best end)
                      (setf (ray-extent ray) best
                            normal (cross-product e2 e1))
                      t))))
           (kd-traverse #'mesh-intersect ray kd-tree))))
     :normal
     (lambda (point)
       (normalize normal)))))

(defmethod compute-object-extents ((mesh mesh) transform)
  (values (slot-value mesh 'min)
          (slot-value mesh 'max)))