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
    :initarg :max)))

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

(defmethod compute-object-properties ((mesh mesh) scene transform &key shading-object)
  (let* ((kd-tree (build-kd-tree mesh (slot-value mesh 'min) (slot-value mesh 'max)))
         (vertices (slot-value mesh 'vertices))
         (indices (sb-ext:array-storage-vector (slot-value mesh 'indices)))
         ;; KLUDGE!
         (normal black))
    (declare (type (simple-array fixnum (*)) indices)
             (type (simple-array t (*)) vertices))
    (declare (optimize speed))
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
                         (e1 nil)
                         (e2 nil))
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
                        (declare (dynamic-extent edge1 edge2))
                        (when (< (- epsilon) det epsilon)
                          (go :next))
                        (let* ((inv-det (/ 1.0 det))
                               ;; Calculate distance from A to ray origin
                               (tvec (vec- (ray-origin ray) a))
                               ;; Calculate U parameter and test bounds
                               (u (* (dot-product tvec pvec) inv-det)))
                          (unless (<= 0.0 u 1.0)
                            (go :next))
                          (let* ( ;; Prepare to test V parameter
                                 (qvec (cross-product tvec edge1))
                                 ;; Calculate V parameter and test bounds
                                 (v (* (dot-product dir qvec) inv-det)))
                            (when (or (< v 0.0) (> (+ v u) 1.0))
                              (go :next))
                            ;; Calculate intersection distance
                            (let ((s (* (dot-product edge2 qvec) inv-det)))
                              (when (< start s best)
                                (setf best s
                                      e1 (copy-vec edge1)
                                      e2 (copy-vec edge2)))))))
                      :next)
                    (when e1
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