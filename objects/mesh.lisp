;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :raylisp)

;;;; MODEL is the actual scene object.
(defclass model (scene-object)
  ((mesh
    :initarg :mesh
    :initform (required-argument :mesh)
    :reader model-mesh)))

;;;; We store geometry in a MESH object, which can be shared between multiple
;;;; instances of MODEL -- not rendered directly.
(defclass mesh ()
  ((indices
    :initform (required-argument :indices)
    :initarg :indices
    :reader mesh-indices)
   (vertices
    :initform (required-argument :vertices)
    :initarg :vertices
    :reader mesh-vertices)
   (min
    :initform (required-argument :min)
    :initarg :min)
   (max
    :initform (required-argument :max)
    :initarg :max)
   (kd-tree
    :initform nil
    :accessor mesh-kd-tree)))

(defmethod print-object ((mesh mesh) stream)
  (print-unreadable-object (mesh stream :type t)
    (format stream "~S triangles, ~S vertices"
            (array-dimension (mesh-indices mesh) 0)
            (length (mesh-vertices mesh)))))

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

(defconstant +mesh-epsilon+ 0.000001)

(defmethod compute-object-properties ((model model) scene transform &key shading-object)
  (assert (not shading-object))
  (assert (matrix= (identity-matrix) transform))
  (let* ((mesh (model-mesh model))
         (kd-tree (or (mesh-kd-tree mesh)
                      (setf (mesh-kd-tree mesh)
                            (build-kd-tree mesh (slot-value mesh 'min) (slot-value mesh 'max)
                                           :verbose t
                                           :name "mesh bounding tree"
                                           :type "triangles"))))
         (vertices (slot-value mesh 'vertices))
         (indices (sb-ext:array-storage-vector (slot-value mesh 'indices)))
         ;; KLUDGE -- we don't have a better way to pass normals around right now.
         (normal black))
    (declare (type (simple-array fixnum (*)) indices)
             (type (simple-array vec (*)) vertices))
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
                         (end (if end (min (+ end epsilon) ext) ext))
                         (start (if start (max (- start epsilon) epsilon) epsilon))
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
                          (unless (<= (- epsilon) u (+ 1.0 epsilon))
                            (go :next))
                          (let* ( ;; Prepare to test V parameter
                                 (qvec (cross-product tvec edge1))
                                 ;; Calculate V parameter and test bounds
                                 (v (* (dot-product dir qvec) inv-det)))
                            (declare (dynamic-extent qvec))
                            (when (or (< v (- epsilon)) (> (+ v u) (+ epsilon 1.0)))
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
       (declare (ignore point))
       (normalize normal)))))

(defmethod compute-object-extents ((model model) transform)
  (let ((mesh (model-mesh model)))
    (transform-bounds (slot-value mesh 'min) (slot-value mesh 'max)
                      transform)))

;;;; GENERATING MESHES

(defun generate-mesh-field (function width x-samples depth z-samples
                            &key transform)
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
    (make-instance 'mesh
                   :vertices vertices
                   :indices indices
                   :min min
                   :max max)))

;;;; LOADING MESHES

(defvar *mesh-loaders* (make-hash-table))

(defmacro define-mesh-loader (format function-name)
  (check-type format keyword)
  `(setf (gethash ,format *mesh-loaders*) ',function-name))

(defun find-mesh-loader (format &optional (errorp t))
  (or (gethash format *mesh-loaders*)
      (when errorp
        (error "Unknown mesh format: ~S" format))))

(defun load-mesh (pathname &key format transform)
  (let* ((type (pathname-type pathname))
         (mesh-format (if (stringp type)
                          (or format (intern (string-upcase type) :keyword))
                          (or format (error "Filetype not apparent, please specify :FORMAT")))))
    (funcall (find-mesh-loader mesh-format) pathname transform)))

(defun build-mesh (vertices faces &key transform)
  (declare (simple-vector vertices faces))
  (let ((map (make-hash-table :test #'equalp))
        (transform (ensure-transform transform))
        (indices (make-array (list (length faces) 3) :element-type 'fixnum))
        (p 0)
        (max (vec float-negative-infinity float-negative-infinity float-negative-infinity))
        (min (vec float-positive-infinity float-positive-infinity float-positive-infinity)))
    (labels ((vertex (face i)
               (let ((vertex (transform-point (aref vertices (elt face i)) transform)))
                 (%vec-min min min vertex)
                 (%vec-max max max vertex)
                 (setf (aref indices p i)
                       (or (gethash vertex map)
                           (setf (gethash vertex map) (hash-table-count map)))))))
      (dotimes (i (length faces))
        (let ((face (aref faces i)))
          (vertex face 0)
          (vertex face 1)
          (vertex face 2))
        (incf p))
      (let ((merged-vertices (make-array (hash-table-count map))))
        (maphash (lambda (vertex index)
                   (setf (aref merged-vertices index) vertex))
                 map)
        (make-instance 'mesh
                       :indices indices
                       :vertices merged-vertices
                       :min min
                       :max max)))))
