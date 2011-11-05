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

(defun mesh-face-count (mesh)
  (array-dimension (slot-value mesh 'indices) 0))

(defun mesh-vertex-count (mesh)
  (length (slot-value mesh 'vertices)))

;;;; We compute a separate KD tree for each mesh. This tells the tree
;;;; code how to handle meshes.

(defmethod kd-set-size ((mesh mesh))
  (mesh-face-count mesh))

(defmethod map-kd-set (function (mesh mesh))
  (dotimes (i (kd-set-size mesh))
    (funcall function i)))

(defmethod make-kd-subset (subset (mesh mesh))
  (let ((result (make-array (length subset) :element-type '(unsigned-byte 32)))
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

(defun build-mesh-kd-tree (mesh)
  (build-kd-tree mesh
                 (slot-value mesh 'min)
                 (slot-value mesh 'max)
                 :verbose t
                 :name "mesh bounding tree"
                 :type "faces"))

(defconstant +mesh-epsilon+ 0.000001)

(defmethod compute-object-properties ((model model) scene transform &key shading-object)
  (assert (not shading-object))
  (let* ((mesh (model-mesh model))
         (inverse (inverse-matrix transform))
         (kd-tree (or (mesh-kd-tree mesh)
                      (setf (mesh-kd-tree mesh) (build-mesh-kd-tree mesh))))
         (vertices (slot-value mesh 'vertices))
         (indices (sb-ext:array-storage-vector (slot-value mesh 'indices)))
         ;; KLUDGE -- we don't have a better way to pass normals around right now.
         (normal +x+))
    (declare (type (simple-array (unsigned-byte 32) (*)) indices)
             (type (simple-array vec (*)) vertices))
    (declare (optimize speed))
    (list
     :intersection
     (macrolet ((cut (form)
                  `(logand most-positive-fixnum ,form)))
       (lambda (ray)
         (with-transformed-ray (local ray inverse)
           (flet ((mesh-intersect (triangles start end)
                    (declare (type (simple-array (unsigned-byte 32) (*)) triangles)
                             (type (or null single-float) start end))
                    (let* ((ext (ray-extent local))
                           (end (if end (min (+ end +epsilon+) ext) ext))
                           (start (if start (max (- start +epsilon+) +epsilon+) +epsilon+))
                           (best end)
                           (e1 (alloc-vec))
                           (e2 (alloc-vec)))
                      (declare (single-float ext end start best))
                      (dotimes (i (length triangles))
                        (let* ((index (aref triangles i))
                               (a (aref vertices (aref indices (cut (+ (* 3 index) 0)))))
                               (b (aref vertices (aref indices (cut (+ (* 3 index) 1)))))
                               (c (aref vertices (aref indices (cut (+ (* 3 index) 2)))))
                               (dir (ray-direction local))
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
                                 (tvec (vec- (ray-origin local) a))
                                 ;; Calculate U parameter and test bounds
                                 (u (* (dot-product tvec pvec) inv-det)))
                            (declare (dynamic-extent tvec))
                            (unless (<= (- +epsilon+) u (+ 1.0 +epsilon+))
                              (go :next))
                            (let* ( ;; Prepare to test V parameter
                                   (qvec (cross-product tvec edge1))
                                   ;; Calculate V parameter and test bounds
                                   (v (* (dot-product dir qvec) inv-det)))
                              (declare (dynamic-extent qvec))
                              (when (or (< v (- +epsilon+)) (> (+ v u) (+ +epsilon+ 1.0)))
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
                              normal (transform-direction (cross-product e2 e1) transform))
                        t))))
             (kd-traverse #'mesh-intersect local kd-tree)))))
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
                            &rest args &key rotate scale translate matrix matrix-list)
  (declare (ignore rotate scale translate matrix matrix-list))
  (let* ((transform (parse-transform-arguments args))
         (n-triangles (* 2 (- x-samples 1) (- z-samples 1)))
         (n-vertices (* x-samples z-samples))
         (indices (make-array (list n-triangles 3) :element-type '(unsigned-byte 32)))
         (vertices (make-array n-vertices))
         (sx (/ (- (float x-samples) 1.0) (float width)))
         (sz (/ (- (float z-samples) 1.0) (float depth)))
         (v -1)
         (min (positive-infinity-vec))
         (max (negative-infinity-vec)))
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
    (let ((mesh (make-instance 'mesh
                               :vertices vertices
                               :indices indices
                               :min min
                               :max max)))
      (setf (mesh-kd-tree mesh) (build-mesh-kd-tree mesh))
      mesh)))

;;;; LOADING MESHES

(defvar *mesh-loaders* (make-hash-table))

(defmacro define-mesh-loader (format function-name)
  (check-type format keyword)
  `(setf (gethash ,format *mesh-loaders*) ',function-name))

(defun find-mesh-loader (format &optional (errorp t))
  (or (gethash format *mesh-loaders*)
      (when errorp
        (error "Unknown mesh format: ~S" format))))

(defun load-mesh (pathname &rest initargs &key format scale rotate translate matrix matrix-list)
  (declare (ignore scale rotate translate matrix matrix-list))
  (let* ((type (pathname-type pathname))
         (mesh-format (if (stringp type)
                          (or format (intern (string-upcase type) :keyword))
                          (or format (error "Filetype not apparent, please specify :FORMAT"))))
         (matrix (parse-transform-arguments initargs)))
    (if (eq :mesh mesh-format)
        (load-builtin-mesh pathname matrix)
        (multiple-value-bind (vertices faces)
            (funcall (find-mesh-loader mesh-format) pathname)
          (let ((mesh (build-mesh vertices faces matrix)))
            (if (matrix= matrix +identity-matrix+)
                ;; If we did not transform on load, we can reuse an old
                ;; KD-tree -- if we have one. If not, build and save one.
                (let ((kd-file (make-pathname :type "kd" :defaults pathname)))
                  (cond ((probe-file kd-file)
                         (setf (mesh-kd-tree mesh)
                               (load-kd-tree kd-file)))
                        (t
                         (let ((tree (build-mesh-kd-tree mesh)))
                           (setf (mesh-kd-tree mesh) tree)
                           (save-kd-tree tree kd-file)))))
                (let ((tree (build-mesh-kd-tree mesh)))
                  (setf (mesh-kd-tree mesh) tree)))
            mesh)))))

(defun build-mesh (vertices faces matrix)
  (declare (simple-vector vertices faces))
  (let ((map (make-hash-table :test #'equalp))
        (indices (make-array (list (length faces) 3) :element-type '(unsigned-byte 32)))
        (p 0)
         (min (positive-infinity-vec))
         (max (negative-infinity-vec)))
    (labels ((vertex (face i)
               (let ((vertex (transform-point (aref vertices (elt face i)) matrix)))
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

;;;; BUILT-IN MESH SERIALIZATION
;;;;
;;;; Binary format:
;;;;
;;;; #x4D455348 (magic bytes, ascii codes for MESH)
;;;; ub32 (format version, currently zero)
;;;; ub32 (number of vertices)
;;;; ub32 (number of indices)
;;;; single,single,single (vertex)
;;;; ...repeats for specified number of times
;;;; ub32,ub32,ub32 (face triplet)
;;;; ...repeats for specifid number of times

(defconstant +mesh-magic-bytes+ #x4d455348)
(defconstant +mesh-format-version+ 0)

(defun save-mesh (mesh pathname &key (if-exists :error))
  (with-open-file (f pathname
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists if-exists)
    (let* ((vertices (mesh-vertices mesh))
           (indices (mesh-indices mesh))
           (vertex-count (length vertices))
           (face-count (mesh-face-count mesh)))
      (write-word +mesh-magic-bytes+ f)
      (write-word +mesh-format-version+ f)
      (write-word vertex-count f)
      (write-word face-count f)
      (dotimes (i vertex-count)
        (let ((vertex (aref vertices i)))
          (dotimes (j 3)
            (write-single (aref vertex j) f))))
      (dotimes (i face-count)
        (dotimes (j 3)
          (write-word (aref indices i j) f))))
    (write-kd-tree (mesh-kd-tree mesh) f))
  mesh)

(defun load-builtin-mesh (pathname matrix)
  (let ((pathname (merge-pathnames pathname (make-pathname :type "mesh")))
        (transformp (not (matrix= matrix +identity-matrix+))))
    (with-open-file (f pathname
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist :error)
      (unless (= +mesh-magic-bytes+ (read-word f))
        (error "~A is not a Raylisp mesh file" pathname))
      (let ((version (read-word f)))
        (unless (= +mesh-format-version+ version)
          (error "Unknown Raylisp mesh format: ~A" version)))
      (let* ((vertex-count (read-word f))
             (face-count (read-word f))
             (vertices (make-array vertex-count))
             (faces (make-array (list face-count 3) :element-type '(unsigned-byte 32)))
             (min (positive-infinity-vec))
             (max (negative-infinity-vec)))
        (if transformp
            (dotimes (i vertex-count)
              (let ((vertex (transform-point
                             (vec (read-single f) (read-single f) (read-single f))
                             matrix)))
                (%vec-min min min vertex)
                (%vec-max max max vertex)
                (setf (aref vertices i) vertex)))
            (dotimes (i vertex-count)
              (let ((vertex (vec (read-single f) (read-single f) (read-single f))))
                (%vec-min min min vertex)
                (%vec-max max max vertex)
                (setf (aref vertices i) vertex))))
        (dotimes (i face-count)
          (dotimes (j 3)
            (setf (aref faces i j) (read-word f))))
        (let ((mesh (make-instance 'mesh
                                   :min min
                                   :max max
                                   :vertices vertices
                                   :indices faces)))
          (setf (mesh-kd-tree mesh)
                (if transformp
                    (build-mesh-kd-tree mesh)
                    (read-kd-tree f)))
          mesh)))))

(defun convert-mesh (source target &rest keys &key translate scale rotate matrix matrix-list)
  (declare (ignore translate scale rotate matrix matrix-list))
  (save-mesh (apply #'load-mesh source keys) target))