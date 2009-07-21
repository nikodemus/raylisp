(in-package :raylisp)

(defclass triangle (scene-object)
  ((vertices
    :initform (required-argument :vertices)
    :initarg :vertices
    :reader vertices-of)))

;;; Intersection method from "Fast, minimum storage ray-triangle intersection."
;;; by Tomas Möller and Ben Trumbore. Journal of Graphics Tools, 2(1):21--28, 1997.
;;;
;;; Currently this is not really as smart as it should be, since we waste space
;;; closing over the normal. Perhaps this points to a lack of flexibility in
;;; system: for a triangle mesh we could like to really call a single non-closed
;;; over normal function always, giving it the edge vectors computed by the
;;; intersection...

(defconstant +triangle-epsilon+ 0.000001)

(defmethod compute-object-properties ((triangle triangle) scene transform &key shading-object)
  (let* ((vertices (vertices-of triangle))
         (m transform)
         (a (transform-point (elt vertices 0) m))
         (b (transform-point (elt vertices 1) m))
         (c (transform-point (elt vertices 2) m)))
    (list
    :intersection
    (unless shading-object
      (sb-int:named-lambda triangle-intersection (ray)
        (block triangle-intersection
          (let* ((dir (ray-direction ray))
                 ;; Find edges from A
                 (edge1 (vec- b a))
                 (edge2 (vec- c a))
                 ;; Begin calculating the determinant - also used to calculate U parameter.
                 (pvec (cross-product dir edge2))
                 ;; If determinant is near zero, ray lies in plane of the triangle
                 (det (dot-product edge1 pvec)))
            (when (< (- +triangle-epsilon+) det +triangle-epsilon+)
              (return-from triangle-intersection nil))
            (let* ((inv-det (/ 1.0 det))
                   ;; Calculate distance from A to ray origin
                   (tvec (vec- (ray-origin ray) a))
                   ;; Calculate U parameter and test bounds
                   (u (* (dot-product tvec pvec) inv-det)))
              (unless (<= (- epsilon) u (+ 1.0 epsilon))
                (return-from triangle-intersection nil))
              (let* ( ;; Prepare to test V parameter
                     (qvec (cross-product tvec edge1))
                     ;; Calculate V parameter and test bounds
                     (v (* (dot-product dir qvec) inv-det)))
                (when (or (< v (- epsilon)) (> (+ v u) (+ 1.0 epsilon)))
                  (return-from triangle-intersection nil))
                ;; Calculate intersection distance
                (let ((s (* (dot-product edge2 qvec) inv-det)))
                  (when (< epsilon s (ray-extent ray))
                    (setf (ray-extent ray) s)
                    t))))))))
    :normal
    (constantly (normalize (cross-product (vec- c a) (vec- b a)))))))

(defmethod compute-object-extents ((triangle triangle) transform)
  (let* ((vertices (vertices-of triangle))
         (m transform)
         (a (transform-point (elt vertices 0) m))
         (b (transform-point (elt vertices 1) m))
         (c (transform-point (elt vertices 2) m)))
    (values (vec-min a b c) (vec-max a b c))))

;;;; To have something to test with we define a partial .obj reader.

(defun load-obj (pathname &rest initargs)
  (let (triangles vertices)
    (block snarf
      (with-open-file (f pathname)
        (loop
          (ecase (read-char f nil :eof)
            ((#\# #\newline #\g)
             (read-line f))
            (#\v
             (push (vec (read f) (read f) (read f)) vertices))
            (#\f
             (push (simple-vector (1- (read f)) (1- (read f)) (1- (read f))) triangles))
            (:eof
             (return-from snarf))))))
    (let ((vertex-vector (coerce (nreverse vertices) 'simple-vector)))
      (loop for triangle in triangles
            collect (apply #'make-instance 'triangle
                           :vertices (simple-vector (aref vertex-vector (aref triangle 0))
                                                    (aref vertex-vector (aref triangle 1))
                                                    (aref vertex-vector (aref triangle 2)))
                           initargs)))))

