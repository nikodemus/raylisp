(in-package :raylisp)

(defclass triangle (scene-object)
  ((vertices
    :initform (required-argument :vertices)
    :initarg :vertices
    :reader vertices-of)))

(defmethod compute-object-properties ((triangle triangle) scene transform &key shade-only)
  (let* ((vertices (vertices-of triangle))
         (m (matrix* transform (transform-of triangle)))
         (a (transform-point (elt vertices 0) m))
         (b (transform-point (elt vertices 1) m))
         (c (transform-point (elt vertices 2) m)))
    (list
    :intersection
    (unless shade-only
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
            (when (< (- epsilon) det epsilon)
              (return-from triangle-intersection nil))
            (let* ((inv-det (/ 1.0 det))
                   ;; Calculate distance from A to ray origin
                   (tvec (vec- (ray-origin ray) a))
                   ;; Calculate U parameter and test bounds
                   (u (* (dot-product tvec pvec) inv-det)))
              (unless (<= 0.0 u 1.0)
                (return-from triangle-intersection nil))
              (let* ( ;; Prepare to test V parameter
                     (qvec (cross-product tvec edge1))
                     ;; Calculate V parameter and test bounds
                     (v (* (dot-product dir qvec) inv-det)))
                (when (or (< v 0.0) (> (+ v u) 1.0))
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
         (m (matrix* transform (transform-of triangle)))
         (a (transform-point (elt vertices 0) m))
         (b (transform-point (elt vertices 1) m))
         (c (transform-point (elt vertices 2) m)))
    (values (vec-min a b c) (vec-max a b c))))