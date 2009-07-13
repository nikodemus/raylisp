(in-package :raylisp)

(defclass sphere (scene-object)
  ((radius
    :initform 1.0 :initarg :radius :reader radius-of)
   (location
    :initform +origin+ :initarg :location
    :reader location-of)))

(defun sphere-matrix (sphere)
  (let ((r (radius-of sphere)))
    (matrix* (transform-of sphere)
              (translate (location-of sphere))
              (scale* r r r))))

(defmethod compute-object-properties ((sphere sphere) scene transform)
  (multiple-value-bind (inverse adjunct/inverse)
      (inverse-and-adjunct/inverse-matrix (matrix* transform (sphere-matrix sphere)))
    (list
     :intersection
     (sb-int:named-lambda sphere-intersection (ray)
       (declare (optimize speed))
       (let* ((o2 (transform-point (ray-origin ray) inverse))
              (d2 (transform-direction (ray-direction ray) inverse)))
         (declare (dynamic-extent o2 d2))
         (let ((s (min-pos-quad-root (+ (dot-product d2 d2))
                                     (* 2.0 (dot-product d2 o2))
                                     (- (dot-product o2 o2) 1.0))))
           ;; FIXME: If we need to follow this pattern elsewhere, then there is no
           ;; need to check against epsilon in MIN-POS-QUAD-ROOT?
           (when (< epsilon s (ray-extent ray))
             (setf (ray-extent ray) s)
             t))))
     :normal
     (lambda (point)
       (normalize (transform-point point adjunct/inverse))))))

(defmethod compute-object-extents ((sphere sphere) transform)
  (transform-extents (vec -1.0 -1.0 -1.0)
                     (vec 1.0 1.0 1.0)
                     (matrix* transform (sphere-matrix sphere))))

(defmethod compute-csg-properties ((sphere sphere) scene transform)
  (let* ((inverse (inverse-matrix (matrix* transform (sphere-matrix sphere))))
	 (compiled (compile-scene-object sphere scene transform)))
    (list
     ;; FIXME: To reduce consing even further: stack allocate
     ;; the csg-interactions and pass a continuation in here.
     ;; ...alternatively, pre-allocate a csg-intersection buffer.
     :all-intersections
     (sb-int:named-lambda sphere-all-intersections (origin direction)
       (declare (optimize speed))
       (let ((o (transform-point origin inverse))
             (d (transform-direction direction inverse)))
         (declare (dynamic-extent o d))
         (multiple-value-bind (r1 r2)
             (pos-quad-roots (dot-product d d)
                             (* 2.0 (dot-product d o))
                             (- (dot-product o o) 1.0))
           (cond ((= -1.0 r1)
                  #())
                 ((= -1.0 r2)
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)))
                 (t
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)
                                 (make-csg-intersection :distance r2 :object compiled)))))))
     :inside
     (lambda (point)
       (> (+ 1.0 epsilon)
          (let ((p (transform-point point inverse)))
            (declare (dynamic-extent p))
            (vec-length p)))))))
