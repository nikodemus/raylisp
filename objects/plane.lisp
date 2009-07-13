(in-package :raylisp)

(defclass plane (scene-object)
  ((normal :initform y-axis :initarg :normal :accessor normal-of)
   (location :initform +origin+ :initarg :location :accessor location-of)))

(defun plane-matrix (plane)
  (matrix* (transform-of plane)
            (translate (location-of plane))
            (reorient y-axis (normal-of plane))))

(defmethod compute-object-properties ((plane plane) scene transform)
  (multiple-value-bind (inverse adjunct)
      (inverse-and-adjunct-matrix (matrix* transform (plane-matrix plane)))
    (list
     :intersection
     (sb-int:named-lambda plane-intersection (ray)
       (let* ((d (transform-direction (ray-direction ray) inverse))
              (dy (aref d 1))
	      (s (if (zerop dy)
		     -1.0 ; parallel
		     (let ((o (transform-point (ray-origin ray) inverse)))
                       (declare (dynamic-extent o))
                       (- (/ (aref o 1) dy))))))
         (declare (dynamic-extent d))
	 (if (< epsilon s (ray-extent ray))
             (progn
               (setf (ray-extent ray) s)
               t)
             (progn
               nil))))
     :normal
     (constantly (normalize (transform-point y-axis adjunct))))))

(defmethod compute-csg-properties ((plane plane) scene transform)
  (let ((inverse (inverse-matrix (matrix* transform (plane-matrix plane))))
	(c-object (compile-scene-object plane scene transform)))
    (list
     :all-intersections
     (sb-int:named-lambda plane-all-intersections (origin direction)
       (let ((d (let* ((d (transform-direction direction inverse))
                       (dy (aref d 1)))
                  (declare (dynamic-extent d))
		  (if (zerop dy)
		      -1.0 ; parallel
		      (let ((o (transform-point origin inverse)))
                        (declare (dynamic-extent o))
                        (- (/ (aref o 1) dy)))))))
	 (if (significantp d)
	     (simple-vector
	      (make-csg-intersection
	       :distance d
	       :object c-object))
	     #())))
     :inside
     (lambda (point)
       (> epsilon
          (let ((p (transform-point point inverse)))
            (declare (dynamic-extent p))
            (aref p 1)))))))
