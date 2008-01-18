(in-package :raylisp)

;;;## Camera
;;;
;;; CAMERA class is transformed to a corresponding raycasting lambda by
;;; COMPILE-CAMERA.

(defclass camera () 
  ((location :accessor location-of)
   (direction :accessor direction-of)
   (up :accessor up-of)
   (right :accessor right-of)))

(defgeneric compile-camera (camera))

(defmethod initialize-instance :after ((camera camera) &key 
				       (look-at z-axis look-at-p)
				       (direction z-axis direction-p)
				       (location origin)
				       (focal-length 1.0)
				       (sky y-axis)
				       (right (/ 4.0 3.0))
				       (up 1.0))
  (when (and look-at-p direction-p)
    (error "Both :LOOK-AT and :DIRECTION given."))
  (let* ((dir-v (normalize (if look-at-p
			       (vector-sub look-at location)
			       direction)))
	 (sky-v (normalize sky))
	 (right-v (cross-product sky-v dir-v))
	 (up-v (cross-product right-v dir-v)))
    (setf (location-of camera) location
	  (direction-of camera) (vector-mul dir-v focal-length)
	  (right-of camera) (vector-mul right-v right)
	  (up-of camera) (vector-mul up-v up))))

;;;### Pinhole Camera
;;;
;;; The most basic camera type.

(defclass pinhole (camera) ())

(defmethod compile-camera ((camera pinhole))
  (let ((dir (direction-of camera))
	(right (right-of camera))
	(up (up-of camera))
	(location (location-of camera)))
    (lambda (rx ry)
      (declare (type float rx ry))
      (note-camera-ray)
      (macrolet ((dim (n)
		   `(+ (aref dir ,n) 
		       (* rx (aref right ,n)) (* ry (aref up ,n)))))
	(make-ray 
	 :origin location
	 :direction (normalized-vector (dim 0) (dim 1) (dim 2)))))))

