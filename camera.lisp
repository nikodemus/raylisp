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

(defmethod print-object ((camera camera) stream)
  (print-unreadable-object (camera stream :type t)
    (format stream "loc: ~S dir: ~S right: ~S up: ~S"
            (location-of camera) (direction-of camera)
            (right-of camera) (up-of camera)))
  camera)

(defgeneric compile-camera (camera))

(defmethod initialize-instance :after ((camera camera) &key
				       look-at direction location
				       focal-length sky
                                       right up aspect-ratio)
  (when (and look-at direction)
    (error "Both :LOOK-AT and :DIRECTION given."))
  (when (and focal-length direction)
    (error "Both :DIRECTION and :FOCAL-LENGTH given."))
  (when (and (or up right) (not (and up right)))
    (error "Only one of :UP and :RIGHT given."))
  (when (and aspect-ratio up right)
    (error ":ASPECT-RATIO given in addition to :UP and :RIGHT."))
  (when (and sky up right)
    (error ":SKY given in addition to :UP and :RIGHT."))
  (unless location
    (setf location (vector 0.0 2.0 -10.0)))
  (let ((normalized-direction
         (when direction
           (normalize direction))))
    (unless direction
      (unless look-at
        (setf look-at origin))
      (unless focal-length
        (setf focal-length 1.0))
      (setf normalized-direction (normalize (vector-sub look-at location))
             direction (vector-mul normalized-direction focal-length)))
    (unless (and up right)
      (unless sky
        (if (= 0 (aref direction 0) (aref direction 2))
            (setf sky z-axis)
            (setf sky y-axis)))
      (unless aspect-ratio
        (setf aspect-ratio (/ 4.0 3.0)))
      (let* ((n-sky (if sky (normalize sky) y-axis))
             (n-right (cross-product n-sky normalized-direction)))
        (setf right (vector-mul n-right aspect-ratio)
              up (normalize (cross-product normalized-direction n-right))))))
  (setf (location-of camera) location
        (direction-of camera) direction
        (right-of camera) right
        (up-of camera) up))

(defun normalize-camera (camera width height)
  (let ((up (normalize (up-of camera)))
        (right (normalize (right-of camera)))
        (image-ratio (float (/ width height))))
    (make-instance (class-of camera)
                   :up up
                   :right (vector-mul right image-ratio)
                   :direction (direction-of camera)
                   :location (location-of camera))))

;;;### Pinhole Camera
;;;
;;; The most basic camera type.

(defclass pinhole (camera) ())

(defmethod compile-camera ((camera pinhole))
  (let ((dir (direction-of camera))
	(right (right-of camera))
	(up (up-of camera))
	(location (location-of camera)))
    (lambda (rx ry counters)
      (declare (type float rx ry))
      (note-camera-ray counters)
      (macrolet ((dim (n)
		   `(+ (aref dir ,n)
		       (* rx (aref right ,n)) (* ry (aref up ,n)))))
	(make-ray
	 :origin location
	 :direction (normalized-vector (dim 0) (dim 1) (dim 2)))))))


