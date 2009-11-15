(in-package :raylisp)

(defclass pinhole-camera (camera) ())

(defmethod normalize-camera ((camera pinhole-camera) width height)
  (let ((ratio (coerce (/ width height) 'single-float))
        (new (call-next-method)))
    (setf (slot-value new 'right) (vec* (right-of new) ratio))
    new))

(defmethod compute-camera-function ((camera pinhole-camera))
  (let ((location (location-of camera))
        (dir (direction-of camera))
	(right (right-of camera))
	(up (up-of camera)))
    (pinhole-function location dir right up)))

(defun pinhole-function (location dir right up)
  (declare (type vec location dir right up)
           (optimize speed))
  (lambda (fun rx ry counters)
    (declare (single-float rx ry)
             (function fun)
             (optimize speed))
    (note-camera-ray counters)
    (macrolet ((dim (n)
                 `(+ (aref dir ,n)
                     (* rx (aref right ,n)) (* ry (aref up ,n)))))
      (with-ray (ray :origin location
                     :direction (normalize (vec (dim 0) (dim 1) (dim 2))))
        (funcall fun ray)))))


