(in-package :raylisp)

(defclass pinhole-camera (camera) ())

(defmethod compute-camera-function ((camera pinhole-camera))
  (let ((dir (direction-of camera))
	(right (right-of camera))
	(up (up-of camera))
	(location (location-of camera)))
    (declare (type vec dir right up location) (optimize speed))
    (lambda (fun rx ry counters)
      (declare (float rx ry)
               (function fun))
      (note-camera-ray counters)
      (macrolet ((dim (n)
		   `(+ (aref dir ,n)
		       (* rx (aref right ,n)) (* ry (aref up ,n)))))
        ;; FIXME: if DIR is a direct argument we lost DXness.
	(let ((dir (normalized-vec (dim 0) (dim 1) (dim 2))))
          (declare (dynamic-extent dir))
          (with-ray (ray :origin location :direction dir)
            (funcall fun ray)))))))

