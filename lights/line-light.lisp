(in-package :raylisp)

(defclass line-light (scene-light color-mixin location-mixin axis-mixin)
  ((samples
    :initform 8
    :initarg :samples
    :reader samples-of)
   (length
    :initform 5.0
    :initarg :length
    :reader length-of)))

(defmethod compute-light-properties ((light line-light) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
	 (shadow-fun (shadow-function location scene))
         (samples (ceiling (samples-of light) 2))
         (step (/ (length-of light) samples))
         (power (/ 1.0 samples))
         (axis (axis-of light)))
    (declare (function shadow-fun)
             (fixnum samples)
             (single-float step power)
             (vec axis))
    (list
     :incident-light
     (lambda (point)
       (vec- location point))
     :illumination
     (lambda (point light-vector counters)
       (declare (optimize speed))
       (let ((p 0.0))
         (dotimes (i samples)
           (let* ((lv (vec+ light-vector (vec* axis (+ (* i step) (random step)))))
                  (len (vec-length lv)))
             (declare (dynamic-extent lv))
             (%vec/ lv lv len)
             (unless (funcall shadow-fun point lv len counters)
               (incf p power))))
         (if (plusp p)
             (values (vec* color p) (vec-length light-vector))
             (values black -1.0)))))))