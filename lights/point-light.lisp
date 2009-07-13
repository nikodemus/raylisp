(in-package :raylisp)

(defclass point-light (scene-light color-mixin location-mixin)
  ()
  (:documentation
   "Basic omnidirectional light source."))

(defmethod compute-light-properties ((light point-light) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
	 (shadow-fun (shadow-function location scene)))
    (declare (function shadow-fun))
    (list
     :incident-light
     (lambda (point)
       (vec- location point))
     :illumination
     (lambda (point light-vector counters)
       (declare (optimize speed))
       (let* ((len (vec-length light-vector))
	      (nlv (vec/ light-vector len)))
	 (if (funcall shadow-fun point nlv len counters)
	     (values black -1.0)
	     (values color len)))))))

