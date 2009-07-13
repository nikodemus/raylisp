(in-package :raylisp)

(defclass spotlight (scene-light color-mixin location-mixin direction-mixin)
  ((aperture :initform 0.9 :initarg :aperture :accessor aperture-of))
  (:documentation
   "A simple spotlight. The smaller the aperture the wider the spot, 1.0
begin the maximum value. Spotlight fades towards its edges."))

(defun linear-fader (aperture)
  (declare (type (float -1.0 1.0) aperture))
  (let ((scale (/ 1.0 (- 1.0 aperture))))
    (lambda (color f)
      (declare (type vec color)
               (type float f))
      (vec* color (* (- f aperture) scale)))))

(defmethod compute-light-properties ((light spotlight) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
         (direction (normalize (direction-of light)))
	 (shadow-fun (shadow-function location scene))
         (aperture (coerce (aperture-of light) 'float))
         (fader (linear-fader aperture)))
    (declare (type float aperture)
             (type function fader shadow-fun)
             (type vec direction location color))
    (list
     :incident-light
     (lambda (point)
       (vec- location point))
     :illumination
     (lambda (point light-vector counters)
       (declare (optimize speed))
       (let* ((len (vec-length light-vector))
              (nlv (vec/ light-vector len))
              (dot (- (dot-product nlv direction))))
         (if (or (< dot aperture) (funcall shadow-fun point nlv len counters))
             (values black -1.0)
             (values (funcall fader color dot) len)))))))
