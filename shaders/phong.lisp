;;; Provides phong highlights, diffuse and ambient components.

(in-package :raylisp)

(defclass phong-shader (shader color-mixin ambient-shader-mixin diffuse-shader-mixin specular-shader-mixin)
  ((size :initform (find-default :size 'float) :initarg :size
	 :accessor size-of)))

(defmethod compute-shader-function ((shader phong-shader) object scene transform)
  (let* ((color (color-of shader))
	 (ambient-color (hadamard-product
                         (vec* (scene-ambient-light scene)
                               (coefficient (ambient-of shader) shader))
			 color))
	 (diffuse-color (vec* color (coefficient (diffuse-of shader) shader)))
	 (specular (coefficient (specular-of shader) shader))
	 (size (size-of shader)))
    (declare (type float specular)
             (type (single-float (0.0)) size)
             (type vec color ambient-color diffuse-color))
    (with-arrays (diffuse-color)
      ;; FIXME: dot ignored?
      (sb-int:named-lambda shade-phong (obj point normal dot ray counters)
        (declare (optimize speed)
                 (ignore obj))
	(let ((color black)
	      (dir (ray-direction ray)))
	  (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
	    (let* ((lv (light-vector light point))
		   (dot (dot-product lv normal)))
              (declare (single-float dot))
	      (when (plusp dot)
		(multiple-value-bind (incident len) (illuminate light point lv counters)
		  (when (plusp len)
		    (let* ((l.n (/ dot len))
			   (h (normalize (vec- lv dir))) ; FIXME: why must we normalize?
			   (n.h^p (expt (the (single-float 0.0) (dot-product normal h)) size))
			   (s-co (* specular n.h^p)))
		      (with-arrays (incident color)
			(macrolet
			    ((dim (n)
			       `(+ (color ,n)
				   (* (incident ,n) (+ (* (diffuse-color ,n) l.n)
						       s-co)))))
			  (setf color (vec (dim 0) (dim 1) (dim 2)))))))))))
	  (vec+ ambient-color color))))))
