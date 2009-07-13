;;; Provides diffuse and ambient components.

(in-package :raylisp)

(defclass solid-shader (shader color-mixin ambient-shader-mixin diffuse-shader-mixin)
  ())

(defmethod compute-shader-function ((shader solid-shader) object scene transform)
  (let* ((color (color-of shader))
         (ambient-color (hadamard-product
                         (vec* (scene-ambient-light scene)
                               (coefficient (ambient-of shader) shader))
                         color))
         (diffuse-color (vec* color
                              (coefficient (diffuse-of shader) shader))))
    (declare (type vec color ambient-color diffuse-color))
    (with-arrays (diffuse-color)
      ;; DOT as argument is ignores -- is this correct?
      (sb-int:named-lambda shade-solid (obj point normal dot ray counters)
        (declare (optimize speed))
        (declare (ignore obj ray))
	(let ((color ambient-color))
          ;; FIXME: Is there a way to store the list of lights directly here,
          ;; without going though 2 indirections each time?
	  (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
	    (let* ((lv (light-vector light point))
		   (dot (dot-product lv normal)))
	      (when (plusp dot)
		(multiple-value-bind (incident len) (illuminate light point lv counters)
		  (when (plusp len)
                    ;; nx * lx/len + ny * ly/len + nz * lz/len
                    ;; == (nx*lx + ny*ly + nz*lz)/len
		    (let ((l.n (/ dot len)))
		      (with-arrays (incident color)
			(macrolet
			    ((dim (n)
			       `(+ (color ,n)
				   (* (incident ,n)
                                      (* (diffuse-color ,n) l.n)))))
			  (setf color (vec (dim 0) (dim 1) (dim 2)))))))))))
          color)))))
