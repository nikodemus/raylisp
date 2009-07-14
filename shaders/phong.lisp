;;; Provides phong highlights, diffuse and ambient components.

(in-package :raylisp)

(defclass phong-shader (shader color-mixin ambient-shader-mixin diffuse-shader-mixin specular-shader-mixin)
  ((size :initform (find-default :size 'float) :initarg :size
	 :accessor size-of)))

(defmethod compute-shader-function ((shader phong-shader) object scene transform)
  (let* ((ambient-term (vec* (scene-ambient-light scene) (ambient-of shader)))
         (color-function
          (compute-color-function (color-of shader) transform))
         (diffuse (diffuse-of shader))
	 (specular (specular-of shader))
	 (size (size-of shader)))
    (declare (type single-float specular diffuse)
             (type (single-float (0.0)) size)
             (type vec ambient-term)
             (type function color-function))
    (sb-int:named-lambda shade-phong (point normal n.d ray counters)
      (declare (optimize speed)
               (ignore n.d))
      (let ((result (copy-vec ambient-term))
            (local-color (funcall color-function point))
            (dir (ray-direction ray)))
        (declare (type vec result local-color dir))
        ;; FIXME: too much indirection at runtime: make SCENE-COMPILED-SCENE
        ;; available at scene compilation time.
        (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
          (let* ((lv (light-vector light point))
                 (dot (dot-product lv normal)))
            (declare (single-float dot))
            (when (plusp dot)
              (multiple-value-bind (incident len) (illuminate light point lv counters)
                (when (plusp len)
                  (let* ((l.n (/ dot len))
                         (h (normalize (vec- lv dir)))
                         (n.h^p (expt (the (single-float 0.0) (dot-product normal h)) size))
                         (s-co (* specular n.h^p)))
                    (macrolet
                        ((dim (n)
                           `(incf (aref result ,n)
                                  (* (aref incident ,n)
                                     (+ (* (aref local-color ,n) diffuse l.n) s-co)))))
                      (dim 0)
                      (dim 1)
                      (dim 2))))))))
        result))))
