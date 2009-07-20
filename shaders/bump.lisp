;;; Provides diffuse and ambient components.

(in-package :raylisp)

(defclass bump-shader (shader color-mixin ambient-shader-mixin diffuse-shader-mixin)
  ())

(defmethod compute-shader-function ((shader bump-shader) object scene transform)
  (let* ((color (color-of shader))
         (ambient-color (hadamard-product
                         (vec* (scene-ambient-light scene)
                               (coefficient (ambient-of shader) shader))
                         color))
         (diffuse-color (vec* color
                              (coefficient (diffuse-of shader) shader)))
         (light-group (compute-light-group object scene)))
    (declare (type vec color ambient-color diffuse-color))
    (shader-lambda shade-bumps (result point normal dot ray counters)
      (declare (optimize speed))
      (declare (ignore ray dot))
      (%copy-vec result ambient-color)
      (let ((noise (vector-dnoise (vec* point 5.0))))
        (%vec* noise noise 0.2)
        (setf normal (normalize (vec+ normal noise)))
        (dolist (light (light-group-lights light-group))
          (let* ((lv (light-vector light point))
                 (dot (dot-product lv normal)))
            (when (plusp dot)
              (multiple-value-bind (incident len) (illuminate light point lv counters)
                (when (plusp len)
                  ;; nx * lx/len + ny * ly/len + nz * lz/len
                  ;; == (nx*lx + ny*ly + nz*lz)/len
                  (let ((l.n (/ dot len)))
                    (macrolet
                        ((dim (n)
                           `(incf (aref result ,n)
                                  (* (aref incident ,n)
                                     (* (aref diffuse-color ,n) l.n)))))
                      (dim 0)
                      (dim 1)
                      (dim 2))))))))
        result))))