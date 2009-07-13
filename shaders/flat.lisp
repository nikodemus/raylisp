;;; Provides only the ambient component.

(in-package :raylisp)

(defclass flat-shader (shader color-mixin ambient-shader-mixin)
  ())

(defmethod compute-shader-function ((shader flat-shader) object scene transform)
  (let ((ambient-color (hadamard-product
                        (vec* (scene-ambient-light scene)
                              (coefficient (ambient-of shader) shader))
                        (color-of shader))))
    (constantly ambient-color)))
