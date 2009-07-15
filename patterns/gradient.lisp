(in-package :raylisp)

(defclass gradient-pattern (pattern axis-mixin)
  ())

(defmethod compute-pattern-function ((pattern gradient-pattern) transform)
  (let ((inverse (inverse-matrix
                  (matrix* transform (reorient (axis-of pattern) z-axis)))))
    (lambda (point)
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p))
        (imod (aref p 0) 1.0)))))