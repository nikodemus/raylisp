(in-package :raylisp)

(defclass marble-pattern (pattern axis-mixin)
  ())

(defmethod compute-pattern-key-function ((pattern marble-pattern) transform)
  (let ((inverse (inverse-matrix (matrix* transform (reorient (axis-of pattern) z-axis)))))
    (lambda (point)
      (declare (optimize speed))
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p))
        (clamp (abs (cos (+ (aref p 0) (turbulence p 0.4 4.0))))
               0.0 1.0)))))
