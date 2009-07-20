(in-package :raylisp)

(defclass wood-pattern (interpolated-pattern axis-mixin)
  ())

(defmethod compute-interpolated-pattern-function ((pattern wood-pattern) transform)
  (let ((inverse (inverse-matrix (matrix* transform (reorient (axis-of pattern) z-axis)))))
    (interpolated-pattern-lambda wood-pattern (point)
      (declare (optimize speed))
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p))
        ;; FIXME: Parametrize these in the object.
        (imod (+ (sqrt (+ (square (aref p 0)) (square (aref p 1))))
                 (* 0.1 (perlin-noise p 3 0.9)))
              1.0)))))