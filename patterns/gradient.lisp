(in-package :raylisp)

(defclass gradient-pattern (pattern)
  ((axis
    :initform x-axis
    :initarg :axis
    :reader axis-of)))

(defmethod compute-pattern-function ((pattern gradient-pattern) transform)
  (let ((inverse (inverse-matrix
                  (matrix* transform (reorient (axis-of pattern) x-axis)))))
    (lambda (point)
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p))
        (imod (aref p 0) 1.0)))))