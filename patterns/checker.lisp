;;; A checker pattern of two different shaders, ODD and EVEN.

(in-package :raylisp)

(defclass checker-pattern (indexed-pattern)
  ())

(defmethod indexed-pattern-size ((pattern checker-pattern))
  2)

(defmethod compute-pattern-key-function ((pattern checker-pattern) transform)
  (let ((inverse (inverse-matrix transform)))
    (lambda (point)
      (declare (optimize speed) (vec point))
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p) (vec p))
        (macrolet ((dim (n)
                     `(ffloor (+ epsilon (aref p ,n)))))
          (if (oddp (truncate (+ (dim 0) (dim 1) (dim 2))))
              0
              1))))))
