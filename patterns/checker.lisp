;;; A checker pattern of two different shaders, ODD and EVEN.

(in-package :raylisp)

(defclass checker-pattern (indexed-pattern)
  ())

(defmethod pattern-map-size ((pattern checker-pattern))
  2)

(defmethod compute-pattern-key-function ((pattern checker-pattern) transform)
  (let ((inverse (inverse-matrix transform)))
    (break "t = ~S" transform)
    (lambda (point)
      (declare (optimize speed) (vec point))
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p) (vec p))
        (macrolet ((dim (n)
                     `(ifloor (+ epsilon (aref p ,n)) 1.0)))
          (if (oddp (+ (dim 0) (dim 1) (dim 2)))
              0
              1))))))
