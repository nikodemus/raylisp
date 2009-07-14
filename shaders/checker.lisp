;;; A checker pattern of two different shaders, ODD and EVEN.

(in-package :raylisp)

(defclass checker-shader (shader)
  ((odd :initarg :odd :accessor odd-of)
   (even :initarg :even :accessor even-of)))

(defun odd-checkerp (point)
  (declare (type vec point)
           (optimize speed))
  (macrolet ((dim (n)
               `(ifloor (+ epsilon (aref point ,n)) 1.0)))
    (oddp (+ (dim 0) (dim 1) (dim 2)))))

(defmethod compute-shader-function ((shader checker-shader) object scene transform)
  (let* ((inverse (inverse-matrix transform))
         (odd (compile-shader (odd-of shader) object scene transform))
         (even (compile-shader (even-of shader) object scene transform)))
    (sb-int:named-lambda shade-checher (point normal dot ray counters)
      (declare (optimize speed))
      (let ((p2 (transform-point point inverse)))
        (declare (dynamic-extent p2))
        (funcall (if (odd-checkerp p2)
                     odd
                     even)
                 point
                 normal
                 dot
                 ray
                 counters)))))
