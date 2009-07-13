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
  (let* ((t1 (matrix* transform (transform-of shader)))
         (inverse (inverse-matrix (matrix* t1 (transform-of object))))
         (odd (compile-shader (odd-of shader) object scene t1))
         (even (compile-shader (even-of shader) object scene t1)))
    (sb-int:named-lambda shade-checher (obj point normal dot ray counters)
      (declare (optimize speed))
      (let ((p2 (transform-point point inverse)))
        (declare (dynamic-extent p2))
        (funcall (if (odd-checkerp p2)
                     odd
                     even)
                 obj
                 point
                 normal
                 dot
                 ray
                 counters)))))
