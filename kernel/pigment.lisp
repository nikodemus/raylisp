;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :raylisp)

;;;; PIGMENT
;;;;
;;;; Pigment determines the color at a 3D point. Anything that has
;;;; an approriate method defined on COMPUTE-PIGMENT-FUNCTION qualifies
;;;; as a pigment.

(declaim (ftype (function (t matrix) pigment-function)
                compute-pigment-function))
(defgeneric compute-pigment-function (pigment matrix))

(defmethod compute-pigment-function ((color #.(class-of (alloc-vec))) matrix)
  (constant-pigment-function color))

(defmethod compute-pigment-function :around (pigment matrix)
  (check-function-type (call-next-method) 'pigment-function))

(defmethod compute-pigment-function :around ((pigment transformable) matrix)
  (call-next-method pigment (matrix* matrix (transform-of pigment))))

(define-named-lambda pigment-lambda color ((result color) (point vec)) :safe nil)

(defvar *constant-pigments*
  (make-hash-table :test #'vec= :hash-function #'sxhash-vec))

(defun constant-pigment-function (value)
  (check-type value vec)
  (or (gethash value *constant-pigments*)
      (setf (gethash value *constant-pigments*)
            (pigment-lambda constant-pigment (result point)
              (declare (ignore result point))
              value))))
