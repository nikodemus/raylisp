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

(defclass checker-pattern (indexed-pattern)
  ((size
    :initform 2
    :initarg :size
    :reader indexed-pattern-size)))

(defmethod compute-indexed-pattern-function ((pattern checker-pattern) transform)
  (let ((inverse (inverse-matrix transform))
        (mod (indexed-pattern-size pattern)))
    (declare (type (integer 2 #.most-positive-fixnum) mod))
    (indexed-pattern-lambda checker-pattern (point)
      (declare (optimize speed))
      (let ((p (transform-point point inverse)))
        (declare (dynamic-extent p))
        (macrolet ((dim (n)
                     `(ffloor (+ +epsilon+ (aref p ,n)))))
          (mod (truncate (+ (dim 0) (dim 1) (dim 2))) mod))))))

