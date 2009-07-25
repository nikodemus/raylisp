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

(defclass ripple-normal (normal)
  ((origin
    :initform +origin+
    :initarg :origin
    :reader origin-of)))

(defmethod compute-perturbation-function ((normal ripple-normal) transform)
  (let* ((inverse (inverse-matrix transform))
         (height (normal-height normal))
         (origin (origin-of normal)))
    (declare (single-float height)
             (vec origin))
    (perturbation-lambda ripple-normal (result normal point)
      (declare (optimize speed))
      (let* ((p2 (transform-point point inverse))
             (tmp (alloc-vec)))
        (declare (dynamic-extent p2))
        (%noise-vec tmp p2)
        (%adjust-vec result normal tmp
                     (* height (+ (noise p2)
                                  (+ (sin (aref p2 2))
                                     (sin (+ (aref p2 1) (aref p2 0))))
                                  0.0 1.0)))
        (%normalize result result)))))