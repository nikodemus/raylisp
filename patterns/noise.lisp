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

(defclass noise-pattern (interpolated-pattern)
  ((mode
    :initarg :mode
    :initform :scale)))

(defmethod compute-interpolated-pattern-function ((pattern noise-pattern) transform)
  (let ((inverse (inverse-matrix transform))
        (mode (slot-value pattern 'mode)))
    (declare (optimize speed))
    ;; Noise comes in -1 to 1 range, so there are three obvious ways
    ;; to deal with it.
    (ecase mode
      (:scale
       (interpolated-pattern-lambda scale-noise-pattern (point)
         (let ((p (transform-point point inverse)))
           (declare (dynamic-extent p))
           (* 0.5 (+ 1.0 (vector-noise p))))))
      (:abs
       (interpolated-pattern-lambda abs-noise-pattern (point)
         (let ((p (transform-point point inverse)))
           (declare (dynamic-extent p))
           (abs (vector-noise p)))))
      (:clamp
       (interpolated-pattern-lambda clamp-noise-pattern (point)
         (let ((p (transform-point point inverse)))
           (declare (dynamic-extent p))
           (clamp (vector-noise p) 0.0 1.0)))))))

