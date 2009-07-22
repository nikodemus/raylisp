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

(defpackage :raylisp-obj-loader
  (:use :cl)
  (:import-from :raylisp
                #:define-mesh-loader
                #:build-mesh
                #:v))

(in-package :raylisp-obj-loader)

(define-mesh-loader :obj load-obj)

(defun load-obj (pathname transform)
  ;; This is seriously incomplete!
  (let (faces vertices)
    (block snarf
      (with-open-file (f pathname)
        (loop
          (ecase (read-char f nil :eof)
            ((#\# #\newline #\g)
             (read-line f))
            (#\v
             (push (v (read f) (read f) (read f)) vertices))
            (#\f
             (push (vector (1- (read f)) (1- (read f)) (1- (read f))) faces))
            (:eof
             (return-from snarf))))))
    (let ((vertex-vector (coerce (nreverse vertices) 'simple-vector))
          (face-vector (coerce (nreverse faces) 'simple-vector)))
      (build-mesh vertex-vector face-vector :transform transform))))
