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

(in-package :raylisp-sdl)

;;;; SCENE DESCRIPTION LANGUAGE
;;;;
;;;; A small domain specific extension to CL via a few macros.

(defmacro in-scene (name &optional clear)
  (check-type name symbol)
  (check-type clear boolean)
  `(setf *scene* (ensure-scene ',name ,clear)))

(defmacro object (class &body initargs)
  `(add-object (make-instance ',class ,@initargs) *scene*))

(defmacro pattern (pattern-name &body initargs)
  `(make-instance ',(pattern-class-name pattern-name) ,@initargs))

(defmacro texture (&body initargs)
  `(make-instance 'texture-shader ,@initargs))

(defmacro light (light-name &body initargs)
  `(add-light (make-instance ',(light-class-name light-name) ,@initargs) *scene*))

(defmacro camera (camera-name &body initargs)
  `(add-camera (make-instance ',(camera-class-name camera-name) ,@initargs) *scene*))

(defvar *patterns* (make-hash-table))

(defun register-pattern (pattern-name class-name)
  (setf (gethash pattern-name *patterns*) class-name))

(defun pattern-class-name (pattern-name &optional (errorp t))
  (or (gethash pattern-name *patterns*)
      (when errorp
        (error "Unknown pattern: ~S" pattern-name))))

(defmacro define-indexed-pattern (name &body slots-and-options)
  (let ((class-name (sb-int:symbolicate name "-PATTERN")))
    `(progn
       (register-pattern ',name ',class-name)
       (defclass ,class-name (indexed-pattern)
         ,@slots-and-options))))
