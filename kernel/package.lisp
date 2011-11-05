;;;# Package Structure
;;;
;;; Raylisp resides in a single package, and provides currently no
;;; exported interface: to use Raylisp, work in the RAYLISP package
;;; (nickname RL).
;;;
;;; RAYLISP shadows some Common Lisp symbols that are just too
;;; convenient to reserve.

(defpackage :raylisp
  #+nil
  (:nicknames "RL")
  (:use :cl :alexandria :sb-cga)
  (:shadow
   #:float
   #:floatp
   #:intersection
   #:simple-vector
   #:vector
   #:type-of)
  (:shadowing-import-from :sb-cga #:rotate)
  (:export
   ;; protocol classes
   #:scene-object
   #:scene-light
   ;; protocol functions
   #:compute-object-properties
   #:compute-object-extents
   #:compute-light-properties
   ;; scene compilation
   #:compile-scene-object
   #:compile-scene-light

   #:pi
   #:float
   #:floatp
   #:intersection
   #:vector
   #:simple-vector
   #:type-of

   #:define-raylisp-package))

(in-package :raylisp)

(defmacro define-raylisp-package (name &body options)
  `(defpackage ,name
     ,@options
     (:use :cl :sb-cga :raylisp)
     (:shadowing-import-from :raylisp
                             #:float
                             #:floatp
                             #:intersection
                             #:simple-vector
                             #:vector
                             #:type-of)
     (:shadowing-import-from :sb-cga #:rotate)))

(define-raylisp-package :raylisp-user)


