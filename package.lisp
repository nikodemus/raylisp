;;;# Package Structure
;;;
;;; Raylisp resides in a single package, and provides currently no
;;; exported interface: to use Raylisp, work in the RAYLISP package
;;; (nickname RL).
;;;
;;; RAYLISP shadows some Common Lisp symbols that are just too
;;; convenient to reserve.

(defpackage "RAYLISP"
  (:nicknames "RL")
  (:use "CL" "ALEXANDRIA")
  (:shadow
   "FLOAT"
   "FLOATP"
   "INTERSECTION"
   "SIMPLE-VECTOR"
   "VECTOR"
   "TYPE-OF")
  (:export
   ;; protocol classes
   "SCENE-OBJECT"
   "SCENE-LIGHT"
   ;; protocol functions
   "COMPUTE-OBJECT-PROPERTIES"
   "COMPUTE-OBJECT-EXTENTS"
   "COMPUTE-LIGHT-PROPERTIES"
   ;; scene compilation
   "COMPILE-SCENE-OBJECT"
   "COMPILE-SCENE-LIGHT"
   

   "FLOAT"
   "FLOATP"
   "INTERSECTION"
   "VECTOR"
   "SIMPLE-VECTOR"
   "TYPE-OF"))
