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
   "FLOAT"
   "FLOATP"
   "INTERSECTION"
   "VECTOR"
   "SIMPLE-VECTOR"
   "TYPE-OF"))
