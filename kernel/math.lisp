(in-package :raylisp)

;;;# Mathematics
;;;
;;; Raylisp includes basic linear algebra support, such as vector and
;;; matrix operations. For efficiency reasons most functions that
;;; either return or accept bare floating point values (as opposed to
;;; vectors or matrices) are declaimed INLINE.

(declaim (inline square))
(defun square (f)
  (* f f))

(declaim (inline imod))
(defun imod (f r)
  (declare (float f r)
           (inline mod))
  (if (< #.(float most-negative-fixnum) f #.(float most-positive-fixnum))
      (mod f r)
      (mod f r)))

(declaim (inline ifloor))
(defun ifloor (f r)
  (declare (float f r)
           (inline mod))
  (if (< #.(float most-negative-fixnum) f #.(float most-positive-fixnum))
      (floor f r)
      (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (floor f r))))

;;; We define an EXPT replacement with a compiler macro to get
;;; fast powers even at the cost of some accuracy: the compiler
;;; macro uses the minimum number of multiplications.
(declaim (inline power))
(defun power (base power)
  (expt base power))

(define-compiler-macro power (&whole whole base power &environment env)
  (if (integerp power)
      (labels ((build-expr (n last-var alist)
                 (if (= n power)
                     last-var
                     (let* ((alist (acons n last-var alist))
                            (other (assoc-if (lambda (y)
                                               (<= (+ y n) power))
                                             alist))
                            (nm (+ n (car other)))
                            (x^y (make-symbol (format nil "X^~A" nm))))
                       `(let ((,x^y (* ,last-var ,(cdr other))))
                          ,(build-expr nm x^y alist))))))
        (let ((form (let ((positive-expt-expr (build-expr 1 'x nil)))
                      (if (plusp power)
                          positive-expt-expr
                          `(/ ,positive-expt-expr))))
              (real-base (macroexpand base env)))
          (if (atom real-base)
              (subst base 'x form)
              `(let ((x ,base)) ,form))))
      whole))

;;;## Root Solvers
;;;
;;; Two simple and exceedingly naive quadratic root solvers: it should
;;; be noted that these most likely suffer from numerical accuracy
;;; issues, but the author's current competence does not extend to
;;; fixing this right now.

(declaim (inline min-pos-quad-root))
(defun min-pos-quad-root (a b c)
  "Returns the smallest positive real root for ax^2+bx+c, or -1.0
if there are no positive real roots greater then epsilon."
  (declare (type float a b c))
  (let ((d (- (square b) (* 4.0 a c))))
    (if (minusp d)
	-1.0
	(let* ((sqrt-d (sqrt d))
	       (2a (* 2.0 a))
	       (r1 (/ (+ (- b) sqrt-d) 2a))
	       (r2 (/ (- (- b) sqrt-d) 2a)))
          (if (significantp r1)
              (if (significantp r2)
                  (min r1 r2)
                  r1)
              (if (significantp r2)
                  r2
                  -1.0))))))

(declaim (inline pos-quad-roots))
(defun pos-quad-roots (a b c)
  "Returns positive roots for ax^2+bx+c."
  (declare (type float a b c))
  (let ((d (- (square b) (* 4.0 a c))))
    (if (minusp d)
        (values -1.0 -1.0)
        (let* ((sqrt-d (sqrt d))
               (2a (* 2.0 a))
               (r1 (/ (+ (- b) sqrt-d) 2a))
               (r2 (/ (- (- b) sqrt-d) 2a)))
          (cond ((significantp r1)
                 (if (significantp r2)
                     (if (< r1 r2)
                         (values r1 r2)
                         (values r2 r1))
                     (values r1 -1.0)))
                ((significantp r2)
                 (values r2 -1.0))
                (t
                 (values -1.0 -1.0)))))))

;;;## Vectors

(defun @ (x y z)
  "Convenience vector constructor for interactive use."
  (vec (float x) (float y) (float z)))

(defun v (x y z)
  (vec (float x) (float y) (float z)))

(declaim (inline dot-product*))
(defun dot-product* (ax ay az bx by bz)
  (declare (type float ax ay az bx by bz))
  (+ (* ax bx) (* ay by) (* az bz)))

(declaim (inline adjust-vec))
(defun adjust-vec (p d f)
  (declare (type float f) (type vec p d))
  (with-arrays (p d)
    (macrolet ((dim (n) `(+ (p ,n) (* (d ,n) f))))
      (vec (dim 0) (dim 1) (dim 2)))))

(declaim (inline normalized-vec))
(defun normalized-vec (x y z)
  (declare (type float x y z))
  (let ((len (sqrt (+ (square x) (square y) (square z)))))
    (vec (/ x len) (/ y len) (/ z len))))

;;;### Some Constant Vectors
;;;
;;; We provide some constant vectors for convenience. Currently
;;; implemented with DEFINE-SYMBOL-MACRO to work around SBCL
;;; deficiency in references to non-numeric constants.

(macrolet ((def (name r g b)
	     `(define-constant ,name (vec ,r ,g ,b)
                :test #'vec=)))
  (def +origin+ 0.0 0.0 0.0)
  (def x-axis 1.0 0.0 0.0)
  (def y-axis 0.0 1.0 0.0)
  (def z-axis 0.0 0.0 1.0)
  (def black  0.0 0.0 0.0)
  (def white  1.0 1.0 1.0)
  (def red    1.0 0.0 0.0)
  (def green  0.0 1.0 0.0)
  (def blue   0.0 0.0 1.0)
  (def yellow 1.0 1.0 0.0)
  (def purple 1.0 0.0 1.0))

;;;## Matrices

(defun inverse-and-adjunct/inverse-matrix (matrix)
  (let ((inverse (inverse-matrix matrix)))
    (values inverse
	    (matrix* (transpose-matrix inverse) inverse))))

(defun inverse-and-adjunct-matrix (matrix)
  (let ((inverse (inverse-matrix matrix)))
    (values inverse
	    (transpose-matrix inverse))))

