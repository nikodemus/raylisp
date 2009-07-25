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
(defun ifloor (f &optional (r 1.0))
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

;;;## Vectors

(defun v (x y z)
  (vec (float x) (float y) (float z)))

(defun deg (degrees)
  "Returns radians for degrees."
  (float (* pi (/ degrees 180))))

(declaim (inline dot-product*))
(defun dot-product* (ax ay az bx by bz)
  (declare (type float ax ay az bx by bz))
  (+ (* ax bx) (* ay by) (* az bz)))

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
  (def +x+ 1.0 0.0 0.0)
  (def +y+ 0.0 1.0 0.0)
  (def +z+ 0.0 0.0 1.0)
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
