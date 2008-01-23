(in-package :raylisp)

;;;# Mathematics
;;;
;;; Raylisp includes basic linear algebra support, such as vector and
;;; matrix operations. For efficiency reasons most functions that
;;; either return or accept bare floating point values (as opposed to
;;; vectors or matrices) are declaimed INLINE.

;;; We define an EXPR replacement with a compiler macro to get
;;; fast powers even at the cost of some accuracy.
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
  "Returns a vector of positive roots for ax^2+bx+c."
  (declare (type float a b c))
  (let ((d (- (square b) (* 4.0 a c))))
    (if (minusp d)
        (float-vector)
        (let* ((sqrt-d (sqrt d))
               (2a (* 2.0 a))
               (r1 (/ (+ (- b) sqrt-d) 2a))
               (r2 (/ (- (- b) sqrt-d) 2a)))
          (cond ((significantp r1)
                 (if (significantp r2)
                     (if (< r1 r2)
                         (float-vector r1 r2)
                         (float-vector r2 r1))
                     (float-vector r1)))
                ((significantp r2)
                 (float-vector r2))
                (t
                 (float-vector)))))))

;;;## Vectors
;;;
;;; Vectors are all three-dimensional and are considered immutable; no
;;; vector operations mutate their arguments. Support for non-consing
;;; operations in not inconcievable, but would probably be best
;;; handled by compiler-macros that combine things like (VECTOR-ADD
;;; (VECTOR-MUL ...) ...) into a single operation.
;;;
;;; Even though mathematically the distinction between a point and a
;;; vector is a clear and important, we allow the boundary to blur,
;;; using the term "point" only when it is vital for understanding.
;;; The VECTOR type is used for both points and vectors, and currently
;;; for RGB colors as well.

(deftype vector ()
  '(simple-array float (3)))

(declaim (inline vector))
(defun vector (x y z)
  (float-vector x y z))

(declaim (inline alloc-vector))
(defun alloc-vector ()
  (make-array 3 :element-type 'float))

(defun @ (x y z)
  "Convenience vector constructor for interactive use."
  (vector (float x) (float y) (float z)))

(macrolet 
    ((def (name op &key inline scalar (combine 'vector))
       `(progn
	  ,@(when inline `((declaim (inline ,name))))
	  (defun ,name (a ,(if scalar 'f 'b))
	    (declare (type vector a ,@(unless scalar '(b)))
		     ,@(when scalar '((type float f)))
		     (optimize speed))
	    (,combine (,op (aref a 0) ,(if scalar 'f '(aref b 0)))
		      (,op (aref a 1) ,(if scalar 'f '(aref b 1)))
		      (,op (aref a 2) ,(if scalar 'f '(aref b 2))))))))
  (def vector-add +)
  (def vector-sub -)
  (def hadamard-product *)
  (def vector-div / :inline t :scalar t)
  (def vector-mul * :inline t :scalar t)
  (def dot-product * :inline t :combine +))

(declaim (inline dot-product*))
(defun dot-product* (ax ay az bx by bz)
  (declare (type float ax ay az bx by bz))
  (+ (* ax bx) (* ay by) (* az bz)))

(declaim (inline vector-length))
(defun vector-length (v)
  (declare (vector v))
  (with-arrays (v)
    (macrolet ((dim (n) `(square (v ,n))))
      (sqrt (+ (dim 0) (dim 1) (dim 2))))))

(defun vector-min (&rest vectors)
  (macrolet ((dim (n) `(min (aref a ,n) (aref b ,n))))
    (reduce (lambda (a b)
              (vector (dim 0) (dim 1) (dim 2)))
            vectors)))

(defun vector-max (&rest vectors)
  (macrolet ((dim (n) `(max (aref a ,n) (aref b ,n))))
    (reduce (lambda (a b)
              (vector (dim 0) (dim 1) (dim 2)))
            vectors)))

(defun reverse-vector (v)
  (declare (type vector v))
  (with-arrays (v)
    (vector (- (v 0)) (- (v 1)) (- (v 2)))))

(declaim (inline vector-lerp))
(defun vector-lerp (a b f)
  (let ((f2 (- 1 f)))
    (with-arrays (a b)
      (macrolet ((dim (n) `(+ (* f2 (a ,n)) (* f (b ,n)))))
        (vector (dim 0) (dim 1) (dim 2))))))

(declaim (inline adjust-vector))
(defun adjust-vector (p d f)
  (declare (type float f))
  (with-arrays (p d)
    (macrolet ((dim (n) `(+ (p ,n) (* (d ,n) f))))
      (vector (dim 0) (dim 1) (dim 2)))))

(defun cross-product (a b)
  (declare (type vector a b))
  (with-arrays (a b)
    (vector (- (* (a 1) (b 2)) (* (a 2) (b 1)))
	    (- (* (a 2) (b 0)) (* (a 0) (b 2)))
	    (- (* (a 0) (b 1)) (* (a 1) (b 0))))))

(declaim (inline normalized-vector))
(defun normalized-vector (x y z)
  (declare (type float x y z))
  (let ((len (sqrt (+ (square x) (square y) (square z)))))
    (vector (/ x len) (/ y len) (/ z len))))

(defun normalize (v)
  (declare (type vector v))
  (with-arrays (v)
    (normalized-vector (v 0) (v 1) (v 2))))

;;;### Some Constant Vectors
;;;
;;; We provide some constant vectors for convenience. Currently
;;; implemented with DEFINE-SYMBOL-MACRO to work around SBCL
;;; deficiency in references to non-numeric constants.

(macrolet ((def (name r g b)
	     `(define-symbol-macro ,name 
		  (vector ,r ,g ,b))))
  (def origin 0.0 0.0 0.0)
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
;;;
;;; Matrices are all 4x4 and are considered immutable; no matrix
;;; operations mutate their arguments.

(deftype matrix ()
  '(simple-array float (4 4)))

(defun matrix (init)
  ;; This is pretty slow, but that is all right as no matrices are
  ;; created at runtime.
  (make-array '(4 4) :element-type 'float :initial-contents init))

(defun zero-matrix ()
  (make-array '(4 4) :element-type 'float :initial-element 0.0))

(defun identity-matrix ()
  (matrix (list (list 1.0 0.0 0.0 0.0)
		(list 0.0 1.0 0.0 0.0)
		(list 0.0 0.0 1.0 0.0)
		(list 0.0 0.0 0.0 1.0))))

(defun inverse-matrix (matrix)
  "Inverse of an orthogonal affine 4x4 matrix."
  (declare (type matrix matrix))
  (let ((inverse (zero-matrix)))
    (with-arrays (matrix inverse)
      ;; transpose and invert scales for upper 3x3
      (dotimes (i 3)
	(let ((scale (/ 1.0 (+ (square (matrix 0 i))
			       (square (matrix 1 i))
			       (square (matrix 2 i))))))
	  (dotimes (j 3)
	    (setf (inverse i j) (* scale (matrix j i))))))
      ;; translation: negation after dotting with upper rows
      (let ((x (matrix 0 3))
	    (y (matrix 1 3))
	    (z (matrix 2 3)))
	(dotimes (i 3)
	  (setf (inverse i 3) (- (+ (* x (inverse i 0))
				    (* y (inverse i 1))
				    (* z (inverse i 2)))))))
      ;; affine bottom row (0 0 0 1)
      (dotimes (i 3)
	(setf (inverse 3 i) 0.0))
      (setf (inverse 3 3) 1.0)
      inverse)))

(defun matrix-product (&rest matrices)
  "Product of n 4x4 matrices. For convenience NILs are also
accepted and ignored."
  (labels ((product (m n)
	     (let ((product (zero-matrix)))
	       (dotimes (i 4)
		 (dotimes (j 4)
		   (setf (aref product i j)
			 (loop for k below 4 
			       summing (* (aref m i k) (aref n k j))))))
	       product)))
    (reduce #'product (remove-if #'null matrices))))

(defun transpose-matrix (matrix)
  (let ((transpose (zero-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref transpose i j) (aref matrix j i))))
    transpose))

;;;### Transformations
;;;
;;; These functions apply transformation matrices to points/vectors.

(macrolet ((def (name doc combine translatep)
	     ;; open-code manually. *sigh*
	     `(progn
                ,@(unless (eq combine 'vector)
                          `((declaim (inline ,name))))
                (defun ,name (v matrix)
                  ,doc
                  (declare (optimize speed)
                           (type vector v)
                           (type matrix matrix))
                  (with-arrays (v matrix)
                    (let ((x (v 0)) (y (v 1)) (z (v 2)))
                      (macrolet ((row (n) 
                                   `(+ (* x (matrix ,n 0))
                                       (* y (matrix ,n 1))
                                       (* z (matrix ,n 2))
                                       ,@(when ,translatep `((matrix ,n 3))))))
                        (,combine (row 0) (row 1) (row 2)))))))))
  (def transform-vector
      "Apply a 4x4 matrix to vector."
    vector t)
  (def transform-vector-values
      "Apply a 4x4 matrix to vector, returning result as multiple values."
    values t)
  (def transform/normalize-vector
      "Apply a 4x4 matrix to vector, normalizing the result."
    normalized-vector t)
  (def transform-direction
      "Apply a 4x4 matrix to vector ignoring translation."
    vector nil)
  (def transform-direction-values
      "Apply a 4x4 matrix to vector ignoring translation, returning the result
as multiple values."
    values nil))

;;;### Additional Matrix Constructors
;;;
;;; We include here, as matrix operations, some convenience functions
;;; for construction of various transformation matrices as well.

(defun translate* (x y z)
  (matrix (list (list 1.0 0.0 0.0 (float x))
		(list 0.0 1.0 0.0 (float y))
		(list 0.0 0.0 1.0 (float z))
		(list 0.0 0.0 0.0 1.0))))

(defun translate (v)
  (with-arrays (v)
    (translate* (v 0) (v 1) (v 2))))

(defun scale* (x y z)
  (matrix (list (list (float x)       0.0       0.0   0.0)
		(list       0.0 (float y)       0.0   0.0)
		(list       0.0       0.0 (float z)   0.0)
		(list       0.0       0.0       0.0   1.0))))

(defun scale (v)
  (with-arrays (v)
    (scale* (v 0) (v 1) (v 2))))

(defun rotate-around (v radians)
  (let ((c (cos radians))
	(s (sin radians))
	(g (- 1.0 (cos radians))))
    (with-arrays (v)
      (let* ((x (v 0)) (y (v 1)) (z (v 2))
	     (gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
	     (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
	(matrix 
	 (list 
	  (list       (+ gxx c) (- gxy (* s z)) (+ gxz (* s y)) 0.0)
	  (list (+ gxy (* s z))       (+ gyy c) (- gyz (* s x)) 0.0)
	  (list (- gxz (* s y)) (+ gyz (* s x))       (+ gzz c) 0.0)
	  (list             0.0             0.0             0.0 1.0)))))))

(defun reorient (a b)
  "Construct a transformation matrix to reorient A with B."
  (let ((na (normalize a))
	(nb (normalize b)))
    (if (approximates na nb)
	(identity-matrix)
	(rotate-around (normalize (cross-product na nb))
		       (acos (dot-product na nb))))))

;;;### Inverse and Adjunct Matrices

(defun inverse-and-adjunct/inverse-matrix (matrix)
  (let ((inverse (inverse-matrix matrix)))
    (values inverse
	    (matrix-product (transpose-matrix inverse) 
			    inverse))))

(defun inverse-and-adjunct-matrix (matrix)
  (let ((inverse (inverse-matrix matrix)))
    (values inverse
	    (transpose-matrix inverse))))
