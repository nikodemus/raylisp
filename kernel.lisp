(in-package :raylisp)

;;;# Kernel
;;;
;;; RAY, INTERSECTION, and ENVIRONMENT structures are our fundamental
;;; raytracing constructs.
;;;
;;; ENVIRONMENT represents all the environments the predecessors of a
;;; ray have entered, but not yet exited. Currently used only for IOR,
;;; but probably will in the future store volumetric shaders as well.
;;; Immutable.

(defstruct environment
  (ior (find-default :ior '(float 0.0)) :type (float 0.0))
  (link nil :type (or null environment)))

(defvar *global-environment* (make-environment))

;;; RAY represents an individual ray. Immutable except for the EXTENT
;;; slot, which is used by object intersection functions to store the
;;; intersection point.
;;;
;;; It might be good to have a separate EYE-RAY and SHADOW-RAY with a
;;; shared base class, since shadow rays don't need all the slots.
;;;
;;; It might also be worth it to allow mutation of other slots as
;;; well, so that the initial ray could be used to reflection or
;;; refraction, but currently this doesn't seem worth the added
;;; complexity -- but it would reduce ray-consing for refractive
;;; scenes by 50%...

(defstruct (ray
             (:constructor
              %make-ray (origin direction extent weight depth environment))) 
  (origin (required-argument) :type vector)
  (direction (required-argument) :type vector)
  (extent (required-argument) :type float)
  (weight (required-argument) :type (float 0.0 1.0))
  (depth (required-argument) :type (and fixnum unsigned-byte))
  (environment (required-argument) :type environment))

(definterface make-ray (origin direction (extent float-positive-infinity)
                        (weight 1.0) (depth 0) (environment *global-environment*))
  %make-ray)

;;; INTERSECTION stores additional data about an intersection point. 

(defstruct (intersection
             (:constructor
              %make-intersection (point normal n.d)))
  (point (required-argument) :type vector)
  (normal (required-argument) :type vector)
  (n.d (required-argument) :type float))

(definterface make-intersection (point normal n.d)
  %make-intersection)

;;; Spawning new rays. The SPAWN-RAYS is quite ugly and should be fixed.

(defun reflected-ray (intersection ray specular)
  (declare (type intersection intersection)
           (type ray ray)
           (type float specular))
  (note-reflected-ray)
  ;; Intersection normal is always on the side of ray origin, but
  ;; precalculated N.D may be negative:
  ;;
  ;; R = Rx + Ry, D = Dx + Dy
  ;; => D = Rx - Ry <=> Rx = D + Ry
  ;;
  ;; |Ry| = |N.D| => Ry = N*|N.D|
  ;;
  ;; => R = D + 2*Ry = D + 2*N*|N.D|
  (make-ray
   :origin (intersection-point intersection)
   :direction
   ;; The code below is really this, but avoid intermediate vectors.
   ;; FIXME: It would be nice if we had compiler-macros for this...
   ;;
   ;;  (normalize
   ;;   (adjust-vector (ray-direction ray) 
   ;;                  (intersection-normal intersection)
   ;;                  (* 2.0 (abs (intersection-n.d intersection)))))
   ;;
   (let ((d (ray-direction ray))
         (n (intersection-normal intersection))
         (f (* 2.0 (abs (intersection-n.d intersection)))))
     (with-arrays (n d)
       (macrolet ((dim (i)
                    `(+ (d ,i) (* (n ,i) f))))
         (let* ((x (dim 0)) (y (dim 1)) (z (dim 2))
                (l (sqrt (+ (* x x) (* y y) (* z z)))))
           (vector (/ x l) (/ y l) (/ z l))))))
   :weight (* (ray-weight ray) specular)
   :depth (1+ (ray-depth ray))
   :environment (ray-environment ray)))

(defun spawn-rays (intersection ray specular transmit ior)
  (declare (type intersection intersection)
           (type ray ray)
           (type float specular transmit ior))
  (let ((n.d (intersection-n.d intersection))
	(current (ray-environment ray)))
    (multiple-value-bind (rel-ior new)
	(if (plusp n.d)
	    ;; ray exits: IOR is from the current environment
	    (let ((previous (environment-link current)))
	      (values (/ ior (environment-ior previous))
		      previous))
	    ;; ray enters: IOR is from the new environment
	    (values (/ (environment-ior current) ior)
		    (make-environment :ior ior :link current)))
      ;; FIXME: Add derivation of the refraction formula as a comment.
      (let ((cs2 (- 1.0 (* (square rel-ior) (- 1.0 (square n.d))))))
	(if (significantp cs2)
	    ;; normal case
	    (values
	     (reflected-ray intersection ray specular)
	     (progn
	       (note-refracted-ray)
	       (make-ray
		:origin (intersection-point intersection)
		:direction
                ;; The code below is really this, but avoids
                ;; intermediate vectors. FIXME: It would be nice if we
                ;; had compiler-macros to do this...
                ;;
                ;;  (normalize
                ;;   (vector-add
                ;;    (vector-mul (intersection-normal intersection)
                ;;                (- (* rel-ior (abs n.d)) (sqrt cs2)))
                ;;    (vector-mul (ray-direction ray) rel-ior)))
                ;;
                (let ((n (intersection-normal intersection))
                      (d (ray-direction ray))
                      (f (- (* rel-ior (abs n.d)) (sqrt cs2))))
                  (with-arrays (n d)
                    (macrolet ((dim (i)
                                 `(+ (* f (n ,i)) (* rel-ior (d ,i)))))
                      (let* ((x (dim 0)) (y (dim 1)) (z (dim 2))
                             (l (sqrt (+ (* x x) (* y y) (* z z)) )))
                        (vector (/ x l) (/ y l) (/ z l))))))
		:weight (* (ray-weight ray) transmit)
		:depth (1+ (ray-depth ray))
		:environment new)))
	    ;; total internal reflection
	    ;;
	    ;; FIXME: the second dummy ray here seems like an ugly hack
	    (values 
	     (reflected-ray intersection ray 1.0)
	     (load-time-value 
	      (make-ray :origin origin :direction x-axis :weight 0.0))))))) )

