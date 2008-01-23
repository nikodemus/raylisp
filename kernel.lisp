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

;;; Spawning new rays. The SPAWN-RAYS is quite ugly and should be fixed.

(defun reflected-ray (point normal dot ray specular counters)
  (declare (type vector point normal)
           (type ray ray)
           (type float dot specular))
  (note-reflected-ray counters)
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
   :origin point
   :direction
   ;; The code below is really this, but avoid intermediate vectors.
   ;; FIXME: It would be nice if we had compiler-macros for this...
   ;;
   ;;  (normalize
   ;;   (adjust-vector (ray-direction ray) 
   ;;                  normal
   ;;                  (* 2.0 (abs dot))))
   ;;
   (let ((d (ray-direction ray))
         (f (if (minusp dot) (* -2.0 dot) (* 2.0 dot))))
     (with-arrays (normal d)
       (macrolet ((dim (i)
                    `(+ (d ,i) (* (normal ,i) f))))
         (let* ((x (dim 0)) (y (dim 1)) (z (dim 2))
                (l (sqrt (+ (* x x) (* y y) (* z z)))))
           (vector (/ x l) (/ y l) (/ z l))))))
   :weight (* (ray-weight ray) specular)
   :depth (1+ (ray-depth ray))
   :environment (ray-environment ray)))

(defun spawn-rays (point normal dot ray specular transmit ior counters)
  (declare (type vector point normal)
           (type ray ray)
           (type float dot specular transmit ior))
  (let ((current (ray-environment ray)))
    (multiple-value-bind (rel-ior new)
	(if (plusp dot)
	    ;; ray exits: IOR is from the current environment
	    (let ((previous (environment-link current)))
	      (values (/ ior (environment-ior previous))
		      previous))
	    ;; ray enters: IOR is from the new environment
	    (values (/ (environment-ior current) ior)
		    (make-environment :ior ior :link current)))
      ;; FIXME: Add derivation of the refraction formula as a comment.
      (let ((cs2 (- 1.0 (* (square rel-ior) (- 1.0 (square dot))))))
	(if (significantp cs2)
	    ;; normal case
	    (values
	     (reflected-ray point normal dot ray specular counters)
	     (progn
	       (note-refracted-ray counters)
	       (make-ray
		:origin point
		:direction
                ;; The code below is really this, but avoids
                ;; intermediate vectors. FIXME: It would be nice if we
                ;; had compiler-macros to do this...
                ;;
                ;;  (normalize
                ;;   (vector-add
                ;;    (vector-mul normal
                ;;                (- (* rel-ior (abs dot)) (sqrt cs2)))
                ;;    (vector-mul (ray-direction ray) rel-ior)))
                ;;
                (let ((d (ray-direction ray))
                      (f (- (* rel-ior (abs dot)) (sqrt cs2))))
                  (with-arrays (normal d)
                    (macrolet ((dim (i)
                                 `(+ (* f (normal ,i)) (* rel-ior (d ,i)))))
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
	     (reflected-ray point normal dot ray 1.0 counters)
	     (load-time-value 
	      (make-ray :origin origin :direction x-axis :weight 0.0))))))) )

