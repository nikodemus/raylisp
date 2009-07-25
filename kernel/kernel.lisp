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

(declaim (inline make-environment))
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

(declaim (inline make-ray))
(defstruct (ray)
  (origin (required-argument) :type vec)
  (direction (required-argument) :type vec)
  (extent float-positive-infinity :type float)
  (weight 1.0 :type (float 0.0 1.0))
  (depth 0 :type (and fixnum unsigned-byte))
  (environment *global-environment* :type environment))

(defmethod print-object ((ray ray) stream)
  (print-unreadable-object (ray stream :type nil)
    (format stream "Ray ~S @ ~S ~F~%  ~
                    weight: ~S, depth: ~S"
            (ray-origin ray) (ray-direction ray) (ray-extent ray)
            (ray-weight ray) (ray-depth ray))))

(defmacro with-ray ((var &rest args) &body forms)
  `(let ((,var (make-ray ,@args)))
     (declare (dynamic-extent ,var))
     ,@forms))

(defmacro with-transformed-ray ((new original matrix) &body body)
  (with-gensyms (new-origin new-direction)
    (once-only (original matrix)
      `(let ((,new-origin (transform-point (ray-origin ,original) ,matrix))
             (,new-direction (transform-direction (ray-direction ,original) ,matrix)))
         (declare (dynamic-extent ,new-origin ,new-direction))
         (with-ray (,new :origin ,new-origin
                         :direction ,new-direction
                         :extent (ray-extent ,original)
                         :weight (ray-weight ,original)
                         :depth (ray-depth ,original)
                         :environment (ray-environment ,original))
           ,@body)))))

;;; Spawning new rays. The SPAWN-RAYS is quite ugly and should be fixed.

(declaim (inline reflected-ray-direction))
(defun reflected-ray-direction (normal dot ray)
  (declare (type vec normal)
           (type ray ray)
           (type float dot))
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
         (vec (/ x l) (/ y l) (/ z l)))))))

(declaim (inline %refracted-ray-direction))
(defun %refracted-ray-direction (result normal dot ray rel-ior cs2)
  ;; The code below is really this, but avoids
  ;; intermediate vectors.
  ;;
  ;;  (normalize
  ;;   (vector-add
  ;;    (vector-mul normal
  ;;                (- (* rel-ior (abs dot)) (sqrt cs2)))
  ;;    (vector-mul (ray-direction ray) rel-ior)))
  ;;
  (let ((tmp (alloc-vec)))
    (declare (dynamic-extent tmp))
    (%normalize result
                (%vec+ result
                       (%vec* result normal (- (* rel-ior (abs dot)) (sqrt cs2)))
                       (%vec* tmp (ray-direction ray) rel-ior)))))

(declaim (inline weak-ray-p))
(defun weak-ray-p (ray scene)
  (declare (ray ray) (scene scene))
  (or (= (ray-depth ray) (scene-depth-limit scene))
      (< (ray-weight ray) (scene-adaptive-limit scene))))

(defmacro with-reflected-ray
    ((reflected &key point normal dot-product incident-ray specular counters)
     &body forms)
  ;; Intersection normal is always on the side of ray origin, but
  ;; precalculated N.D may be negative:
  ;;
  ;; R = Rx + Ry, D = Dx + Dy
  ;; => D = Rx - Ry <=> Rx = D + Ry
  ;;
  ;; |Ry| = |N.D| => Ry = N*|N.D|
  ;;
  ;; => R = D + 2*Ry = D + 2*N*|N.D|
  ;;
  (once-only ((ray incident-ray))
    `(with-ray (,reflected
                :origin ,point
                :direction (reflected-ray-direction ,normal ,dot-product ,ray)
                :weight (* (ray-weight ,ray) ,specular)
                :depth (1+ (ray-depth ,ray))
                :environment (ray-environment ,ray))
       (note-reflected-ray ,counters)
       ,@forms)))

(defmacro with-rel-ior (((rel-ior new-env) &key dot-product environment local-ior) &body forms)
  (with-gensyms (tmp (thunk "CALL-WITH-REL-IOR"))
    (once-only ((env environment)
                (ior local-ior))
      `(flet ((,thunk (,rel-ior ,new-env)
                ,@forms))
         (if (plusp ,dot-product)
             (let ((,tmp (or (environment-link ,env) *global-environment*)))
               (,thunk  (/ ,ior (environment-ior ,tmp)) ,tmp))
             (let ((,tmp (make-environment :ior ,ior :link ,env)))
               (,thunk (/ (environment-ior ,env) ,ior) ,tmp)))))))

(defmacro with-spawned-rays (((reflected refracted) &key point normal dot-product incident-ray
                              specular transmit ior counters)
                             &body forms)
  (with-gensyms ((thunk "CALL-WITH-SPAWNED-RAYS")
                 rel-ior new cs2 r1 r2)
    (once-only ((ray incident-ray)
                (dot dot-product)
                ior point normal)
      `(flet ((,thunk (,reflected ,refracted)
                ,@forms))
         (with-rel-ior ((,rel-ior ,new) :dot-product ,dot :local-ior ,ior :environment (ray-environment ,ray))
           ;; FIXME: Add derivation of the refraction formula as a comment.
           (let ((,cs2 (- 1.0 (* (square ,rel-ior) (- 1.0 (square ,dot))))))
             (with-reflected-ray (,r1 :point ,point :normal ,normal :dot-product ,dot
                                      :incident-ray ,ray :specular ,specular :counters ,counters)
               (if (significantp ,cs2)
                   ;; normal case
                   (with-ray (,r2 :origin ,point
                                  :direction (refracted-ray-direction ,normal ,dot ,ray ,rel-ior ,cs2)
                                  :weight (* (ray-weight ,ray) ,transmit)
                                  :depth (1+ (ray-depth ,ray))
                                  :environment ,new)
                     (note-refracted-ray counters)
                     (,thunk ,r1 ,r2))
                   ;; total internal reflection
                   ;; FIXME: the second dummy ray here seems like an ugly hack
                   (,thunk ,r1 (load-time-value
                                (make-ray :origin +origin+ :direction +x+
                                          :weight 0.0)))))))))))

(defmacro with-refracted-ray ((ray &key point normal dot-product incident-ray
                                   transmit ior counters)
                              &body body)
  (once-only (incident-ray dot-product ior point normal transmit)
    `(flet ((refraction-thunk (,ray) ,@body))
       (with-rel-ior ((rel-ior new)
                      :dot-product ,dot-product :local-ior ,ior
                      :environment (ray-environment ,incident-ray))
         ;; FIXME: Add derivation of the refraction formula as a comment.
         (let ((cs2 (- 1.0 (* (square rel-ior) (- 1.0 (square ,dot-product)))))
               (new-dir (alloc-vec)))
           (declare (dynamic-extent new-dir))
           ;; Don't execute the body for total internal reflection.
           (when (significantp cs2)
             (with-ray (refracted :origin ,point
                                  :direction (%refracted-ray-direction new-dir ,normal ,dot-product ,ray rel-ior cs2)
                                  :weight (* (ray-weight ,ray) ,transmit)
                                  :depth (1+ (ray-depth ,ray))
                                  :environment new)
               (note-refracted-ray ,counters)
               (refraction-thunk refracted))))))))