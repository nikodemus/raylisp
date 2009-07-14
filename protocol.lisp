(in-package :raylisp)

;;;# Scene Protocols
;;;
;;;## Object Protocols
;;;
;;; In order to participate in CSG operations they must also implement
;;; the CSG protocol. In arder to take advantage of optimizations
;;; based on spatial hierarchies they should also implement the
;;; bounding protocol if possible.
;;;

(defgeneric compute-object-properties (scene-object scene transform))

(defgeneric compute-object-extents (scene-object transform))

(defmethod compute-object-extents ((object scene-object) transform)
  nil)

(defun compile-scene-object (object scene transform)
  (destructuring-bind (&key intersection normal)
      (compute-object-properties object scene transform)
    (assert (and intersection normal))
    (multiple-value-bind (min max) (compute-object-extents object transform)
      (make-compiled-object
       :intersection intersection
       :normal normal
       :shader (compile-shader (shader-of object) object scene transform)
       :min min :max max
       :name (name-of object)))))

(declaim (inline intersect))
(defun intersect (object ray counters shadow)
  (multiple-value-bind (hitp x)
      (funcall (object-intersection object) ray)
    (declare (type boolean hitp) (type (or null compiled-object) x))
    (note-intersection counters shadow hitp)
    (when hitp
      (if shadow
          object
          (if x x object)))))

(defgeneric compute-light-properties (light scene))

(defun compile-scene-light (light scene)
  (destructuring-bind (&key incident-light illumination)
      (compute-light-properties light scene)
    (assert (and incident-light illumination))
    (make-compiled-light
     :direction incident-light
     :illumination illumination)))

(declaim (inline light-vector illuminate))

(defun light-vector (light point)
  (funcall (light-direction light) point))

(defun illuminate (light point light-vector counters)
  (funcall (light-illumination light) point light-vector counters))

;;;### Light Buffers
;;;
;;; Lights do not need to, but they can take advantage of the built-in
;;; support for light-buffers by obtaining an optimized shadow-casting
;;; function during compilation for the relevant location:
;;;
;;; TODO: Actually implement light buffers: sort scene into six groups:
;;; bounding box in +x, -x, +y, -y, +z, and -z direction from light.
;;; When casting shadows test only objects in the gross direction of the
;;; point from location, and place the shadowing object first in the
;;; group.

(defun shadow-function (location scene)
  (declare (ignore location))
  (check-type scene scene)
  (with-arrays (location)
    (let (;; No real light buffers yet: just a single shadow object cache.
	  (last nil))
      (declare (type (or null compiled-object) last))
      (lambda (point nlv len counters)
	(declare (type vec point nlv) (type float len)
		 (optimize speed))
	(with-ray (ray :origin point :direction nlv :extent len)
	  (when (or (and last (intersect last ray counters t))
                    (let ((int (find-scene-intersection ray scene counters t)))
                      (when int
                        (setf last int))))
            t))))))

;;;## Shader protocol
;;;
;;; Shader protocol controls the compilation of SHADER instances into
;;; a more efficient representation used for rendering. There must be
;;; an applicable method on COMPUTE-SHADER-FUNCTION for each subclass of
;;; SHADER that returns the corresponding shader function:
;;;
;;;#### The shader function
;;;
;;; must accept an INTERSECTION and a RAY, and return the apparent color.

(defgeneric compute-shader-function (shader object scene transform))

(declaim (ftype (function (t t t t) (values (function (compiled-object vec vec float ray t)
                                                  (values vec &optional))
                                        &optional))
                compile-shader))
(defun compile-shader (shader object scene transform)
  (compute-shader-function shader object scene transform))

(defmethod compute-shader-function ((null null) object scene transform)
  (constantly black))

(defgeneric shader-weight (shader)
  (:method-combination +))

(declaim (inline coefficient))
(defun coefficient (value shader)
  (declare (float value))
  (/ value (the float (shader-weight shader))))

(declaim (inline shade))
(defun shade (object ray counters)
  (declare (optimize speed))
  (let* ((point (adjust-vec (ray-origin ray) (ray-direction ray)
                            (ray-extent ray)))
	 (normal (funcall (object-normal object) point))
	 (n.d (dot-product normal (ray-direction ray))))
    (flet ((%shade (n)
             (funcall (object-shader object)
                      object
                      point
                      n
                      n.d
                      ray
                      counters)))
      (if (plusp n.d)
          (let ((n2 (vec* normal -1.0)))
            (declare (dynamic-extent n2))
            (%shade n2))
          (%shade normal)))))

;;;; CAMERA

(defclass camera (name-mixin)
  ((location :accessor location-of)
   (direction :accessor direction-of)
   (up :accessor up-of)
   (right :accessor right-of)))

(defmethod print-object ((camera camera) stream)
  (print-unreadable-object (camera stream :type t)
    (format stream "loc: ~S dir: ~S right: ~S up: ~S"
            (location-of camera) (direction-of camera)
            (right-of camera) (up-of camera)))
  camera)

(defgeneric compute-camera-function (camera))

(defmethod initialize-instance :after ((camera camera) &key
				       look-at direction location
				       focal-length sky
                                       right up aspect-ratio)
  (when (and look-at direction)
    (error "Both :LOOK-AT and :DIRECTION given."))
  (when (and focal-length direction)
    (error "Both :DIRECTION and :FOCAL-LENGTH given."))
  (when (and (or up right) (not (and up right)))
    (error "Only one of :UP and :RIGHT given."))
  (when (and aspect-ratio up right)
    (error ":ASPECT-RATIO given in addition to :UP and :RIGHT."))
  (when (and sky up right)
    (error ":SKY given in addition to :UP and :RIGHT."))
  (unless location
    (setf location (vec 0.0 2.0 -10.0)))
  (let ((normalized-direction
         (when direction
           (normalize direction))))
    (unless direction
      (unless look-at
        (setf look-at +origin+))
      (unless focal-length
        (setf focal-length 1.0))
      (setf normalized-direction (normalize (vec- look-at location))
            direction (vec* normalized-direction focal-length)))
    (unless (and up right)
      (unless sky
        (if (= 0 (aref direction 0) (aref direction 2))
            (setf sky z-axis)
            (setf sky y-axis)))
      (unless aspect-ratio
        (setf aspect-ratio (/ 4.0 3.0)))
      (let* ((n-sky (if sky (normalize sky) y-axis))
             (n-right (cross-product n-sky normalized-direction)))
        (setf right (vec* n-right aspect-ratio)
              up (normalize (cross-product normalized-direction n-right))))))
  (setf (location-of camera) location
        (direction-of camera) direction
        (right-of camera) right
        (up-of camera) up))

(defgeneric normalize-camera (camera width height))

(defmethod normalize-camera ((camera camera) width height)
  (make-instance (class-of camera)
                 :up (normalize (up-of camera))
                 :right (normalize (right-of camera))
                 :direction (direction-of camera)
                 :location (location-of camera)))

;;;; COLORS AND PATTERNS
;;;;
;;;; FIXME: Instead of MAKE-PATTERN taking a function as the first argument,
;;;; it should take a "description":
;;;;
;;;; eg. (make-pattern `(gradient :transform (rotate* 0.0 ,rot 0.0))
;;;;                   `((0.0 ,black)
;;;;                     (1.0 ,white)))
;;;; ...maybe.
;;;;
;;;; In any case, the pattern should be responsible for applying any transformations
;;;; as well.
;;;;
;;;; It might also be worthwhie to have a macro
;;;;
;;;; (with-color-function (color pattern-or-color) ...body..)
;;;;
;;;; which expands into
;;;;
;;;; (ecase pattern-or-color
;;;;  (vec (flet (((color (point) pattern-or-color)) ...body...)))
;;;;  (pattern (let ((f (compute-color-function pattern-or-color))) (flet ((color (point) (funcall f))) ...body...))))
;;;;
;;;; so that solid colors don't pay extra.

(defstruct pattern
  (function (required-argument :function) :type (function (vec) single-float))
  (map (required-argument :map) :type list))

(defgeneric compute-color-function (color brightness transform))

(defmethod compute-color-function ((color #.(class-of (alloc-vec))) bightness transform)
  (constantly color))

(defmethod compute-color-function ((pattern pattern) brightness transform)
  (let* ((pattern-function (pattern-function pattern))
         (inverse (inverse-matrix transform))
         (map (pattern-map pattern))
         (map-size (length map))
         (colors (make-array (length map)))
         (values (make-array (length map) :element-type 'single-float))
         (last -1.0)
         (p 0))
    (unless map
      (error "Pattern's map is empty."))
    ;; Build the map and verify it.
    (dolist (elt map)
      (destructuring-bind (value color) elt
        (cond ((and (= p 0) (/= 0.0 value))
               (error "Pattern's map does not start at 0.0:~%  ~S" map))
              ((and (= p (1- map-size)) (/= 1.0 value))
               (error "Pattern's map does not end at 1.0:~%  ~S" map))
              ((>= last value)
               (error "Pattern's map is not strictly increasing:~%  ~S" map)))
        (setf (aref values p) value
              (aref colors p) (vec* color brightness)
              last value)
        (incf p)))
    (sb-int:named-lambda color-function (point)
      (declare (optimize speed))
      (block color-function
        (let* ((point2 (transform-point point inverse))
               (value (funcall pattern-function point2))
               (tmp 0.0))
          (declare (dynamic-extent point2) (single-float value tmp))
          ;; FIXME: Linear search is not so good with big maps!
          (let* ((index (loop for i from 0 below map-size
                              do (let ((this (aref values i)))
                                   (cond ((= value this)
                                          ;; Exact hit, no need to compute anything else.
                                          (return-from color-function (aref colors i)))
                                         ((< tmp value this)
                                          (setf tmp (- 1.0 (/ (- this value) (- this tmp))))
                                          (return i))
                                         (t
                                          (setf tmp this)))))))
            (vec-lerp (aref colors (- index 1)) (aref colors index) tmp)))))))

(defun gradient-pattern (point)
  (imod (* (aref point 0) 0.3) 1.0))
