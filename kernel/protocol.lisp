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

(defgeneric compute-object-properties (scene-object scene transform &key shading-object))

(defgeneric compute-object-extents (scene-object transform))

(defmethod compute-object-extents ((object scene-object) transform)
  nil)

(defun compile-scene-object (object scene transform &key shading-object)
  (let ((m (if shading-object
               transform
               (matrix* transform (transform-of object)))))
    (destructuring-bind (&key intersection normal)
        (compute-object-properties object scene m :shading-object shading-object)
     (assert normal)
     (let ((shader (compile-shader (shader-of object) (or shading-object object) scene m)))
       (if shading-object
           (make-shading-object
            :normal normal
            :shader shader)
           (multiple-value-bind (min max) (compute-object-extents object m)
             (assert intersection)
             (make-intersection-object
              :intersection intersection
              :normal normal
              :shader shader
              :min min :max max
              :scene-object object)))))))

(declaim (inline intersect))
(defun intersect (object ray counters shadow)
  (multiple-value-bind (hitp x)
      (funcall (object-intersection object) ray)
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

(defun shadow-function (light location scene)
  (declare (ignore location))
  (check-type scene scene)
  (if (fill-light-p light)
      (constantly nil)
      (with-arrays (location)
        (let ( ;; No real light buffers yet: just a single shadow object cache.
              (last nil))
          (lambda (point nlv len counters)
            (declare (type vec point nlv) (type float len)
                     (optimize speed))
            (with-ray (ray :origin point :direction nlv :extent len)
              (when (or (and last (intersect last ray counters t))
                        (let ((int (find-scene-intersection ray scene counters t)))
                          (when int
                            (setf last int))))
                t)))))))

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
