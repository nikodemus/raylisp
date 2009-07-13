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
