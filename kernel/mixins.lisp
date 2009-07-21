(in-package :raylisp)

;;;## General Purpose Mixins
;;;
;;; Used by shaders, patterns, lights, and objects.

(defclass transform-mixin ()
  ((%transform
    :initform (identity-matrix)
    :initarg :transform)))

(defun ensure-transform (spec)
  (etypecase spec
    (cons (apply #'matrix* (reverse spec)))
    (matrix spec)
    (null (identity-matrix))))

(defmethod transform-of ((obj transform-mixin))
  (ensure-transform (slot-value obj '%transform)))

(defclass axis-mixin ()
  ((axis
    :initform z-axis
    :initarg :axis
    :reader axis-of)))

(defclass light-group-mixin ()
  ((light-group
    :initform :global
    :initarg :light-group
    :reader light-group-of)))

(defclass color-mixin ()
  ((color
    :initform white
    :initarg :color
    :accessor color-of)))

(defclass location-mixin ()
  ((location
    :initform +origin+
    :initarg :location
    :accessor location-of)))

(defclass direction-mixin ()
  ((direction
    :initform z-axis :initarg :direction
    :accessor direction-of)))

;;;## Shader Mixins
;;;
;;; Provide slots and SHADER-WEIGHT methods for the slots.

(defgeneric shader-weight (shader)
  (:method-combination +))

(defclass specular-shader-mixin ()
  ((specular
    :initform (find-default :specular 'real)
    :initarg :specular :accessor specular-of)))

(defmethod specular-of :around ((obj specular-shader-mixin))
  (coerce (call-next-method) 'single-float))

(defmethod shader-weight + ((shader specular-shader-mixin))
  (specular-of shader))

(defclass diffuse-shader-mixin ()
  ((diffuse :initform (find-default :diffuse 'real)
            :initarg :diffuse :accessor diffuse-of)))

(defmethod diffuse-of :around ((obj diffuse-shader-mixin))
  (coerce (call-next-method) 'single-float))

(defmethod shader-weight + ((shader diffuse-shader-mixin))
  (diffuse-of shader))

(defclass transmit-shader-mixin ()
  ((transmit
    :initform (find-default :transmit 'real)
    :initarg :transmit :accessor transmit-of)))

(defmethod transmit-of :around ((obj transmit-shader-mixin))
  (coerce (call-next-method) 'single-float))

(defmethod shader-weight + ((shader transmit-shader-mixin))
  (transmit-of shader))

(defclass ambient-shader-mixin ()
  ((ambient
    :initform (find-default :ambient 'real)
    :initarg :ambient :accessor ambient-of)))

(defmethod ambient-of :around ((obj ambient-shader-mixin))
  (coerce (call-next-method) 'single-float))

(defmethod shader-weight + ((shader ambient-shader-mixin))
  (ambient-of shader))
