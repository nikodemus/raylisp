(in-package :raylisp)

;;;; TRANSFORMABLE
;;;;
;;;; Transformable objects have a MATRIX slot, and deal specially with
;;;; initargs :ROTATE, :TRANSLATE, :SCALE, :MATRIX, and :MATRIX-LIST

(defclass transformable ()
  ((%matrix)))

(defun parse-transform-arguments (plist)
  (let ((matrix +identity-matrix+)
        (rotatep nil)
        (translatep nil)
        (scalep nil)
        (matrixp nil)
        (matrix-listp nil))
    (flet ((apply-transform (m)
             (setf matrix (matrix* m matrix))))
      (doplist (key val plist)
        (case key
          (:rotate
           (if rotatep
               (warn "Ignoring secondary rotation: ~S" val)
               (apply-transform (rotate val)))
           (setf rotatep t))
          (:translate
           (if translatep
               (warn "Ignoring secondary translation: ~S" val)
               (apply-transform (translate val)))
           (setf translatep t))
          (:scale
           (if scalep
               (warn "Ignoring secondary scaling: ~S" val)
               (let ((scale (if (realp val)
                                (scale (v val val val))
                                (scale val))))
                 (apply-transform scale)))
           (setf scalep t))
          (:matrix
           (if matrixp
               (warn "Ignoring secondary matrix:~%   ~A" val)
               (apply-transform val))
           (setf matrixp t))
          (:matrix-list
           (if matrix-listp
               (warn "Ignoring secondary matrix list:~%  ~A" val)
               (if (cdr val)
                   (apply-transform (apply #'matrix* (reverse val)))
                   (apply-transform (car val))))
           (setf matrix-listp t))
          (%matrix
           (error "Unexpected ~S keyword argument: ~S" key val)))))
    matrix))

(defmethod initialize-instance :before ((obj transformable) &rest initargs
                                        &key rotate translate scale matrix matrix-list)
  (declare (ignore rotate translate scale matrix matrix-list))
  (setf (slot-value obj '%matrix) (parse-transform-arguments initargs)))

(defmethod transform-of ((obj transformable))
  (slot-value obj '%matrix))

;;;## General Purpose Mixins
;;;
;;; Used by shaders, patterns, lights, and objects.

(defclass axis-mixin ()
  ((axis
    :initform +z+
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
    :initform +z+ :initarg :direction
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
