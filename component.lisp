(in-package :raylisp)

(defun transform-extents (a b matrix)
  ;; Naive method: transform all corners.
  ;; See http://www.ics.uci.edu/~arvo/code/TransformingBoxes.c
  ;; for a better way.
  (with-arrays (a b)
    (let (min-x min-y min-z max-x max-y max-z)
      (flet ((tran (i j k &optional init)
               (let* ((v (transform-point (vec i j k) matrix))
                      (x (aref v 0))
                      (y (aref v 1))
                      (z (aref v 2)))
                 (declare (dynamic-extent v))
                 (cond (init
                        (setf min-x x
                              min-y y
                              min-z z
                              max-x x
                              max-y y
                              max-z z))
                       (t
                        (minf min-x x)
                        (minf min-y y)
                        (minf min-z z)
                        (maxf max-x x)
                        (maxf max-y y)
                        (maxf max-z z))))))
        (tran (a 0) (a 1) (a 2) t)
        (tran (a 0) (a 1) (b 2))
        (tran (a 0) (b 1) (a 2))
        (tran (a 0) (b 1) (b 2))
        (tran (b 0) (a 1) (a 2))
        (tran (b 0) (a 1) (b 2))
        (tran (b 0) (b 1) (a 2))
        (tran (b 0) (b 1) (b 2)))
      (values (vec min-x min-y min-z)
              (vec max-x max-y max-z)))))

;;;## General Purpose Mixins
;;;
;;; Used by both shaders and lights.

(defclass color-mixin ()
  ((color
    :initform (find-default :color 'vec) :initarg :color
    :accessor color-of)))

(defclass location-mixin ()
  ((location
    :initform (find-default :location 'vec) :initarg :location
    :accessor location-of)))

(defclass direction-mixin ()
  ((direction
    :initform (find-default :direction 'vec) :initarg :direction
    :accessor direction-of)))

;;;## Lights

;;;## Shaders
;;;
;;;### Specular Shader
;;;
;;; Mixin: provides no behaviour, only the specular slot and weight.

(defclass specular ()
  ((specular
    :initform (find-default :specular '(float 0.0 1.0))
    :initarg :specular :accessor specular-of)))

(defmethod shader-weight + ((shader specular))
  (specular-of shader))

;;;### Diffuse Shader
;;;
;;; Mixin: provides no behaviour, only the diffuse slot and weight.

(defclass diffuse ()
  ((diffuse :initform 0.9 :initarg :diffuse :accessor diffuse-of)))

(defmethod shader-weight + ((shader diffuse))
  (diffuse-of shader))

;;;### Transmit Shader
;;;
;;; Mixin: provides no behaviour, only the transmit slot.

(defclass transmit ()
  ((transmit
    :initform (find-default :transmit '(float 0.0 1.0))
    :initarg :transmit :accessor transmit-of)))

(defmethod shader-weight + ((shader transmit))
  (transmit-of shader))

;;;### Ambient Shader
;;;
;;; Mixin: provides no behaviour, only the ambient slot.

(defclass ambient ()
  ((ambient
    :initform (find-default :ambient '(float 0.0 1.0))
    :initarg :ambient :accessor ambient-of)))

(defmethod shader-weight + ((shader ambient))
  (ambient-of shader))
