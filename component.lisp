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

;;; NOISE-SHADER

(defclass noise-shader (shader)
  ((start :initarg :start :reader start-of)
   (end :initarg :end :reader end-of)
   (scale :initarg :scale :reader scale-of)))

(defmethod compute-shader-function ((shader noise-shader) object scene transform)
  (let ((start (compile-shader (start-of shader) object scene transform))
        (end (compile-shader (end-of shader) object scene transform))
        (scale (/ 1.0 (scale-of shader))))
    (declare (optimize speed))
    (lambda (obj point normal n.d ray counters)
      (let* ((v (vec* point scale))
             (noise (vector-noise v))
             (start-color (funcall start obj point normal n.d ray counters))
             (end-color (funcall end obj point normal n.d ray counters)))
        (declare (dynamic-extent v))
        (%vec-lerp v start-color end-color (clamp noise 0.0 1.0))))))

;;;## Checker Shader
;;;
;;; A checker pattern of two different shaders, ODD and EVEN.

(defclass checker (shader)
  ((odd :initarg :odd :accessor odd-of)
   (even :initarg :even :accessor even-of)
   (scale :initform 1 :initarg :scale :accessor scale-of)))

(defun checkerp (point scale)
  (declare (type vec point) (float scale)
           (optimize speed))
  (macrolet ((dim (n)
               `(ifloor (+ epsilon (aref point ,n)) scale)))
    (oddp (+ (dim 0) (dim 1) (dim 2)))))

(defmethod compute-shader-function ((shader checker) object scene transform)
  (let* ((t1 (matrix* transform (transform-of shader)))
         (inverse (inverse-matrix (matrix* t1 (transform-of object))))
         (odd (compile-shader (odd-of shader) object scene t1))
         (even (compile-shader (even-of shader) object scene t1))
         (scale (float (scale-of shader))))
    (sb-int:named-lambda shade-checher (obj point normal dot ray counters)
      (declare (optimize speed))
      (let ((p2 (transform-point point inverse)))
        (declare (dynamic-extent p2))
        (funcall (if (checkerp p2 scale)
                     odd
                     even)
                 obj
                 point
                 normal
                 dot
                 ray
                 counters)))))

;;;## Composite Shader
;;;
;;; Combines arbitrary shaders

(defclass composite (shader)
  ((shaders :initarg :shaders :accessor shaders-of)))

(defmethod compute-shader-function ((shader composite) object scene transform)
  (let* ((functions (mapcar (lambda (part)
                              (the function (compile-shader part object scene transform)))
			    (shaders-of shader)))
	 (count (float (length functions))))
    (sb-int:named-lambda shade-composite (obj point normal dot ray counters)
      (declare (optimize speed))
      (let ((result (alloc-vec)))
        (dolist (fun functions)
          (declare (function fun))
          (%vec+ result result
                 (funcall fun obj point normal dot ray counters)))
        (%vec/ result result count)))))
