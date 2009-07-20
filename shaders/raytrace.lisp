;;;; Reflection and refraction, but no local model.

(in-package :raylisp)

(defclass raytrace-shader (shader specular-shader-mixin transmit-shader-mixin)
  ((ior :initform (find-default :ior '(float 0.0)) :initarg :ior :accessor ior-of)))

(declaim (inline weak-ray-p))
(defun weak-ray-p (ray scene)
  (declare (ray ray) (scene scene))
  (or (= (ray-depth ray) (scene-depth-limit scene))
      (< (ray-weight ray) (scene-adaptive-limit scene))))

(defun compute-raytrace-shader-function (shader object scene transform)
  (declare (scene scene))
  (let* ((specular (coefficient (specular-of shader) shader))
	 (transmit (coefficient (transmit-of shader) shader))
	 (ior (ior-of shader)))
    (declare (float specular transmit ior))
    (cond ((plusp transmit)
           (shader-lambda shade-with-2-rays (result point normal n.d ray counters)
             (declare (optimize speed) (float n.d) (type vec point normal) (ray ray))
             (if (weak-ray-p ray scene)
                 black
                 (with-spawned-rays ((reflected refracted)
                                     :point point :normal normal :dot-product n.d
                                     :incident-ray ray :specular specular :transmit transmit
                                     :ior ior :counters counters)
                   (let ((refracted-color (alloc-vec))
                         (reflected-color (raytrace result reflected scene counters)))
                     (declare (dynamic-extent refracted-color))
                     (setf refracted-color (raytrace refracted-color refracted scene counters))
                     (%vec+ result reflected-color refracted-color))))))
          ((plusp specular)
           (shader-lambda shade-with-1-ray (result point normal n.d ray counters)
             (declare (optimize speed))
             (if (weak-ray-p ray scene)
                 black
                 (with-reflected-ray (ray :point point :normal normal :dot-product n.d
                                          :incident-ray ray :specular specular :counters counters)
                   (raytrace result ray scene counters)))))
          (t
           (warn "Bogus specular and transmit components, ~
                  ignoring RAYTRACE-SHADER.")
           (constant-shader-function black)))))

(defmethod compute-shader-function ((shader raytrace-shader) object scene transform)
  (compute-raytrace-shader-function shader object scene transform))

