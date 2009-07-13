;;;; Reflection and refraction, but no local model.

(in-package :raylisp)

(defclass raytrace-shader (shader specular transmit)
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
           (lambda (obj point normal n.d ray counters)
             (declare (optimize speed) (float n.d) (type vec point normal) (ray ray)
                      (ignore obj))
             (if (weak-ray-p ray scene)
                 black
                 (with-spawned-rays ((reflected refracted)
                                     :point point :normal normal :dot-product n.d
                                     :incident-ray ray :specular specular :transmit transmit
                                     :ior ior :counters counters)
                   (vec+ (raytrace reflected scene counters)
                         (raytrace refracted scene counters))))))
          ((plusp specular)
           (lambda (obj point normal n.d ray counters)
             (declare (optimize speed)
                      (ignore obj))
             (if (weak-ray-p ray scene)
                 black
                 (with-reflected-ray (ray :point point :normal normal :dot-product n.d
                                          :incident-ray ray :specular specular :counters counters)
                   (raytrace ray scene counters)))))
          (t
           (warn "Bogus specular and transmit components, ~
                  ignoring RAYTRACE-SHADER.")
           (constantly black)))))

(defmethod compute-shader-function ((shader raytrace-shader) object scene transform)
  (compute-raytrace-shader-function shader object scene transform))

