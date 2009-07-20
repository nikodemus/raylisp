;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :raylisp)

;;;; TEXTURE SHADER
;;;;
;;;; Combines
;;;;
;;;;   pigment
;;;;   metallic-highlights 0-1
;;;;   specular 0-1
;;;;   roughness 0-1
;;;;
;;;; 1. Compute pigment at point.
;;;; 2. Compute normal at point.
;;;; 3. Collect light according to surface properties.

(defclass texture-shader (shader diffuse-shader-mixin)
  ((pigment
    :initarg :pigment
    :initform white
    :reader pigment-of)
   (metallic
    :initarg :metallic
    :initform nil
    :reader metallic-p)
   (brilliance
    :initarg :brilliance
    :initform 1.0
    :reader brilliance-of)
   (specular
    :initarg :specular
    :initform 0.0
    :reader specular-of)
   (roughness
    :initarg :roughness
    :initform 0.05
    :reader roughness-of)
   (reflection
    :initarg :reflection
    :initform 0.0
    :reader reflection-of)
   (fresnel
    :initarg :fresnel
    :initform 0.8
    :reader fresnel-of)))

(defmethod compute-shader-function ((shader texture-shader) object scene transform)
  (let* (#+nil (inverse (inverse-matrix transform))
         #+nil
         (pigment-fun (compute-pigment-function (pigment-of shader)))
         (pigment-fun (pattern-function (pigment-of shader) transform))
         #+nil
         (normal-fun (compute-pertubation-function (normal-of shader)))
         (light-group (compute-light-group object scene))
         (d-co (the (single-float 0.0 1.0) (diffuse-of shader)))
         (brilliance (the (single-float 1.0) (brilliance-of shader)))
         (specular (specular-of shader))
         (1/roughness (/ 1.0 (roughness-of shader)))
         (metallic (metallic-p shader))
         (reflection (reflection-of shader))
         (fresnel (fresnel-of shader))
         (1-fresnel (- 1.0 fresnel)))
    (declare (function pigment-fun #+nil normal-fun)
             (single-float d-co specular reflection 1/roughness fresnel 1-fresnel))
    (shader-lambda shade-texturre (point normal n.d ray counters)
      (declare (optimize speed))
      (let* ((point2 point #+nil (transform-point point inverse))
             (pigment (funcall pigment-fun point2))
             (normal2 #+nil (funcall normal-fun point2) normal)
             (result (alloc-vec)))
        (declare (dynamic-extent #+nil point2 #+nil normal2))
        ;; For all lights...
        (dolist (light (light-group-lights light-group))
          (let* ((lv (light-vector light point))
                 (lvn (normalize lv))
                 (l.n (dot-product lvn normal2)))
            (declare (dynamic-extent lvn))
            (when (plusp l.n)
              (multiple-value-bind (incident len) (illuminate light point lv counters)
                (when (plusp len)
                  ;; Diffuse color
                  (let* ((diffuse-factor (if (= 1.0 brilliance)
                                             (* d-co l.n)
                                             (expt (* d-co l.n) brilliance)))
                         (diffuse (hadamard-product (vec* pigment diffuse-factor) incident)))
                    (declare (dynamic-extent diffuse))
                    (%vec+ result result diffuse))
                  ;; Specular highlight
                  (let* ((eye->light (vec- lvn (ray-direction ray)))
                         (h (vec/ eye->light (sqrt (the (single-float 0.0)
                                                     (dot-product eye->light eye->light)))))
                         (h.n (dot-product h normal2)))
                    (declare (dynamic-extent eye->light h))
                    (when (plusp h.n)
                      (let ((hilite (vec* (if metallic pigment incident)
                                          (* specular (expt h.n 1/roughness)))))
                        (declare (dynamic-extent hilite))
                        #+nil
                        (break "s=~S, 1/r = ~S, exp = ~S, p = ~s, c = ~S"
                               specular
                               1/roughness
                               (expt h.n 1/roughness)
                               (* specular (expt h.n 1/roughness))
                               hilite)
                        (%vec+ result result hilite)))))))))
        ;; Specular reflection
        (when (plusp reflection)
          (unless (weak-ray-p ray scene)
            (let ((weight (if metallic
                              (* reflection (+ fresnel (* 1-fresnel (power (- 1.0 (abs n.d)) 5))))
                              reflection)))
              (with-reflected-ray (ray :point point :normal normal :dot-product n.d
                                       :incident-ray ray :specular weight :counters counters)
                (let ((reflected (raytrace ray scene counters)))
                  (if metallic
                      (let ((metallic-reflection (hadamard-product pigment reflected)))
                        (declare (dynamic-extent metallic-reflection))
                        (%vec+ result result metallic-reflection))
                      (%vec+ result result reflected)))))))
        result))))
