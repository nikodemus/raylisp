(in-package :raylisp)

;;;# Built-in Scene Components
;;;
;;;## Scene-Objects
;;;
;;;### Sphere

(defclass sphere (scene-object)
  ((radius 
    :initform (find-default :radius 'float) :initarg :radius :reader radius-of)
   (location 
    :initform (find-default :location 'vector) :initarg :location 
    :reader location-of)))

(defun sphere-matrix (sphere)
  (let ((r (radius-of sphere)))
    (matrix-product (translate (location-of sphere))
		    (scale* r r r)
		    (transform-of sphere))))

(defmethod compute-object-properties ((sphere sphere) scene)
  (multiple-value-bind (inverse adjunct/inverse)
      (inverse-and-adjunct/inverse-matrix (sphere-matrix sphere))
    (list
     :intersection
     (lambda (ray)
       (let-values (((ox oy oz)
                     (transform-vector-values (ray-origin ray) inverse))
                    ((dx dy dz)
                     (transform-direction-values (ray-direction ray) inverse)))
         (let ((s (min-pos-quad-root (+ (square dx) (square dy) (square dz))
                                     (* 2.0 (dot-product* dx dy dz ox oy oz))
                                     (- (+ (square ox) (square oy) (square oz))
                                        1.0))))
           ;; FIXME: If we need to follow this pattern elsewhere, then there is no
           ;; need to check against epsilon in MIN-POS-QUAD-ROOT.
           (when (< epsilon s (ray-extent ray))
             (setf (ray-extent ray) s)
             t))))
     :normal
     (lambda (point)
       (transform/normalize-vector point adjunct/inverse)))))

(defmethod compute-object-extents ((sphere sphere))
  (let ((matrix (sphere-matrix sphere)))
    (values (transform-vector (vector -1.0 -1.0 -1.0) matrix)
	    (transform-vector (vector 1.0 1.0 1.0) matrix))))

(defmethod compute-csg-properties ((sphere sphere) scene)
  (let* ((inverse (inverse-matrix (sphere-matrix sphere)))
	 (compiled (compile-scene-object sphere scene)))
    (list
     :all-intersections
     (lambda (origin direction)
       ;; FIXME: for some reason SBCL gives a lot for float to pointer
       ;; notes for these -values calls, and I don't understand why.
       ;; Possibly register exhaustion? Need to benchmark, and see if
       ;; packing the results into vectors that will be throws away is
       ;; faster...
       (let-values (((ox oy oz) (transform-vector-values origin inverse))
                    ((dx dy dz) (transform-direction-values direction inverse)))
	 (map 'simple-vector
	      (lambda (d)
		(make-csg-intersection :distance d :object compiled))
	      (pos-quad-roots (dot-product* dx dy dz
                                            dx dy dz)
                              (* 2.0 (dot-product* dx dy dz
                                                   ox oy oz))
                              (- (dot-product* ox oy oz
                                               ox oy oz)
                                 1.0)))))
     :inside
     (lambda (point)
       (> (+ 1.0 epsilon)
	  (vector-length (transform-vector point inverse)))))))

;;;### Plane

(defclass plane (scene-object) 
  ((normal :initform y-axis :initarg :normal :accessor normal-of)
   (location :initform origin :initarg :location :accessor location-of)))

(defun plane-matrix (plane)
  (matrix-product (transform-of plane)
		  (translate (location-of plane))
		  (reorient y-axis (normal-of plane))))

(defmethod compute-object-properties ((plane plane) scene)
  (multiple-value-bind (inverse adjunct)
      (inverse-and-adjunct-matrix (plane-matrix plane))
    (list
     :intersection
     (lambda (ray)
       (let* ((dy (aref (transform-direction (ray-direction ray) inverse) 1))
	      (s (if (zerop dy)
		     -1.0 ; parallel
		     (- (/ (aref (transform-vector (ray-origin ray) inverse) 1)
			   dy)))))
	 (when (< epsilon s (ray-extent ray))
	   (setf (ray-extent ray) s)
	   t)))
     :normal
     (constantly (transform/normalize-vector y-axis adjunct)))))

(defmethod compute-csg-properties ((plane plane) scene)
  (let ((inverse (inverse-matrix (plane-matrix plane)))
	(c-object (compile-scene-object plane scene)))
    (list
     :all-intersections
     (lambda (origin direction)
       (let ((d (let ((dy (aref (transform-direction direction inverse) 1)))
		  (if (zerop dy)
		      -1.0 ; parallel
		      (- (/ (aref (transform-vector origin inverse) 1) dy))))))
	 (if (significantp d)
	     (simple-vector
	      (make-csg-intersection
	       :distance d
	       :object c-object))
	     #())))
     :inside
     (lambda (point)
       (> epsilon
	  (aref (transform-vector point inverse) 1))))))

;;;## General Purpose Mixins
;;;
;;; Used by both shaders and lights.

(defclass color-mixin ()
  ((color 
    :initform (find-default :color 'vector) :initarg :color 
    :accessor color-of)))

(defclass location-mixin ()
  ((location 
    :initform (find-default :location 'vector) :initarg :location
    :accessor location-of)))

(defclass direction-mixin ()
  ((direction 
    :initform (find-default :direction 'vector) :initarg :direction 
    :accessor direction-of)))

;;;## Lights
;;;
;;;### Point Light

(defclass point-light (scene-light color-mixin location-mixin)
  ()
  (:documentation
   "Basic omnidirectional light source."))

(defmethod compute-light-properties ((light point-light) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
	 (shadow-fun (shadow-function location scene)))
    (list
     :incident-light
     (lambda (point)
       (vector-sub location point))
     :illumination
     (lambda (point light-vector counters)
       (let* ((len (vector-length light-vector))
	      (nlv (vector-div light-vector len)))
	 (if (funcall shadow-fun point nlv len counters)
	     (values black -1.0)
	     (values color len)))))))

;;;### Spotlight

(defclass spotlight (scene-light color-mixin location-mixin direction-mixin)
  ((aperture :initform 0.9 :initarg :aperture :accessor aperture-of))
  (:documentation
   "A simple spotlight. The smaller the aperture the wider the spot, 1.0
begin the maximum value. Spotlight fades towards its edges."))

(defun linear-fader (aperture)
  (declare (type (float -1.0 1.0) aperture))
  (let ((scale (/ 1.0 (- 1.0 aperture))))
    (lambda (color f)
      (declare (type vector color)
               (type float f))
      (vector-mul
       color
       (* (- f aperture) scale)))))

(defmethod compute-light-properties ((light spotlight) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
         (direction (normalize (direction-of light)))
	 (shadow-fun (shadow-function location scene))
         (aperture (coerce (aperture-of light) 'float))
         (fader (linear-fader aperture)))
    (declare (type float aperture)
             (type function fader shadow-fun)
             (type vector direction location color))
    (list
     :incident-light
     (lambda (point)
       (vector-sub location point))
     :illumination
     (lambda (point light-vector counters)
       (let* ((len (vector-length light-vector))
              (nlv (vector-div light-vector len))
              (dot (- (dot-product nlv direction))))
         (if (or (< dot aperture) (funcall shadow-fun point nlv len counters))
             (values black -1.0)
             (values (funcall fader color dot) len)))))))

;;;### Solar (Parallel) Light

(defclass solar-light (scene-light color-mixin direction-mixin)
  ()
  (:documentation
   "Solar light appears to shine from the same direction and distance
everywhere in the scene, simulating an effectively infinitely distant light
source such as the sun or moon."))

(defmethod compute-light-properties ((light solar-light) scene)
  (let ((nd (normalize (direction-of light)))
	(color (color-of light)))
    ;; I'm sure there was a reason not to use SHADOW-FUNCTION here...
    (list
     :incident-light
     (constantly nd)
     :illumination
     (lambda (point lv counters)
       (let ((ray (make-ray :origin point :direction lv)))
	 (if (find-scene-intersection ray scene counters t)
	     (values black -1.0)
	     (values color 1.0)))))))


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

;;;### Raytracing Shader
;;;
;;; Provides reflection and refraction, but no local model.

(defclass raytrace (shader specular transmit)
  ((ior :initform (find-default :ior '(float 0.0)) :initarg :ior :accessor ior-of)))

(defmethod compute-shader-function ((shader raytrace) scene)
  (declare (type scene scene))
  (let* ((specular (coefficient (specular-of shader) shader))
	 (transmit (coefficient (transmit-of shader) shader))
	 (ior (ior-of shader)))
    (flet ((weakp (ray)
             ;; Decide if the ray is too weak to trace further. This
             ;; is might conceptually cleaner in the RAYTRACE
             ;; function, but is more efficient here and weak leaves
             ;; never need to be instantiated.
             (declare (type ray ray))
             (or (= (ray-depth ray) (scene-depth-limit scene))
                 (<= (ray-weight ray) (scene-adaptive-limit scene)))))
      (cond ((plusp transmit)
             (lambda (point normal n.d ray counters)
               (if (weakp ray)
                   black
                   (multiple-value-bind (reflected refracted)
                       (spawn-rays point normal n.d ray specular transmit ior counters)
                     (vector-add (raytrace reflected scene counters)
                                 (raytrace refracted scene counters))))))
            ((plusp specular)
             (lambda (point normal n.d ray counters)
               (if (weakp ray)
                   black
                   (raytrace (reflected-ray point normal n.d ray specular counters) scene counters))))
            (t
             (warn "Bogus specular and transmit components, ~
                    ignoring RAYTRACE shader.")
             (constantly black))))))

;;;## Flat Shader
;;;
;;; Provides only the ambient component.

(defclass flat (shader color-mixin ambient)
  ())

(defmethod compute-shader-function ((shader flat) scene)
  (let ((ambient-color (hadamard-product
                        (vector-mul (scene-ambient-light scene)  
                                    (coefficient (ambient-of shader) shader))
                        (color-of shader))))
    (constantly ambient-color)))

;;;## Solid Shader
;;;
;;; Provides diffuse and ambient components.

(defclass solid (shader color-mixin ambient diffuse)
  ())

(defmethod compute-shader-function ((shader solid) scene)
  (let* ((color (color-of shader))
         (ambient-color (hadamard-product
                         (vector-mul (scene-ambient-light scene)  
                                     (coefficient (ambient-of shader) shader))
                         color))
         (diffuse-color (vector-mul
                         color
                         (coefficient (diffuse-of shader) shader))))
    (with-arrays (diffuse-color)
      (lambda (point normal dot ray counters)
        (declare (ignore ray))
	(let ((color ambient-color))
          ;; FIXME: Is there a way to store the list of lights directly here,
          ;; without going though 2 indirections each time?
	  (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
	    (let* ((lv (light-vector light point))
		   (dot (dot-product lv normal)))
	      (when (plusp dot)
		(multiple-value-bind (incident len) (illuminate light point lv counters)
		  (when (plusp len)
                    ;; nx * lx/len + ny * ly/len + nz * lz/len
                    ;; == (nx*lx + ny*ly + nz*lz)/len
		    (let ((l.n (/ dot len)))
		      (with-arrays (incident color)
			(macrolet 
			    ((dim (n)
			       `(+ (color ,n)
				   (* (incident ,n)
                                      (* (diffuse-color ,n) l.n)))))
			  (setf color (vector (dim 0) (dim 1) (dim 2)))))))))))
          color)))))

;;;## Phong Shader
;;;
;;; Provides phong highlights, diffuse and ambient components.

(defclass phong (shader color-mixin ambient diffuse specular)
  ((size :initform (find-default :size 'float) :initarg :size 
	 :accessor size-of)))
 
(defmethod compute-shader-function ((shader phong) scene)
  (let* ((color (color-of shader))
	 (ambient-color (hadamard-product
                         (vector-mul (scene-ambient-light scene)  
                                     (coefficient (ambient-of shader) shader))
			 color))
	 (diffuse-color (vector-mul color (coefficient (diffuse-of shader) shader)))
	 (specular (coefficient (specular-of shader) shader))
	 (size (size-of shader)))
    (declare (type float specular size))
    (with-arrays (diffuse-color)
      (lambda (point normal dot ray counters)
	(let ((color black)
	      (dir (ray-direction ray)))
	  (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
	    (let* ((lv (light-vector light point))
		   (dot (dot-product lv normal)))
	      (when (plusp dot)
		(multiple-value-bind (incident len) (illuminate light point lv counters)
		  (when (plusp len)
		    (let* ((l.n (/ dot len))
			   (h (normalize (vector-sub lv dir))) ; FIXME:
                                                               ; why
                                                               ; must
                                                               ; we
                                                               ; normalize?
			   (n.h^p (expt (dot-product normal h) size))
			   (s-co (* specular n.h^p)))
		      (with-arrays (incident color)
			(macrolet 
			    ((dim (n)
			       `(+ (color ,n)
				   (* (incident ,n) (+ (* (diffuse-color ,n) l.n) 
						       s-co)))))
			  (setf color (vector (dim 0) (dim 1) (dim 2)))))))))))
	  (vector-add ambient-color color))))))

;;;## Gradient Shader
;;;
;;; A gradient from shader START to END along AXIS.

(defclass gradient (shader)
  ((start :initarg :start :accessor start-of)
   (end :initarg :end :accessor end-of)
   (axis :initarg :axis :initform 1 :accessor axis-of)
   (scale :initarg :scale :initform 1.0 :accessor scale-of)
   (smooth :initarg :smooth :initform nil :accessor smoothp)))

(defmethod compute-shader-function ((shader gradient) scene)
  (let ((start (compile-shader (start-of shader) scene))
        (end (compile-shader (end-of shader) scene))
        (axis (axis-of shader))
        (scale (/ 1.0 (scale-of shader))))
    (declare (type (integer 0 2) axis)
             (type function start end)
             (type float scale))
    (if (smoothp shader)
        (lambda (point normal dot ray counters)
          (let* ((start (funcall start point normal dot ray counters))
                 (end (funcall end point normal dot ray counters))
                 (ratio (mod (* (aref point axis) scale) 2.0)))
            (if (> 1.0 ratio)
                (vector-lerp start end (* ratio 0.5))
                (vector-lerp end start (* ratio 0.5)))))
        (lambda (point normal dot ray counters)
          (let* ((start (funcall start point normal dot ray counters))
                 (end (funcall end point normal dot ray counters))
                 (ratio (mod (* (aref point axis) scale) 1.0)))
            (vector-lerp start end ratio))))))

;;; NOISE-SHADER

(defclass noise-shader (shader)
  ((start :initarg :start :reader start-of)
   (end :initarg :end :reader end-of)
   (scale :initarg :scale :reader scale-of)))

(defmethod compute-shader-function ((shader noise-shader) scene)
  (let ((start (compile-shader (start-of shader) scene))
        (end (compile-shader (end-of shader) scene))
        (scale (/ 1.0 (scale-of shader))))
    (lambda (point normal n.d ray counters)
      (let ((noise (vector-noise (vector-mul point scale)))
            (start-color (funcall start point normal n.d ray counters))
            (end-color (funcall end point normal n.d ray counters)))
        (vector-lerp start-color end-color (clamp noise 0.0 1.0))))))



;;;## Checker Shader
;;;
;;; A checker pattern of two different shaders, ODD and EVEN.

(defclass checker (shader)
  ((odd :initarg :odd :accessor odd-of)
   (even :initarg :even :accessor even-of)))

(defun checkerp (point)
  (declare (type vector point))
  (macrolet ((dim (n)
	       `(floor (+ epsilon (aref point ,n)))))
    (oddp (+ (dim 0) (dim 1) (dim 2)))))

(defmethod compute-shader-function ((shader checker) scene)
  (let ((odd (compile-shader (odd-of shader) scene))
	(even (compile-shader (even-of shader) scene)))
    (lambda (point normal dot ray counters)
      (funcall (if (checkerp point)
                   odd
                   even)
               point
               normal
               dot
               ray
               counters))))

;;;## Composite Shader
;;;
;;; Combines arbitrary shaders

(defclass composite (shader)
  ((shaders :initarg :shaders :accessor shaders-of)))

(defmethod compute-shader-function ((shader composite) scene)
  (let* ((functions (mapcar (lambda (shader) (compile-shader shader scene))
			    (shaders-of shader)))
	 (count (float (length functions))))
    (lambda (point normal dot ray counters)
      (vector-div (reduce (lambda (c f)
			    (vector-add c (funcall f point normal dot ray counters)))
			  functions
			  :initial-value black)
		  count))))

