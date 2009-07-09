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
    :initform (find-default :location 'vec) :initarg :location
    :reader location-of)))

(defun xmatrix* (&rest args)
  (apply #'matrix* (remove nil args)))

(defun sphere-matrix (sphere)
  (let ((r (radius-of sphere)))
    (xmatrix* (transform-of sphere)
              (translate (location-of sphere))
              (scale* r r r))))

(defmethod compute-object-properties ((sphere sphere) scene)
  (multiple-value-bind (inverse adjunct/inverse)
      (inverse-and-adjunct/inverse-matrix (sphere-matrix sphere))
    (list
     :intersection
     (sb-int:named-lambda sphere-intersection (ray)
       (declare (optimize speed))
       (let* ((o2 (transform-point (ray-origin ray) inverse))
              (d2 (transform-direction (ray-direction ray) inverse)))
         (declare (dynamic-extent o2 d2))
         (let ((s (min-pos-quad-root (+ (dot-product d2 d2))
                                     (* 2.0 (dot-product d2 o2))
                                     (- (dot-product o2 o2) 1.0))))
           ;; FIXME: If we need to follow this pattern elsewhere, then there is no
           ;; need to check against epsilon in MIN-POS-QUAD-ROOT.
           (when (< epsilon s (ray-extent ray))
             (setf (ray-extent ray) s)
             t))))
     :normal
     (lambda (point)
       (normalize (transform-point point adjunct/inverse))))))

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

(defmethod compute-object-extents ((sphere sphere))
  (transform-extents (vec -1.0 -1.0 -1.0)
                     (vec 1.0 1.0 1.0)
                     (sphere-matrix sphere)))

(defmethod compute-csg-properties ((sphere sphere) scene)
  (let* ((inverse (inverse-matrix (sphere-matrix sphere)))
	 (compiled (compile-scene-object sphere scene)))
    (list
     ;; FIXME: To reduce consing even further: stack allocate
     ;; the csg-interactions and pass a continuation in here.
     ;; ...alternatively, pre-allocate a csg-intersection buffer.
     :all-intersections
     (sb-int:named-lambda sphere-all-intersections (origin direction)
       (declare (optimize speed))
       (let ((o (transform-point origin inverse))
             (d (transform-direction direction inverse)))
         (declare (dynamic-extent o d))
         (multiple-value-bind (r1 r2)
             (pos-quad-roots (dot-product d d)
                             (* 2.0 (dot-product d o))
                             (- (dot-product o o) 1.0))
           (cond ((= -1.0 r1)
                  #())
                 ((= -1.0 r2)
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)))
                 (t
                  (simple-vector (make-csg-intersection :distance r1 :object compiled)
                                 (make-csg-intersection :distance r2 :object compiled)))))))
     :inside
     (lambda (point)
       (> (+ 1.0 epsilon)
          (let ((p (transform-point point inverse)))
            (declare (dynamic-extent p))
            (vec-length p)))))))

;;;### Plane

(defclass plane (scene-object)
  ((normal :initform y-axis :initarg :normal :accessor normal-of)
   (location :initform +origin+ :initarg :location :accessor location-of)))

(defun plane-matrix (plane)
  (xmatrix* (transform-of plane)
            (translate (location-of plane))
            (reorient y-axis (normal-of plane))))

(defmethod compute-object-properties ((plane plane) scene)
  (multiple-value-bind (inverse adjunct)
      (inverse-and-adjunct-matrix (plane-matrix plane))
    (list
     :intersection
     (sb-int:named-lambda plane-intersection (ray)
       (let* ((d (transform-direction (ray-direction ray) inverse))
              (dy (aref d 1))
	      (s (if (zerop dy)
		     -1.0 ; parallel
		     (let ((o (transform-point (ray-origin ray) inverse)))
                       (declare (dynamic-extent o))
                       (- (/ (aref o 1) dy))))))
         (declare (dynamic-extent d))
	 (if (< epsilon s (ray-extent ray))
             (progn
               (setf (ray-extent ray) s)
               t)
             (progn
               nil))))
     :normal
     (constantly (normalize (transform-point y-axis adjunct))))))

(defmethod compute-csg-properties ((plane plane) scene)
  (let ((inverse (inverse-matrix (plane-matrix plane)))
	(c-object (compile-scene-object plane scene)))
    (list
     :all-intersections
     (sb-int:named-lambda plane-all-intersections (origin direction)
       (let ((d (let* ((d (transform-direction direction inverse))
                       (dy (aref d 1)))
                  (declare (dynamic-extent d))
		  (if (zerop dy)
		      -1.0 ; parallel
		      (let ((o (transform-point origin inverse)))
                        (declare (dynamic-extent o))
                        (- (/ (aref o 1) dy)))))))
	 (if (significantp d)
	     (simple-vector
	      (make-csg-intersection
	       :distance d
	       :object c-object))
	     #())))
     :inside
     (lambda (point)
       (declare (optimize speed))
       (> epsilon
          (let ((p (transform-point point inverse)))
            (declare (dynamic-extent p))
            (aref p 1)))))))

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
    (declare (function shadow-fun))
    (list
     :incident-light
     (lambda (point)
       (vec- location point))
     :illumination
     (lambda (point light-vector counters)
       (declare (optimize speed))
       (let* ((len (vec-length light-vector))
	      (nlv (vec/ light-vector len)))
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
      (declare (type vec color)
               (type float f))
      (vec* color (* (- f aperture) scale)))))

(defmethod compute-light-properties ((light spotlight) scene)
  (let* ((location (location-of light))
	 (color (color-of light))
         (direction (normalize (direction-of light)))
	 (shadow-fun (shadow-function location scene))
         (aperture (coerce (aperture-of light) 'float))
         (fader (linear-fader aperture)))
    (declare (type float aperture)
             (type function fader shadow-fun)
             (type vec direction location color))
    (list
     :incident-light
     (lambda (point)
       (vec- location point))
     :illumination
     (lambda (point light-vector counters)
       (declare (optimize speed))
       (let* ((len (vec-length light-vector))
              (nlv (vec/ light-vector len))
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
       (with-ray (ray :origin point :direction lv)
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

(declaim (inline weak-ray-p))
(defun weak-ray-p (ray scene)
  (declare (ray ray) (scene scene))
  (or (= (ray-depth ray) (scene-depth-limit scene))
      (< (ray-weight ray) (scene-adaptive-limit scene))))

(defun compute-raytrace-shader-function (shader scene)
  (declare (scene scene))
  (let* ((specular (coefficient (specular-of shader) shader))
	 (transmit (coefficient (transmit-of shader) shader))
	 (ior (ior-of shader)))
    (declare (float specular transmit ior))
    (cond ((plusp transmit)
           (lambda (point normal n.d ray counters)
             (declare (optimize speed) (float n.d) (type vec point normal) (ray ray))
             (if (weak-ray-p ray scene)
                 black
                 (with-spawned-rays ((reflected refracted)
                                     :point point :normal normal :dot-product n.d
                                     :incident-ray ray :specular specular :transmit transmit
                                     :ior ior :counters counters)
                   (vec+ (raytrace reflected scene counters)
                         (raytrace refracted scene counters))))))
          ((plusp specular)
           (lambda (point normal n.d ray counters)
             (declare (optimize speed))
             (if (weak-ray-p ray scene)
                 black
                 (with-reflected-ray (ray :point point :normal normal :dot-product n.d
                                          :incident-ray ray :specular specular :counters counters)
                   (raytrace ray scene counters)))))
          (t
           (warn "Bogus specular and transmit components, ~
                    ignoring RAYTRACE shader.")
           (constantly black)))))

(defmethod compute-shader-function ((shader raytrace) scene)
  (compute-raytrace-shader-function shader scene))

;;;## Flat Shader
;;;
;;; Provides only the ambient component.

(defclass flat (shader color-mixin ambient)
  ())

(defmethod compute-shader-function ((shader flat) scene)
  (let ((ambient-color (hadamard-product
                        (vec* (scene-ambient-light scene)
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
                         (vec* (scene-ambient-light scene)
                               (coefficient (ambient-of shader) shader))
                         color))
         (diffuse-color (vec* color
                              (coefficient (diffuse-of shader) shader))))
    (declare (type vec color ambient-color diffuse-color))
    (with-arrays (diffuse-color)
      ;; DOT as argument is ignores -- is this correct?
      (sb-int:named-lambda shade-solid (point normal dot ray counters)
        (declare (optimize speed))
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
			  (setf color (vec (dim 0) (dim 1) (dim 2)))))))))))
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
                         (vec* (scene-ambient-light scene)
                               (coefficient (ambient-of shader) shader))
			 color))
	 (diffuse-color (vec* color (coefficient (diffuse-of shader) shader)))
	 (specular (coefficient (specular-of shader) shader))
	 (size (size-of shader)))
    (declare (type float specular)
             (type (single-float (0.0)) size)
             (type vec color ambient-color diffuse-color))
    (with-arrays (diffuse-color)
      ;; FIXME: dot ignored?
      (sb-int:named-lambda shade-phong (point normal dot ray counters)
        (declare (optimize speed))
	(let ((color black)
	      (dir (ray-direction ray)))
	  (dolist (light (compiled-scene-lights (scene-compiled-scene scene)))
	    (let* ((lv (light-vector light point))
		   (dot (dot-product lv normal)))
              (declare (single-float dot))
	      (when (plusp dot)
		(multiple-value-bind (incident len) (illuminate light point lv counters)
		  (when (plusp len)
		    (let* ((l.n (/ dot len))
			   (h (normalize (vec- lv dir))) ; FIXME: why must we normalize?
			   (n.h^p (expt (the (single-float 0.0) (dot-product normal h)) size))
			   (s-co (* specular n.h^p)))
		      (with-arrays (incident color)
			(macrolet
			    ((dim (n)
			       `(+ (color ,n)
				   (* (incident ,n) (+ (* (diffuse-color ,n) l.n)
						       s-co)))))
			  (setf color (vec (dim 0) (dim 1) (dim 2)))))))))))
	  (vec+ ambient-color color))))))

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
             (type float scale)
             (optimize speed))
    (if (smoothp shader)
        (sb-int:named-lambda shade-smooth-gradient (point normal dot ray counters)
          (declare (optimize speed))
          (declare (type vec point))
          (let* ((start (funcall start point normal dot ray counters))
                 (end (funcall end point normal dot ray counters))
                 (ratio (imod (* (aref point axis) scale) 2.0)))
            (declare (type vec start end))
            (if (> 1.0 ratio)
                (vec-lerp start end (* ratio 0.5))
                (vec-lerp end start (* ratio 0.5)))))
        (sb-int:named-lambda shade-gradient (point normal dot ray counters)
          (declare (optimize speed))
          (declare (type vec point))
          (let* ((start (funcall start point normal dot ray counters))
                 (end (funcall end point normal dot ray counters))
                 (ratio (imod (* (aref point axis) scale) 1.0)))
            (declare (type vec start end))
            (vec-lerp start end ratio))))))

;;; NOISE-SHADER

(defclass noise-shader (shader)
  ((start :initarg :start :reader start-of)
   (end :initarg :end :reader end-of)
   (scale :initarg :scale :reader scale-of)))

(defmethod compute-shader-function ((shader noise-shader) scene)
  (let ((start (compile-shader (start-of shader) scene))
        (end (compile-shader (end-of shader) scene))
        (scale (/ 1.0 (scale-of shader))))
    (declare (optimize speed))
    (lambda (point normal n.d ray counters)
      (let* ((v (vec* point scale))
             (noise (vector-noise v))
             (start-color (funcall start point normal n.d ray counters))
             (end-color (funcall end point normal n.d ray counters)))
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

(defmethod compute-shader-function ((shader checker) scene)
  (let ((odd (compile-shader (odd-of shader) scene))
	(even (compile-shader (even-of shader) scene))
        (scale (float (scale-of shader))))
    (sb-int:named-lambda shade-checher (point normal dot ray counters)
      (declare (optimize speed))
      (funcall (if (checkerp point scale)
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
  (let* ((functions (mapcar (lambda (shader)
                              (the function (compile-shader shader scene)))
			    (shaders-of shader)))
	 (count (float (length functions))))
    (sb-int:named-lambda shade-composite (point normal dot ray counters)
      (declare (optimize speed))
      (let ((result (alloc-vec)))
        (dolist (fun functions)
          (declare (function fun))
          (%vec+ result result
                 (funcall fun point normal dot ray counters)))
        (%vec/ result result count)))))
