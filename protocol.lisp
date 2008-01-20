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

(defgeneric object-functions (object scene))

(defun compile-object (object scene)
  (multiple-value-bind (intersection normal) (object-functions object scene)
    (multiple-value-bind (min max) (object-extents object)
      (make-compiled-object
       :intersection intersection
       :normal normal
       :shader (compile-shader (shader-of object) scene)
       :min min :max max))))

(declaim (inline intersect))
(defun intersect (object ray &optional shadow)
  (if shadow
      (note-shadow-test)
      (note-intersection))
  (multiple-value-bind (hitp x)
      (funcall (object-intersection object) ray)
    (declare (type boolean hitp) (type (or null compiled-object) x))
    (when hitp
      (if shadow
          (note-shadow)
          (note-hit)))
    (values hitp (or x object))))

(defgeneric object-extents (object))

(defmethod object-extents ((object object))
  nil)


;;;#### The inside function
;;;
;;; must accept a vector designating a point, and return true if that point
;;; is inside the object.

(defgeneric csg-functions (object scene))

(defclass csg-type ()
  ((type 
    :initform (find-default :type '(member difference intersection))
    :initarg :type :accessor type-of)))

(defclass csg (object csg-type)
  ((objects :initform nil :initarg :objects :accessor objects-of))
  (:documentation
   "An uncompiled CSG node, representing a boolean operation
between an arbitrary number of operands. Transformed later to a
tree of CSG-NODE instances."))

(defun csg-nodes (csg)
  "Return a tree of CSG-NODEs for a CSG instance."
  (with-defaults (:type (type-of csg) :transform (transform-of csg))
    (reduce (lambda (x y)
	      (make-instance 'csg-node :left x :right y))
	    (objects-of csg))))

(defmethod object-functions ((csg csg) scene)
  (object-functions (csg-nodes csg) scene))

(defmethod csg-functions ((csg csg) scene)
  (csg-functions (csg-nodes csg) scene))

(defclass csg-node (object csg-type)
  ((left :initarg :left :accessor left-of)
   (right :initarg :right :accessor right-of))
  (:documentation
   "An uncompiled CSG node, representing a boolean operation
between two operands. CSG instances are transformed to trees of
CSG-NODE instances immediately before compilation."))

(defstruct (csg-intersection
             (:constructor %make-csg-intersection (distance object)))
  "Holds an intersection distance and the primitive (not CSG)
object responsible for it. Used during calculation of RAY/CSG
intersections."
  (distance (required-argument) :type (float #.epsilon))
  (object (required-argument) :type compiled-object))

(definterface make-csg-intersection (distance object)
  %make-csg-intersection)

(defmacro csg-lambda (fun origin direction)
  (let ((ci (gensym "CSG-INTERSECTION")))
    `(lambda (,ci)
       (funcall ,fun (adjust-vector ,origin
				    ,direction
				    (csg-intersection-distance ,ci))))))

(defun undelegated-csg-normal (point)
  (declare (ignore point))
  (error "CSG normal not delegated."))

(defmethod object-functions ((node csg-node) scene)
  (values 
   ;; intersect
   (let* ((matrix (transform-of node))
	  (inverse (if matrix (inverse-matrix matrix) (identity-matrix))))
     (let-values (((intersect-x inside-x) (csg-functions (left-of node) scene))
		  ((intersect-y inside-y) (csg-functions (right-of node) scene)))
      (declare (type (function (vector vector) (simple-array csg-intersection (*)))
		     intersect-x intersect-y)
	       (type (function (vector) t) inside-x inside-y))
      (macrolet 
	  ((make-lambda (find-x find-y)
	     `(lambda (ray)
                (declare (type ray ray))
		(let* ((o (transform-vector (ray-origin ray) inverse))
		       (d (transform-direction (ray-direction ray) inverse))
		       (sx (,find-x (csg-lambda inside-y o d)
				    (funcall intersect-x o d)))
		       (sy (,find-y (csg-lambda inside-x o d)
				    (funcall intersect-y o d))))
		  (let ((s (if (and sx sy)
			       (if (< (csg-intersection-distance sx)
				      (csg-intersection-distance sy))
				   sx
				   sy)
			       (or sx sy))))
		    (when (and s (< epsilon (csg-intersection-distance s)
				    (ray-extent ray)))
		      (setf (ray-extent ray) (csg-intersection-distance s))
		      (values t (csg-intersection-object s))))))))
	(ecase (type-of node)
	  (intersection
	   (make-lambda find-if find-if))
	  (difference 
	   (make-lambda find-if-not find-if))))))
   ;; normal
   #'undelegated-csg-normal))

(defmethod csg-functions ((node csg-node) scene)
  (let-values (((intersect-x inside-x) (csg-functions (left-of node) scene))
	       ((intersect-y inside-y) (csg-functions (right-of node) scene)))
    (declare (type (function (vector vector) simple-vector)
		   intersect-x intersect-y)
	     (type (function (vector) t) inside-x inside-y))
    (values
     ;; csg intersection function 
     ;; FIXME: Don't we need to obey the transform here?
     (macrolet 
	 ((make-lambda (remove-x remove-y)
	    `(lambda (origin direction)
               (declare (type vector origin direction))
	       (merge 'simple-vector
		      (,remove-x (csg-lambda inside-y origin direction)
				 (funcall intersect-x origin direction))
		      (,remove-y (csg-lambda inside-x origin direction)
				 (funcall intersect-y origin direction))
		      #'< :key #'csg-intersection-distance))))
       (ecase (type-of node)
	 (intersection
	  (make-lambda remove-if-not remove-if-not))
	 (difference
	  (make-lambda remove-if remove-if-not))))
     ;; inside function
     (macrolet ((make-lambda (combine)
		  `(lambda (point)
		     (,combine (funcall inside-x point)
			       (funcall inside-y point)))))
	 (ecase (type-of node)
	   (intersection
	    (make-lambda and))
	   (difference
	    (make-lambda (lambda (x y) (and x (not y))))))))))





(defgeneric light-functions (light scene))

(defun compile-light (light scene)
  (multiple-value-bind (direction illumination)
      (light-functions light scene)
    (make-compiled-light
     :direction direction
     :illumination illumination)))

(declaim (inline light-vector illuminate))
	 
(defun light-vector (light point)
  (funcall (light-direction light) point))

(defun illuminate (light point light-vector)
  (funcall (light-illumination light) point light-vector))

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
  (with-arrays (location)
    (let ((c-scene (scene-compiled-scene scene))
          ;; No real light buffers yet: just a single shadow object cache.
	  (last nil))
      (declare (type (or null compiled-object) last))
      (lambda (point nlv len)
	(declare (type vector point nlv) (type float len)
		 (optimize speed))
	(let ((ray (make-ray :origin point :direction nlv :extent len)))
	  (if last
	      (or (intersect last ray t)
		  (find-if (lambda (object)
			     (declare (type compiled-object object))
			     (unless (eq last object)
			       (when (intersect object ray t)
				 (setf last object))))
			   (compiled-scene-objects c-scene)))
	      (find-if (lambda (object)
			 (declare (type compiled-object object))
			 (when (intersect object ray t)
			   (setf last object)))
		       (compiled-scene-objects c-scene))))))))

;;;## Shader protocol
;;;
;;; Shader protocol controls the compilation of SHADER instances into
;;; a more efficient representation used for rendering. There must be
;;; an applicable method on SHADER-FUNCTION for each subclass of
;;; SHADER that returns the corresponding shader function:
;;;
;;;#### The shader function
;;;
;;; must accept an INTERSECTION and a RAY, and return the apparent color.

(defgeneric shader-function (shader scene))

(defun compile-shader (shader scene)
  (shader-function shader scene))

(defmethod shader-function ((null null) scene)
  (constantly black))

(defgeneric shader-weight (shader)
  (:method-combination +))

(defun coefficient (value shader)
  (/ value (shader-weight shader)))

(declaim (inline shade))
(defun shade (object ray)
  (let* ((point (adjust-vector (ray-origin ray) (ray-direction ray) 
			       (ray-extent ray)))
	 (normal (funcall (object-normal object) point))
	 (n.d (dot-product normal (ray-direction ray))))
    (funcall (object-shader object)
             point 
             (if (plusp n.d) (reverse-vector normal) normal)
             n.d
	     ray)))
