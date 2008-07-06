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

(defgeneric compute-object-properties (scene-object scene))

(defgeneric compute-object-extents (scene-object))

(defmethod compute-object-extents ((object scene-object))
  nil)

(defun compile-scene-object (object scene)
  (destructuring-bind (&key intersection normal)
      (compute-object-properties object scene)
    (assert (and intersection normal))
    (multiple-value-bind (min max) (compute-object-extents object)
      (make-compiled-object
       :intersection intersection
       :normal normal
       :shader (compile-shader (shader-of object) scene)
       :min min :max max
       :name (name-of object)))))

(declaim (inline intersect))
(defun intersect (object ray counters shadow)
  (multiple-value-bind (hitp x)
      (funcall (object-intersection object) ray)
    (declare (type boolean hitp) (type (or null compiled-object) x))
    (note-intersection counters shadow hitp)
    (when hitp
      (if shadow
          object
          (if x x object)))))

;;;#### The inside function
;;;
;;; must accept a vector designating a point, and return true if that point
;;; is inside the object.

(defgeneric compute-csg-properties (object scene))

(defclass csg-type ()
  ((type 
    :initform (find-default :type '(member difference intersection))
    :initarg :type :accessor type-of)))

(defclass csg (scene-object csg-type)
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

(defmethod compute-object-properties ((csg csg) scene)
  (compute-object-properties (csg-nodes csg) scene))

(defmethod compute-csg-properties ((csg csg) scene)
  (compute-csg-properties (csg-nodes csg) scene))

(defclass csg-node (scene-object csg-type)
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

(defmethod compute-object-properties ((node csg-node) scene)
  (list
   :intersection
   (let* ((matrix (transform-of node))
	  (inverse (if matrix (inverse-matrix matrix) (identity-matrix))))
     (let-plists ((((:all-intersections all-left) (:inside inside-left)) (compute-csg-properties (left-of node) scene))
		  (((:all-intersections all-right) (:inside inside-right)) (compute-csg-properties (right-of node) scene)))
       (declare (type (function (vector vector) (simple-array csg-intersection (*)))
                      all-left all-right)
                (type (function (vector) t) inside-left inside-right))
       (macrolet 
           ((make-lambda (find-left find-right)
              `(lambda (ray)
                 (declare (type ray ray))
                 (let* ((o (transform-vector (ray-origin ray) inverse))
                        (d (transform-direction (ray-direction ray) inverse))
                        (sx (,find-left (csg-lambda inside-right o d)
                                        (funcall all-left o d)))
                        (sy (,find-right (csg-lambda inside-left o d)
                                         (funcall all-right o d))))
                   (let ((s (if (and sx sy)
                                (if (< (csg-intersection-distance sx)
                                       (csg-intersection-distance sy))
                                    sx
                                    sy)
                                (or sx sy))))
                     (when (and s (< epsilon (csg-intersection-distance s) (ray-extent ray)))
                       (setf (ray-extent ray) (csg-intersection-distance s))
                       (values t (csg-intersection-object s))))))))
         (ecase (type-of node)
           (intersection
            (make-lambda find-if find-if))
           (difference 
            (make-lambda find-if-not find-if))))))
   :normal
   #'undelegated-csg-normal))

(defmethod compute-csg-properties ((node csg-node) scene)
  (let-plists ((((:all-intersections all-left) (:inside inside-left)) (compute-csg-properties (left-of node) scene))
               (((:all-intersections all-right) (:inside inside-right)) (compute-csg-properties (right-of node) scene)))
    (declare (type (function (vector vector) simple-vector) all-left all-right)
	     (type (function (vector) t) inside-left inside-right))
    (list
     :all-intersections
     ;; FIXME: Don't we need to obey the transform here?
     (macrolet 
         ((make-lambda (remove-left remove-right)
            `(lambda (origin direction)
               (declare (type vector origin direction))
               ;; FIXME: There have to be more efficient ways to do this...
               ;; Maybe instead of simple-vectors of csg-intersections
               ;; we should have a simple-vector like this:
               ;; #(distance object distance object distance...)?
               (merge 'simple-vector
                      (,remove-left (csg-lambda inside-right origin direction)
                                    (funcall all-left origin direction))
                      (,remove-right (csg-lambda inside-left origin direction)
                                     (funcall all-right origin direction))
                      #'< :key #'csg-intersection-distance))))
       (ecase (type-of node)
         (intersection
          (make-lambda remove-if-not remove-if-not))
         (difference
          (make-lambda remove-if remove-if-not))))
     :inside
     (macrolet ((make-lambda (combine)
                  `(lambda (point)
                     (,combine (funcall inside-left point)
                               (funcall inside-right point)))))
       (ecase (type-of node)
         (intersection
          (make-lambda and))
         (difference
          (make-lambda (lambda (x y) (and x (not y))))))))))

(defgeneric compute-light-properties (light scene))

(defun compile-scene-light (light scene)
  (destructuring-bind (&key incident-light illumination)
      (compute-light-properties light scene)
    (assert (and incident-light illumination))
    (make-compiled-light
     :direction incident-light
     :illumination illumination)))

(declaim (inline light-vector illuminate))
	 
(defun light-vector (light point)
  (funcall (light-direction light) point))

(defun illuminate (light point light-vector counters)
  (funcall (light-illumination light) point light-vector counters))

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
  (check-type scene scene)
  (with-arrays (location)
    (let (;; No real light buffers yet: just a single shadow object cache.
	  (last nil))
      (declare (type (or null compiled-object) last))
      (lambda (point nlv len counters)
	(declare (type vector point nlv) (type float len)
		 (optimize speed))
	(with-ray (ray :origin point :direction nlv :extent len)
	  (when (or (and last (intersect last ray counters t))
                    (let ((int (find-scene-intersection ray scene counters t)))
                      (when int
                        (setf last int))))
            t))))))

;;;## Shader protocol
;;;
;;; Shader protocol controls the compilation of SHADER instances into
;;; a more efficient representation used for rendering. There must be
;;; an applicable method on COMPUTE-SHADER-FUNCTION for each subclass of
;;; SHADER that returns the corresponding shader function:
;;;
;;;#### The shader function
;;;
;;; must accept an INTERSECTION and a RAY, and return the apparent color.

(defgeneric compute-shader-function (shader scene))

(defun compile-shader (shader scene)
  (compute-shader-function shader scene))

(defmethod compute-shader-function ((null null) scene)
  (constantly black))

(defgeneric shader-weight (shader)
  (:method-combination +))

(declaim (inline coefficient))
(defun coefficient (value shader)
  (declare (float value))
  (/ value (the float (shader-weight shader))))

(declaim (inline shade))
(defun shade (object ray counters)
  (let* ((point (adjust-vector (ray-origin ray) (ray-direction ray) 
			       (ray-extent ray)))
	 (normal (funcall (object-normal object) point))
	 (n.d (dot-product normal (ray-direction ray))))
    (funcall (object-shader object)
             point 
             (if (plusp n.d) (reverse-vector normal) normal)
             n.d
	     ray
             counters)))
