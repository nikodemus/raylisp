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

(defgeneric compute-object-properties (scene-object scene transform))

(defgeneric compute-object-extents (scene-object transform))

(defmethod compute-object-extents ((object scene-object) transform)
  nil)

(defun compile-scene-object (object scene transform)
  (destructuring-bind (&key intersection normal)
      (compute-object-properties object scene transform)
    (assert (and intersection normal))
    (multiple-value-bind (min max) (compute-object-extents object transform)
      (make-compiled-object
       :intersection intersection
       :normal normal
       :shader (compile-shader (shader-of object) object scene transform)
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

(defgeneric compute-csg-properties (object scene transform))

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

(defmethod compute-object-properties ((csg csg) scene transform)
  (compute-object-properties (csg-nodes csg) scene transform))

(defmethod compute-csg-properties ((csg csg) scene transform)
  (compute-csg-properties (csg-nodes csg) scene transform))

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
    `(sb-int:named-lambda %csg-lambda (,ci)
       (funcall ,fun (adjust-vec ,origin
                                 ,direction
                                 (csg-intersection-distance ,ci))))))

(defun undelegated-csg-normal (point)
  (declare (ignore point))
  (error "CSG normal not delegated."))

(defmethod compute-object-properties ((node csg-node) scene transform)
  (let ((matrix (matrix* transform (transform-of node))))
    (list
     :intersection
     (let-plists ((((:all-intersections all-left) (:inside inside-left))
                   (compute-csg-properties (left-of node) scene matrix))
                  (((:all-intersections all-right) (:inside inside-right))
                   (compute-csg-properties (right-of node) scene matrix)))
       (declare (type (function (vec vec) (simple-array csg-intersection (*)))
                      all-left all-right)
                (type (function (vec) t) inside-left inside-right))
       (macrolet
           ((make-lambda (name find-left find-right)
              `(sb-int:named-lambda ,name (ray)
                 (declare (type ray ray) (optimize speed))
                 (let* ((o (ray-origin ray))
                        (d (ray-direction ray))
                        (sx (,find-left (csg-lambda inside-right o d)
                                        (funcall all-left o d)))
                        (sy (,find-right (csg-lambda inside-left o d)
                                         (funcall all-right o d))))
                   (declare (dynamic-extent o d))
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
            (make-lambda csg-intersection-lambda find-if find-if))
           (difference
            (make-lambda csg-difference-lambda find-if-not find-if)))))
     :normal
     #'undelegated-csg-normal)))

;;; KLUDGE: SBCL's built-in MERGE is slow.
;;;
;;; FIXME: There have to be more efficient ways to do this...
;;; Maybe instead of simple-vectors of csg-intersections
;;; we should have a simple-vector like this:
;;; #(distance object distance object distance...)?
(defun merge-csg-intersections (v1 v2)
  (declare (simple-vector v1 v2) (optimize speed))
  (let* ((l1 (length v1))
         (l2 (length v2))
         (result (make-array (+ l1 l2)))
         (pr 0)
         (p1 0)
         (p2 0))
    (macrolet ((handle-comparison ()
                 `(cond ((< (csg-intersection-distance i1) (csg-intersection-distance i2))
                         (setf (aref result pr) i1)
                         (incf pr)
                         (cond ((>= (incf p1) l1)
                                (setf (aref result pr) i2)
                                (incf pr)
                                (if (>= (incf p2) l2)
                                    (go :end)
                                    (go :finish-from-v2)))
                               (t
                                (go :loop-with-i2))))
                        (t
                         (setf (aref result pr) i2)
                         (incf pr)
                         (cond ((>= (incf p2) l2)
                                (setf (aref result pr) i1)
                                (incf pr)
                                (if (>= (incf p1) l1)
                                    (go :end)
                                    (go :finish-from-v1)))
                               (t
                                (go :loop-with-i1)))))))
      (tagbody
         (cond ((>= p1 l1)
                (go :finish-from-v2))
               ((>= p2 l2)
                (go :finish-from-v1)))
         (let ((i1 (aref v1 p1))
               (i2 (aref v2 p2)))
           (declare (type csg-intersection i1 i2))
           (tagbody
              (handle-comparison)
            :loop-with-i1
              (setf i2 (aref v2 p2))
              (handle-comparison)
            :loop-with-i2
              (setf i1 (aref v1 p1))
              (handle-comparison)))
       :finish-from-v1
         (replace result v1 :start1 pr :start2 p1)
         (go :end)
       :finish-from-v2
         (replace result v2 :start1 pr :start2 p2)
         (go :end)
       :end)
      (unless (every #'csg-intersection-p result)
        (break "~S and ~S gave ~S" v1 v2 result))
      result)))

(defun fast-remove-if-not (function vector)
  (declare (function function)
           (simple-vector vector)
           (optimize speed))
  (let* ((len (length vector))
         (result (make-array len))
         (p 0))
    (declare (fixnum p))
    (dotimes (i len)
      (declare (optimize (sb-c::insert-array-bounds-checks 0)))
      (let ((elt (aref vector i)))
        (when (funcall function elt)
          (setf (aref result p) elt)
          (setf p (logand most-positive-fixnum (+ 1 p))))))
    (sb-kernel:%shrink-vector result p)
    result))

(defun fast-remove-if (function vector)
  (declare (function function)
           (simple-vector vector)
           (optimize speed))
  (let* ((len (length vector))
         (result (make-array len))
         (p 0))
    (declare (fixnum p))
    (dotimes (i len)
      (declare (optimize (sb-c::insert-array-bounds-checks 0)))
      (let ((elt (aref vector i)))
        (unless (funcall function elt)
          (setf (aref result p) elt)
          (setf p (logand most-positive-fixnum (+ 1 p))))))
    (sb-kernel:%shrink-vector result p)
    result))

(defmethod compute-csg-properties ((node csg-node) scene transform)
  (let-plists ((((:all-intersections all-left) (:inside inside-left))
                (compute-csg-properties (left-of node) scene transform))
               (((:all-intersections all-right) (:inside inside-right))
                (compute-csg-properties (right-of node) scene transform)))
    (declare (type (function (vec vec) simple-vector) all-left all-right)
	     (type (function (vec) t) inside-left inside-right))
    (list
     :all-intersections
     (macrolet
         ((make-lambda (name remove-left remove-right)
            `(sb-int:named-lambda ,name (origin direction)
               (declare (type vec origin direction) (optimize speed))
               (merge-csg-intersections
                (,remove-left (csg-lambda inside-right origin direction)
                              (funcall all-left origin direction))
                (,remove-right (csg-lambda inside-left origin direction)
                               (funcall all-right origin direction))))))
       (ecase (type-of node)
         (intersection
          (make-lambda csg-intersection-merge fast-remove-if-not fast-remove-if-not))
         (difference
          (make-lambda csg-difference-merge fast-remove-if fast-remove-if-not))))
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
	(declare (type vec point nlv) (type float len)
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

(defgeneric compute-shader-function (shader object scene transform))

(declaim (ftype (function (t t t t) (values (function (compiled-object vec vec float ray t)
                                                  (values vec &optional))
                                        &optional))
                compile-shader))
(defun compile-shader (shader object scene transform)
  (compute-shader-function shader object scene transform))

(defmethod compute-shader-function ((null null) object scene transform)
  (constantly black))

(defgeneric shader-weight (shader)
  (:method-combination +))

(declaim (inline coefficient))
(defun coefficient (value shader)
  (declare (float value))
  (/ value (the float (shader-weight shader))))

(declaim (inline shade))
(defun shade (object ray counters)
  (declare (optimize speed))
  (let* ((point (adjust-vec (ray-origin ray) (ray-direction ray)
                            (ray-extent ray)))
	 (normal (funcall (object-normal object) point))
	 (n.d (dot-product normal (ray-direction ray))))
    (flet ((%shade (n)
             (funcall (object-shader object)
                      object
                      point
                      n
                      n.d
                      ray
                      counters)))
      (if (plusp n.d)
          (let ((n2 (vec* normal -1.0)))
            (declare (dynamic-extent n2))
            (%shade n2))
          (%shade normal)))))
