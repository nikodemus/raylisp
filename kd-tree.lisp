(in-package :raylisp)

(declaim (optimize speed))

(deftype axis ()
  '(member 0 1 2))

(declaim (inline next-axis prev-axis))
(defun next-axis (axis)
  (declare (axis axis))
  (if (= axis 2)
      0
      (1+ axis)))

(defun prev-axis (axis)
  (declare (axis axis))
  (if (= axis 0)
      2
      (1- axis)))

(defstruct (kd-node  (:constructor nil))
  (min (required-argument) :type vec)
  (max (required-argument) :type vec))

(declaim (inline kd-min kd-max))

(defun kd-min (kd-node)
  (kd-node-min kd-node))
(defun kd-max (kd-node)
  (kd-node-max kd-node))

(defstruct (kd-interior-node (:include kd-node))
  (left (required-argument :left) :type kd-node)
  (right (required-argument :right) :type kd-node)
  (depth (required-argument :depth) :type fixnum)
  (axis (required-argument :axis) :type axis)
  (plane-position (required-argument :plane-position) :type single-float))

(declaim (inline kd-left kd-right kd-axis kd-plane-position kd-depth))

(defun kd-left (kd-node)
  (kd-interior-node-left kd-node))

(defun kd-right (kd-node)
  (kd-interior-node-right kd-node))

(defun kd-axis (kd-node)
  (kd-interior-node-axis kd-node))

(defun kd-plane-position (kd-node)
  (kd-interior-node-plane-position kd-node))

(defun kd-depth (kd-node)
  (kd-interior-node-depth kd-node))

(defstruct (kd-leaf-node (:include kd-node) (:predicate kd-leaf-p))
  objects)

(declaim (inline kd-objects))

(defun kd-objects (kd-node)
  (kd-leaf-node-objects kd-node))

(defconstant +kd-stack-node-index+     0)
(defconstant +kd-stack-distance-index+ 1)
(defconstant +kd-stack-point-index+    2)
(defconstant +kd-stack-prev-index+     3)
(defconstant +kd-stack-entry-size+     4)

(declaim (inline make-kd-stack
                    kd-stack-node kd-stack-distance kd-stack-point kd-stack-prev
                    (setf kd-stack-node) (setf kd-stack-distance)
                    (setf kd-stack-point) (setf kd-stack-prev)))

(defun make-kd-stack (kd-node)
  (make-array (* +kd-stack-entry-size+ 50)))

(defun kd-stack-node (stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-node-index+)))

(defun (setf kd-stack-node) (node stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-node-index+)) node))

(defun kd-stack-distance (stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-distance-index+)))

(defun (setf kd-stack-distance) (distance stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-distance-index+)) distance))

(defun kd-stack-point (stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-point-index+)))

(defun (setf kd-stack-point) (point stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-point-index+)) point))

(defun kd-stack-prev (stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-prev-index+)))

(defun (setf kd-stack-prev) (prev stack pointer)
  (declare (simple-vector stack) (fixnum pointer))
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-prev-index+)) prev))

;;; RayTravAlgRECB from Appendix C.
(defun kd-traverse (root ray counters shadowp)
  (declare (kd-node root)
           (ray ray)
           (optimize speed))
  (multiple-value-bind (entry-distance exit-distance)
      (ray/box-intersections ray (kd-min root) (kd-max root))
    (declare (type (or null float) entry-distance)
             (float exit-distance))
    (when entry-distance
      (unless (kd-interior-node-p root)
        (return-from kd-traverse (%find-intersection ray (kd-objects root) counters shadowp)))
      (let ((stack (make-kd-stack root))
            (current-node root)
            (entry-pointer 0)
            (ray-origin (ray-origin ray))
            (ray-direction (ray-direction ray)))
        (declare (dynamic-extent stack))
        (setf (kd-stack-distance stack entry-pointer) entry-distance
              (kd-stack-point stack entry-pointer)
              (if (>= entry-distance 0.0)
                  (adjust-vec ray-origin ray-direction entry-distance)
                  ray-origin))
        (let ((exit-pointer 1)
              (far-child nil))
          (declare (fixnum exit-pointer))
          (setf (kd-stack-distance stack exit-pointer) exit-distance)
          (setf (kd-stack-point stack exit-pointer)
                (adjust-vec ray-origin ray-direction exit-distance))
          (setf (kd-stack-node stack exit-pointer) nil)
          (loop while current-node
                do (loop until (kd-leaf-p current-node)
                         do (tagbody
                               (let* ((split (kd-plane-position current-node))
                                      (axis (kd-axis current-node))
                                      (entry-projection
                                       (aref (the vec (kd-stack-point stack entry-pointer)) axis))
                                      (exit-projection
                                       (aref (the vec (kd-stack-point stack  exit-pointer)) axis)))
                                 (cond ((<= entry-projection split)
                                        (cond ((<= exit-projection split)
                                               (setf current-node (kd-left current-node))
                                               (go :cont))
                                              ((= exit-projection split)
                                               (setf current-node (kd-right current-node))
                                               (go :cont))
                                              (t
                                               (setf far-child (kd-right current-node)
                                                     current-node (kd-left current-node)))))
                                       ((< split exit-projection)
                                        (setf current-node (kd-right current-node))
                                        (go :cont))
                                       (t
                                        (setf far-child (kd-left current-node)
                                              current-node (kd-right current-node))))
                                 (let ((distance (/ (- split (aref ray-origin axis)) (aref ray-direction axis)))
                                       (tmp exit-pointer))
                                   (incf-fixnum exit-pointer)
                                   (when (= exit-pointer entry-pointer)
                                     (incf-fixnum exit-pointer))
                                   (setf (kd-stack-prev stack exit-pointer) tmp
                                         (kd-stack-distance stack exit-pointer) distance
                                         (kd-stack-node stack exit-pointer) far-child
                                         (kd-stack-point stack exit-pointer)
                                         (let ((point (make-array 3 :element-type 'float))
                                               (next-axis (next-axis axis))
                                               (prev-axis (prev-axis axis)))
                                           (setf (aref point axis) split)
                                           (setf (aref point next-axis)
                                                 (+ (aref ray-origin next-axis)
                                                    (* distance (aref ray-direction next-axis))))
                                           (setf (aref point prev-axis)
                                                 (+ (aref ray-origin prev-axis)
                                                    (* distance (aref ray-direction prev-axis))))
                                           point))))
                             :cont))
                (when current-node
                  (let ((intersection (%find-intersection* ray
                                                           (kd-objects current-node)
                                                           (kd-stack-distance stack entry-pointer)
                                                           (kd-stack-distance stack exit-pointer)
                                                           counters
                                                           shadowp)))
                    (when intersection
                      (return-from kd-traverse intersection))))
                (setf entry-pointer exit-pointer
                      current-node (kd-stack-node stack exit-pointer)
                      exit-pointer (kd-stack-prev stack entry-pointer))))))))

(defun ray/box-intersections (ray bmin bmax)
  (declare (type vec bmin bmax)
           (optimize speed))
  (let ((dir (ray-direction ray))
        (orig (ray-origin ray)))
    (with-arrays (dir orig)
     (let ((ox (orig 0))
           (oy (orig 1))
           (oz (orig 2))
           (dx (dir 0))
           (dy (dir 1))
           (dz (dir 2)))
       (flet ((sides (axis oc dc)
                (if (not (= dc 0.0))
                    (let ((t1 (/ (- (aref bmin axis) oc) dc))
                          (t2 (/ (- (aref bmax axis) oc) dc)))
                      (if (> t1 t2)
                          (values t2 t1)
                          (values t1 t2)))
		    (values 0.0 float-positive-infinity))))
         (declare (inline sides))
         (let-values (((x1 x2) (sides 0 ox dx))
                      ((y1 y2) (sides 1 oy dy))
                      ((z1 z2) (sides 2 oz dz)))
           (let ((t1 (max x1 y1 z1))
                 (t2 (min x2 y2 z2)))
             (if (> t1 t2)
                 (values nil 0.0)
                 (values t1 t2)))))))))

(defun make-kd-tree (objects)
  (let (bounded unbounded min max)
    (dolist (object objects)
      (let ((this-min (object-min object))
            (this-max (object-max object)))
        (cond (this-min
               (push object bounded)
               (if min
                   (setf min (vec-min min this-min this-max)
                         max (vec-max max this-min this-max))
                   (setf min (vec-min this-min this-max)
                         max (vec-max this-min this-max))))
              (t
               (push object unbounded)))))
    (values (when bounded
              (subdivide bounded min max))
            unbounded)))

(defun new-corner (point axis old)
  (with-arrays (old)
    (ecase axis
     (0
      (vec point (old 1) (old 2)))
     (1
      (vec (old 0) point (old 2)))
     (2
      (vec (old 0) (old 1) point)))))

(defun subdivide (objects min max)
  (if objects
      (multiple-value-bind (point axis left right) (divide objects min max)
        (if point
            (let ((new-max (new-corner point axis max))
                  (new-min (new-corner point axis min)))
              #+nil
              (assert (and left right min max new-min new-max))
              (let-values (((left-kd left-depth) (subdivide left min new-max))
                           ((right-kd right-depth) (subdivide right new-min max)))
                (let ((depth (max left-depth right-depth)))
                  (values (make-kd-interior-node
                           :plane-position point
                           :axis axis
                           :min min
                           :max max
                           :depth depth
                           :left left-kd
                           :right right-kd)
                          (1+ depth)))))
            (values (make-kd-leaf-node :min min :max max :objects objects) 1)))
      (values (make-kd-leaf-node :min min :max max :objects nil) 1)))

(defun divide (objects min max)
  (assert objects)
  (let ((parent-area (surface-area* min max))
        (best-cost nil)
        (best-point nil)
        (best-axis nil)
        (best-left nil)
        (best-right nil))
    (dolist (object objects)
      (dolist (axis '(0 1 2))
        (dolist (p (list (aref (object-min object) axis) (aref (object-max object) axis)))
          (multiple-value-bind (left right) (split-objects objects p axis)
            (let ((cost (split-cost left right parent-area)))
              (when (or (not best-cost) (< cost best-cost))
                (setf best-cost cost
                      best-point p
                      best-axis axis
                      best-left left
                      best-right right)))))))
    (unless (or (= best-point (aref min best-axis))
                (= best-point (aref max best-axis))
                (>= best-cost (whole-cost objects parent-area)))
      (values best-point best-axis best-left best-right))))

(defun split-objects (objects point axis)
  (let (left right)
    (dolist (object objects)
      (when (< (aref (object-min object) axis) point)
        (push object left))
      (when (> (aref (object-max object) axis) point)
        (push object right)))
    (values left right)))

(defun split-cost (left right parent-area)
  (* (if (and left right)
         1
         0.8d0)
     (+ (* (intersection-probability left parent-area)
           (intersection-cost left))
        (* (intersection-probability right parent-area)
           (intersection-cost right)))))

(defun whole-cost (objects area)
  (+ (* (intersection-probability objects area)
        (intersection-cost objects))))

(defun intersection-cost (objects)
  ;; FIXME: Some objects are cheap, some expensive!
  (* 0.01 (length objects)))

(defun intersection-probability (objects parent-area)
  (let ((sum 0))
    (dolist (object objects)
      (incf sum (/ (surface-area object) parent-area)))
    sum))

(defun surface-area (object)
  (let ((min (object-min object))
        (max (object-max object)))
    (surface-area* min max)))

(defun surface-area* (min max)
  (declare (type vec min max))
  (let ((x (- (aref max 0) (aref min 0)))
        (y (- (aref max 1) (aref min 1)))
        (z (- (aref max 2) (aref min 2))))
    (+ (* x y) (* x z) (* y z))))
