(require :mcclim)
(require :alexandria)

(defpackage :kd
  (:use :cl :alexandria))

(in-package :kd)

;;;; FOR REAL

(deftype rvector ()
  '(simple-array single-float (3)))

(deftype axis ()
  '(member 0 1 2))

(declaim (inline next-axis prev-axis))
(defun next-axis (axis)
  (if (= axis 2)
      0
      (1+ axis)))
(defun prev-axis (axis)
  (if (= axis 0)
      2
      (1- axis)))

(declaim (inline make-rvector))
(defun make-rvector ()
  (make-array 3 :element-type 'single-float))

(defstruct ray
  (origin (required-argument) :type rvector)
  (direction (required-argument) :type rvector))

(defstruct (kd-node  (:constructor nil))
  (min (required-argument) :type rvector)
  (max (required-argument) :type rvector))

(declaim (inline kd-min kd-max))

(defun kd-min (kd-node)
  (kd-node-min kd-node))

(defun kd-max (kd-node)
  (kd-node-max kd-node))

(defstruct (kd-interior-node (:include kd-node))
  (left (required-argument) :type kd-node)
  (right (required-argument) :type kd-node)
  (depth (required-argument) :type fixnum)
  (axis (required-argument) :type fixnum)
  (plane-position (required-argument) :type single-float))

(declaim (inline kd-left kd-right kd-axis kd-plane-position))

(defun kd-left (kd-node)
  (kd-interior-node-left kd-node))

(defun kd-right (kd-node)
  (kd-interior-node-right kd-node))

(defun kd-axis (kd-node)
  (kd-interior-node-axis kd-node))

(defun kd-plane-position (kd-node)
  (kd-interior-node-plane-position kd-node))

(defstruct (kd-leaf-node (:include kd-node) (:predicate kd-leaf-p))
  objects)

(declaim (inline kd-objects kd-depth))

(defun kd-objects (kd-node)
  (kd-leaf-node-objects kd-node))

(defconstant +kd-stack-node-index+ 0)
(defconstant +kd-stack-distance-index+ 1)
(defconstant +kd-stack-point-index+ 2)
(defconstant +kd-stack-prev-index+ 3)
(defconstant +kd-stack-entry-size+ 4)

(declaim (inline make-kd-stack 
                 kd-stack-node kd-stack-distance kd-stack-point kd-stack-prev
                 (setf kd-stack-node) (setf kd-stack-distance) 
                 (setf kd-stack-point) (setf kd-stack-prev)))

(defun make-kd-stack (kd-node)
  (make-array (* +kd-stack-entry-size+ (kd-depth kd-node))))

(defun kd-stack-node (stack pointer)
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-node-index+)))

(defun (setf kd-stack-node) (node stack pointer)
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-node-index+)) node))

(defun kd-stack-distance (stack pointer)
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-distance-index+)))

(defun (setf kd-stack-distance) (distance stack pointer)
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-distance-index+)) distance))

(defun kd-stack-point (stack pointer)
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-point-index+)))

(defun (setf kd-stack-point) (point stack pointer)
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-point-index+)) point))

(defun kd-stack-prev (stack pointer)
  (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-prev-index+)))

(defun (setf kd-stack-prev) (prev stack pointer)
  (setf (aref stack (+ (* pointer +kd-stack-entry-size+) +kd-stack-prev-index+)) prev))

;;; RayTravAlgRECB from Appendix C.
(defun kd-traverse (kd-node ray)
  (declare (optimize sb-c::stack-allocate-dynamic-extent)
           (kd-node kd-node)
           (ray ray))
  (multiple-value-bind (entry-distance exit-distance)
      (ray/box-intersections ray (kd-min kd-node) (kd-max kd-node))
    (when (> exit-distance 0.0)
      (let ((stack (make-kd-stack kd-node))
            (current-node kd-node)
            (entry-pointer 0)
            (ray-origin (ray-origin ray))
            (ray-direction (ray-direction ray)))
        (declare (dynamic-extent stack))
        (setf (kd-stack-distance stack entry-pointer) entry-distance
              (kd-stack-point stack entry-pointer)
              (if (>= entry-distance 0.0)
                  (vector-adjust ray-origin ray-direction entry-distance)
                  ray-origin))
        (let ((exit-pointer 1)
              (far-child nil))
          (setf (kd-stack-distance stack exit-pointer) exit-distance
                (kd-stack-point stack exit-pointer)
                (vector-adjust ray-origin ray-direction exit-distance))
          (setf (kd-stack-node stack exit-pointer) nil)
          (loop while current-node
                do (tagbody
                    :continue
                      (when (kd-interior-node-p current-node)
                        (let* ((split (kd-plane-position current-node))
                               (axis (kd-axis current-node))
                               (entry-projection (aref (kd-stack-point stack entry-pointer) axis))
                               (exit-projection (aref (kd-stack-point stack  exit-pointer) axis)))
                          (cond ((<= entry-projection split)
                                 (cond ((<= exit-projection split)
                                        (setf current-node (kd-left current-node))
                                        (go :continue))                                     
                                       ((= exit-projection split)
                                        (setf current-node (kd-right current-node))
                                        (go :continue))
                                       (t
                                        (setf far-child (kd-right current-node)
                                              current-node (kd-left current-node)))))
                                ((< split exit-projection)
                                 (setf current-node (kd-right current-node))
                                 (go :continue))
                                (t
                                 (setf far-child (kd-left current-node)
                                       current-node (kd-right current-node))))
                          (let ((distance (/ (- split (aref ray-origin axis) ) (aref ray-direction axis)))
                                (tmp exit-pointer))
                            (incf exit-pointer)
                            (when (= exit-pointer entry-pointer)
                              (incf exit-pointer))
                            (setf (kd-stack-prev stack exit-pointer) tmp
                                  (kd-stack-distance stack exit-pointer) distance
                                  (kd-stack-point stack exit-pointer)
                                  (let ((point (make-rvector))
                                        (next-axis (next-axis axis))
                                        (prev-axis (prev-axis axis)))
                                    (setf (aref point axis) split)
                                    (setf (aref point next-axis) 
                                          (+ (aref ray-origin next-axis)
                                             (* distance (aref ray-direction next-axis))))
                                    (setf (aref point prev-axis) 
                                          (+ (aref ray-origin prev-axis)
                                             (* distance (aref ray-direction prev-axis))))
                                    point))))))
                   (let ((intersection (find-intersection ray (kd-objects current-node)
                                                          (kd-stack-distance stack entry-pointer)
                                                          (kd-stack-distance stack exit-pointer))))
                     (when intersection
                       (return intersection))
                     (setf entry-pointer exit-pointer
                           current-node (kd-stack-node stack exit-pointer)
                           exit-pointer (kd-stack-prev stack exit-pointer)))))))))

(defun subdivide (objects min max)
  (if objects
      (multiple-value-bind (point axis left right) (divide boxes min max)
        (if point
            (let ((new-max (new-corner point axis max))
                  (new-min (new-corner point axis min)))
              (make-instance 'kd-interior
                             :min min
                             :max max
                             :left (subdivide left min new-max)
                             :right (subdivide right new-min max)))
            (make-instance 'kd-lead :min min :max max :objects objects)))
      (make-instance 'kd-leaf :min min :max max :objects nil)))

(defun divide (boxes min max)
  (assert boxes)
  (let ((parent-area (surface-area* min max))
        (best-cost nil)
        (best-point nil)
        (best-axis nil)
        (best-left nil)
        (best-right nil))
    (dolist (box boxes)
      (dolist (axis '(:x :y))
        (dolist (p (list (box-min box axis) (box-max box axis)))
          (multiple-value-bind (left right) (split-boxes boxes p axis)
            (let ((cost (split-cost left right parent-area)))
              (when (or (not best-cost) (< cost best-cost))
                (setf best-cost cost
                      best-point p
                      best-axis axis
                      best-left left
                      best-right right)))))))
    (unless (or (= best-point (dim best-axis min))
                (= best-point (dim best-axis max))
                (>= best-cost (whole-cost boxes parent-area)))
      (values best-point best-axis best-left best-right))))

(defun split-boxes (boxes point axis)
  (let (left right)
    (dolist (box boxes)
      (when (< (box-min box axis) point)
        (push box left))
      (when (> (box-max box axis) point)
        (push box right)))
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

(defun intersection-cost (boxes)
  ;; FIXME: Some objects are cheap, some expensive.
  (* 0.1 (length boxes)))

(defun intersection-probability (boxes parent-area)
  (let ((sum 0))
    (dolist (box boxes)
      (incf sum (/ (surface-area box) parent-area)))
    sum))

(defun surface-area (box)
  (let ((min (min-of box))
        (max (max-of box)))
    (surface-area* min max)))

(defun surface-area* (min max)
  (* (- (car max) (car min))
     (- (cdr max) (cdr min))))

