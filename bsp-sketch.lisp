(require :mcclim)
(require :alexandria)

(defpackage :bsp
  (:use :clim-lisp :clim :alexandria))

(in-package :bsp)

(define-application-frame bsp-frame ()
  ()
  (:panes
   (canvas :application :display-time nil)
   (interactor :interactor))
  (:layouts
   (default
       (vertically (:height 600 :width 600)
         (3/4 canvas)
         (:fill interactor)))))

(define-bsp-frame-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(defparameter *boxes* (generate-random-boxes))

(define-bsp-frame-command (com-scatter :name t)
    ()
  (let ((canvas (find-pane-named *application-frame* 'canvas)))
    (dolist (box *boxes*)
      (draw-box canvas box))))

(defvar *canvas*)

(define-bsp-frame-command (com-build :name t)
    ()
  (let ((*canvas* (find-pane-named *application-frame* 'canvas)))
    (subdivide *boxes* '(0 . 0) '(600 . 450))))

(defparameter *n-boxes* 60)
(defparameter *min-dimension* 5)
(defparameter *max-dimension* 40)

(defun generate-random-boxes ()
  (loop repeat *n-boxes* 
        collect 
           (let ((x0 (random (- 600 *max-dimension*)))
                 (y0 (random (- 450 *max-dimension*)))
                 (w (+ *min-dimension* 
                       (random (- *max-dimension* *min-dimension*))))
                 (h (+ *min-dimension* 
                       (random (- *max-dimension* *min-dimension*)))))
             (make-instance 'box
                            :min (cons x0 y0)
                            :max (cons (+ x0 w) (+ y0 h))))))

(defun make-plane (axis division min max)
  (destructuring-bind (x1 y1 x2 y2)
      (ecase axis
        (:x
         (list division min division max))
        (:y
         (list min division max division)))
    (draw-line* *canvas* x1 y1 x2 y2)))

(defun flip-axis (axis)
  (ecase axis
    (:x :y)
    (:y :x)))

(defclass box ()
  ((min :initarg :min :reader min-of)
   (max :initarg :max :reader max-of)))

(defmethod draw-box (canvas (box box))
  (draw-rectangle* canvas 
                   (car (min-of box)) (cdr (min-of box))
                   (car (max-of box)) (cdr (max-of box))))

(defun box-x-< (a b)
  (< (car (min-of a)) (car (min-of b))))

(defun box-y-< (a b)
  (< (cdr (min-of a)) (cdr (min-of b))))

(defun axis-predicate (axis)
  (ecase axis
    (:x #'box-x-<)
    (:y #'box-y-<)))

(defun box-min (box axis)
  (ecase axis
    (:x (car (min-of box)))
    (:y (cdr (min-of box)))))

(defun box-max (box axis)
  (ecase axis
    (:x (car (max-of box)))
    (:y (cdr (max-of box)))))

;;; START and END are the dimensions of AXIS, and MIN and MAX are along
;;; the other axis (basically extents of the divisive plane we build.)
(defun subdivide (boxes axis start end min max)
  (when (cdr boxes)
    (multiple-value-bind (div left right)
        (divide boxes axis (+ start (round (- end start) 2)))
      (let ((sixa (flip-axis axis)))
        (if div
            (cons (make-plane axis div min max)
                  (nconc (subdivide left sixa min max start div)
                         (subdivide right sixa min max div end)))
            ;; If we would not divide, try along another axis -- if
            ;; it doesn't succeed either we're done.
            (multiple-value-bind (div left right)
                (divide boxes sixa (+ min (round (- max min) 2)))
              (when div
                (cons (make-plane sixa div start end)
                      (nconc (subdivide left axis start end min div)
                             (subdivide right axis start end div max))))))))))

(defun divide (boxes axis middle)
  (let ( ;; Extents of the best space we've seen so far.
        (start 0)
        (end 0)
        ;; Rightmost end of boxes seen so far.
        (far 0)
        (sorted (sort (copy-list boxes) (axis-predicate axis))))
    (dolist (box sorted)
      (let ((min (box-min box axis))
            (max (box-max box axis)))
        ;; If MIDDLE is to the left of the end of our current
        ;; best space we know that it's the best we can get.
        (unless (<= middle end)
          (when (<= far min)
            ;; If farthest point we've seen so far is to the left of
            ;; the box, we have a new space.
            (when (or (< far middle)
                      (< (- far middle) (- middle end)))
              ;; Which we pick up if the space starts to the left
              ;; of the middle, or if the start of this space is
              ;; closer to the middle then the end of the last
              ;; space.
              (setf start far
                    end min)))
          ;; Update farhest point.
          (maxf far max))))
    (when (plusp start)
      (let ((left nil)
            (right nil))
        ;; We could do this on the first go, but what the hell.
        (when (dolist (box sorted)
                (when (< (box-min box axis) end)
                  (push box left))
                (when (> (box-max box axis) end)
                  (push box right))))
        (values end left right)))))

(run-frame-top-level (make-application-frame 'bsp-frame))

(defmethod print-object ((box box) stream)
  (print-unreadable-object (box stream :type t :identity nil)
    (format stream "~S ~S" (min-of box) (max-of box))))

;;; Using SAH

(defun make-plane (axis division min max)
  (destructuring-bind (x1 y1 x2 y2)
      (ecase axis
        (:x
         (list division (cdr min) division (cdr max)))
        (:y
         (list (car min) division (car max) division)))
    (draw-line* *canvas* x1 y1 x2 y2)
    (medium-finish-output (sheet-medium *canvas*))
    (break)))

(defun new-corner (point axis old)
  (ecase axis
    (:x
     (cons point (cdr old)))
    (:y
     (cons (car old) point))))

(defun dim (axis point)
  (ecase axis
    (:x
     (car point))
    (:y
     (cdr point))))

(defvar *depth* 0)

(defun subdivide (boxes min max)
  (when boxes
    (multiple-value-bind (point axis left right) (divide boxes min max)
      (when point
        (let ((new-max (new-corner point axis max))
              (new-min (new-corner point axis min)))
          (cons (make-plane axis point min max)
                (nconc (subdivide left min new-max)
                       (subdivide right new-min max)))))))))

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

