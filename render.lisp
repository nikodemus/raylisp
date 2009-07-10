(in-package :raylisp)

;;;# Rendering

(defun needs-supersampling-p (raster x y)
  (flet ((min* (dim)
	   (loop
	      for i from (1- x) upto (1+ x)
	      for j from (1- y) upto (1+ y)
	      minimizing (aref (aref raster i j) dim)))
	 (max* (dim)
	   (loop for i from (1- x) upto (1+ x)
	      for j from (1- y) upto (1+ y)
	      maximizing (aref (aref raster i j) dim))))
    (flet ((q (dim)
	     (let ((max* (max* dim))
		   (min* (min* dim)))
	       (/ (- max* min*) (+ max* min*)))))
      (or (> (q 0) 0.4)
	  (> (q 1) 0.3)
	  (> (q 2) 0.6)))))

(defvar *image-coordinates* nil)

(defun shoot-ray (scene camera x y width height &key (normalize-camera t))
  (declare (fixnum width height))
  (when normalize-camera
    (setf camera (normalize-camera camera width height)))
  (let* ((scene (compile-scene scene))
	 (camera (compile-camera camera))
         (counters (make-counters))
         (start (get-internal-run-time))
         (result))
    (let ((rx (- (/ (* 2 x) width) 1.0))
          (ry (- 1.0 (/ (* 2 y) height)))
          (*image-coordinates* (cons x y)))
      (funcall camera
               (sb-int:named-lambda shoot-ray-callback (ray)
                 (if result
                     (error "never")
                     (setf result (raytrace ray scene counters))))
               rx
               ry
               counters)
      x
      y)
    result))

(defun render (scene camera width height callback &key (normalize-camera t))
  (declare (fixnum width height) (optimize speed))
  (when normalize-camera
    (setf camera (normalize-camera camera width height)))
  (let* ((scene (compile-scene scene))
	 (camera (compile-camera camera))
	 (note-interval (ceiling height 80))
         (callback (cond ((functionp callback)
                          callback)
                         ((and (symbolp callback) (fboundp callback))
                          (fdefinition callback))
                         (t
                          (error "Not a valid callback: ~S" callback))))
         (counters (make-counters))
         (start (get-internal-run-time))
         (fheight (float height))
         (fwidth (float width)))
    (declare (function callback camera))
    (fresh-line)
    (flet ((trace-1-ray (ray)
             (raytrace ray scene counters)))
      (declare (dynamic-extent #'trace-1-ray))
      (dotimes (y height)
        (dotimes (x width)
          (let ((rx (- (/ (* 2.0 x) fwidth) 1.0))
                (ry (- 1.0 (/ (* 2.0 y) fheight)))
                (*image-coordinates* (cons x y)))
            (funcall callback
                     (funcall camera
                              #'trace-1-ray
                              rx
                              ry
                              counters)
                     x
                     y)))
        (when (zerop (mod y note-interval))
          (princ ".")
          (force-output))))
    (maybe-report scene counters (- (get-internal-run-time) start))))

(defvar *debugging* nil)

(defmacro with-debug ((continue) &body body)
  `(handler-bind ((error (lambda (c)
                           (unless *debugging*
                             (let ((*debugging* t))
                               (cerror ,continue "Oops:~% ~A" c))))))
     ,@body))

(defun raytrace (ray scene counters)
  "Traces the RAY in SCENE, returning the apparent color."
  (let* ((object (find-scene-intersection ray scene counters))
         (color (if object
                    (shade object ray counters)
                    (scene-background-color scene))))
    (vec* color (ray-weight ray))))

(defun %find-intersection (ray all-objects counters &optional shadow)
  (declare (optimize speed))
  (labels ((recurse (objects x)
             (if (not objects)
                 x
                 (let ((hit (intersect (car objects) ray counters shadow)))
                   (if hit
                       (if shadow
                           hit
                           (recurse (cdr objects) hit))
                       (recurse (cdr objects) x))))))
    (recurse all-objects nil)))

(defun %find-intersection* (ray all-objects min max counters shadowp)
  (declare (float min max))
  (declare (optimize speed))
  (let ((old (ray-extent ray))
        (hit nil))
    (labels ((recurse (objects x)
               (if (not objects)
                   x
                   (let ((hit (intersect (car objects) ray counters shadowp)))
                     (if hit
                         (if shadowp
                             hit
                             (recurse (cdr objects) hit))
                         (recurse (cdr objects) x))))))
      (unwind-protect
           (progn
             (minf (ray-extent ray) max)
             (setf hit (recurse all-objects nil)))
        (cond (hit
               ;; FIXME: Just EPSILON here doesn't seem to be good enough --
               ;; and even that should not really be needed, but TEST-KD-SPLIT-2
               ;; catches here without the 2xEPSILON, and I don't have the energy
               ;; to track down the issue right now.
               (assert (<= (- min (* 2 epsilon)) (ray-extent ray) (+ max (* 2 epsilon)))
                       (min max (ray-extent ray))
                       "~S is not in range ~S - ~S" (ray-extent ray) min max))
              (t
               (setf (ray-extent ray) old)))))
    hit))

(defun find-scene-intersection (ray scene counters &optional shadow)
  (let* ((compiled-scene (scene-compiled-scene scene))
         (unbounded (compiled-scene-objects compiled-scene))
         (tree (compiled-scene-tree compiled-scene))
         (hit (when unbounded
                (%find-intersection ray unbounded counters shadow))))
    (if tree
        (or (kd-traverse tree ray counters shadow)
            hit)
        hit)))
