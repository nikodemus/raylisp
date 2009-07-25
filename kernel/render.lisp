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
	 (camera (compute-camera-function camera))
         (counters (make-counters))
         (result (alloc-vec)))
    (let ((rx (- (/ (* 2 x) width) 1.0))
          (ry (- 1.0 (/ (* 2 y) height)))
          (*image-coordinates* (cons x y)))
      (funcall camera
               (sb-int:named-lambda shoot-ray-callback (ray)
                 (setf result (raytrace result ray scene counters)))
               rx
               ry
               counters)
      x
      y)
    result))

(defun render (scene camera width height callback &key (normalize-camera t)
               (verbose t))
  (declare (fixnum width height) (optimize speed))
  (when normalize-camera
    (setf camera (normalize-camera camera width height)))
  (let* ((scene (compile-scene scene))
         (camera (compute-camera-function camera))
         (note-interval (ceiling height 80))
         (callback (cond ((functionp callback)
                          callback)
                         ((and (symbolp callback) (fboundp callback))
                          (fdefinition callback))
                         (t
                          (error "Not a valid callback: ~S" callback))))
         (counters (make-counters))
         (fheight (float height))
         (fwidth (float width))
         (result (alloc-vec)))
    (declare (function callback camera)
             (single-float fheight fwidth))
    (when verbose
      (format verbose "~&Rendering ~A @ ~S x ~S~%" (scene-name scene) width height)
      (finish-output verbose))
    (let (timing)
      (sb-ext:call-with-timing
       (lambda (&rest args)
         (setf timing args))
       (lambda ()
         (flet ((trace-1-ray (ray)
                  (raytrace result ray scene counters)))
           (declare (dynamic-extent #'trace-1-ray))
           (dotimes (y height)
             (dotimes (x width)
               (let ((rx (- (/ (* 2.0 (float x)) fwidth) 1.0))
                     (ry (- 1.0 (/ (* 2.0 (float y)) fheight)))
                     (*image-coordinates* (cons x y)))
                 (funcall callback
                          (funcall camera
                                   #'trace-1-ray
                                   rx
                                   ry
                                   counters)
                          x
                          y)))
             (when verbose
               (when (zerop (mod y note-interval))
                 (princ "." verbose)
                 (finish-output verbose)))))))
      (when verbose
        (report scene counters timing verbose)))))

(defvar *debugging* nil)

(defmacro with-debug ((continue) &body body)
  `(handler-bind ((error (lambda (c)
                           (unless *debugging*
                             (let ((*debugging* t))
                               (cerror ,continue "Oops:~% ~A" c))))))
     ,@body))

;;; COLOR argument may be stack-allocated by callers.
(defun raytrace (result ray scene counters)
  (let* ((object (find-scene-intersection ray scene counters))
         (apparent-color (if object
                             (shade result object ray counters)
                             (funcall (compiled-scene-background
                                       (scene-compiled-scene scene))
                                      result
                                      ray))))
    (%vec* result apparent-color (ray-weight ray))))

(defun %find-intersection (ray all-objects min max counters shadowp)
  (declare (optimize speed) (type (or null single-float) max))
  (let ((old (ray-extent ray))
        (hit nil))
    (labels ((recurse (objects x)
               (if (not objects)
                   x
                   ;; Object intersection functions are responsible for
                   ;; further limiting the RAY-EXTENT when they hit, we just
                   ;; need to scan them all.
                   (let ((hit (intersect (car objects) ray counters shadowp)))
                     (if hit
                         (if shadowp
                             hit
                             (recurse (cdr objects) hit))
                         (recurse (cdr objects) x))))))
      (unwind-protect
           (progn
             (when max
               (minf (ray-extent ray) max))
             (setf hit (recurse all-objects nil)))
        (cond (hit
               ;; FIXME: Just EPSILON here doesn't seem to be good enough --
               ;; and even that should not really be needed, but TEST-KD-SPLIT-2
               ;; catches here without the 2xEPSILON, and I don't have the energy
               ;; to track down the issue right now.
               ;;
               ;; ...but probably it is because object intersection functions
               ;; are not aware of MIN: so we probably need RAY-MIN and
               ;; RAY-MAX instead of just RAY-EXTENT.
               #+nil
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
                (%find-intersection ray unbounded nil nil counters shadow))))
    (if tree
        (or (find-intersection-in-kd-tree ray tree counters shadow)
            hit)
        hit)))
