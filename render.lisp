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

#+nil
(defun render (scene camera width height &optional supersamplep)
  (declare (fixnum width height))
  (let* ((raster (make-array (list width height)))
	 (scene (compile-scene scene))
	 (camera (compile-camera camera))
	 (note-interval (ceiling height 80)))
    (fresh-line)
    (start-counters)
    (dotimes (y height)
      (dotimes (x width)
	(setf (aref raster x y)
	      (raytrace (funcall camera 
                                 (- (/ (* 2 x) width) 1.0)
                                 (- (/ (* 2 y) height) 1.0))
                        scene)))
      (when (zerop (mod y note-interval))
	(princ ".")
	(force-output)))
    (stop-counters)
    (maybe-report scene)
    (values
     raster
     (when supersamplep
       (let ((raster2 (make-array (list width height))))
	 (start-counters)
	 (write-line "Supersampling")
	 (dotimes (y height)
	   (dotimes (x width)
	     (if (and (plusp x) (plusp y) (< x (1- width)) (< y (1- height))
		      (needs-supersampling-p raster x y))
		 (setf (aref raster2 x y)
		       (vector-div
			(let ((co (vector -0.5 0.0 0.5))
                              (color (vector 0.0 0.0 0.0)))
                          (dotimes (i 3 color)
                            (dotimes (j 3)
                              (let ((u (aref co i)) (v (aref co j)))
                                (setf color
                                      (vector-add
                                       (raytrace
                                        (funcall camera
                                                 (- (/ (* 2 (+ u x)) width) 1.0)
                                                 (- (/ (* 2 (+ v y)) height) 1.0))
                                        scene)
                                       color))))))
			9.0))
		 (setf (aref raster2 x y) (aref raster x y))))
	   (when (zerop (mod y note-interval))
	     (princ ".")
	     (force-output)))
	 (stop-counters)
	 (maybe-report scene)
	 raster2)))))

(defun render (scene camera width height callback &key (normalize-camera t))
  (declare (fixnum width height))
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
                          (error "Not a valid callback: ~S" callback)))))
    (declare (function callback))
    (fresh-line)
    (start-counters)
    (dotimes (y height)
      (dotimes (x width)
        (funcall callback
                 (raytrace (funcall camera 
                                    (- (/ (* 2 x) width) 1.0)
                                    (- (/ (* 2 y) height) 1.0))
                           scene)
                 x
                 y))
      (when (zerop (mod y note-interval))
	(princ ".")
	(force-output)))
    (stop-counters)
    (maybe-report scene)))

(defun raytrace (ray scene)
  "Traces the RAY in SCENE, returning the apparent color."
  (let* ((object (find-intersection ray scene))
         (color (if object
                    (shade object ray)
                    (scene-background-color scene))))
    (vector-mul color (ray-weight ray))))

(defun find-intersection (ray scene)
  (declare (optimize speed))
  (labels ((recurse (objects x)
	     (if (not objects)
		 x ; return
		 (multiple-value-bind (t? x?)  (intersect (car objects) ray)
		   (if t?
		       (recurse (cdr objects) x?)
		       (recurse (cdr objects) x))))))
    (recurse (compiled-scene-objects (scene-compiled-scene scene)) nil)))
