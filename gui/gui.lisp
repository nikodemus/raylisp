(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM")
  (:import-from "RAYLISP"
                "V"
                "+ORIGIN+")
  (:export "RUN"))

(in-package "RAYLISP-GUI")

(defun make-rgba-raster (width height)
  (make-array (list height width) :element-type '(unsigned-byte 32)))

(defun raster-height (raster)
  (array-dimension raster 0))

(defun raster-width (raster)
  (array-dimension raster 1))

(defun vec-rgba (vector)
  (declare (type raylisp::vec vector) (optimize speed))
  (flet ((dim (i)
           (floor (* 255 (raylisp::clamp (aref vector i) 0.0 1.0)))))
    (let ((r (dim 0))
          (g (dim 1))
          (b (dim 2)))
      (logior (ash r 16) (ash g 8) b))))

(defun rgba-vec (rgba)
  (v (/ (ldb (byte 8 16) rgba) 255.0)
     (/ (ldb (byte 8 8) rgba) 255.0)
     (/ (ldb (byte 8 0) rgba) 255.0)))

(defparameter *canvas-height* 400)
(defparameter *canvas-width* 600)

(define-application-frame raylisp-frame ()
  ()
  (:panes
   (canvas :application :display-time nil
           :height *canvas-height* :width *canvas-width*)
   (repl :interactor :min-height 500))
  (:layouts
   (default (vertically (:width *canvas-width* :height (round (* 1.5 *canvas-height*)))
              (2/3 canvas)
              (:fill repl)))))

(defmethod frame-standard-output ((frame raylisp-frame))
  ;; By default, send output to Slime.
  (swank::connection.user-output swank::*emacs-connection*))

(defun find-repl ()
  (find-pane-named *application-frame* 'repl))

(defun render-scene (scene sheet &key min max)
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (min (or min (cons 0 0)))
         (max (or max (cons width height)))
         (row-offset (car min))
         (row-stop (- (car max) 1))
         (row (make-rgba-raster (1+ (- row-stop row-offset)) 1))
         (row-data (sb-ext:array-storage-vector row))
         (end 0))
    (declare (type (simple-array (unsigned-byte 32) (*)) row-data))
    (declare (optimize speed))
    (declare (fixnum row-stop row-offset end))
    (let ((*standard-output* (find-repl)))
      (raylisp::render scene (raylisp::scene-default-camera scene)
                       width height
                       (lambda (color i j)
                         (declare (type sb-cga:vec color)
                                  (type fixnum i j)
                                  (optimize speed))
                         (setf end (max end i))
                         ;; FIXME: Gamma...
                         (setf (aref row-data (- i row-offset)) (vec-rgba color))
                         (when (= i row-stop)
                           (medium-draw-pixels* sheet row row-offset j)))
                       :normalize-camera t
                       :min min
                       :max max
                       :verbose (find-repl)))))

(defun shoot-ray-into-scene (scene sheet x y)
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (old (canvas-color sheet x y))
         (s (find-repl)))
    (format s "~&Shooting ray into ~A @~Sx~S point ~S,~S~%" (raylisp::scene-name scene) width height x y)
    (let ((color (raylisp::shoot-ray scene (raylisp::scene-default-camera scene)
                               x y width height
                               :normalize-camera t)))
      (format s "~&Previous color: #x~X, current color: #x~X~%"
              (ldb (byte 24 0) old)
              (ldb (byte 24 0) (vec-rgba color))))))

(define-raylisp-frame-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-raylisp-frame-command (com-update-raylisp :name t)
    ()
  (require :raylisp))

(define-raylisp-frame-command (com-set-gc-threshold :name t)
    ()
  (let ((mb (accept 'integer :prompt "Mb consed between GCs")))
    (setf (sb-ext:bytes-consed-between-gcs) (* 1024 1024 (abs mb)))))

(define-raylisp-frame-command (com-clear-canvas :name t :menu t)
    ()
  (window-clear (find-pane-named *application-frame* 'canvas)))

(define-raylisp-frame-command (com-clear-repl :name t :menu t)
    ()
  (window-clear (find-pane-named *application-frame* 'repl)))

(define-raylisp-frame-command (com-start-profiling :name t)
    ()
  (sb-sprof:reset)
  (sb-sprof:start-profiling :sample-interval 0.01))

(define-raylisp-frame-command (com-start-alloc-profiling :name t)
    ()
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode :alloc))

(define-raylisp-frame-command (com-report :name t)
    ()
  (sb-sprof:stop-profiling)
  (sb-sprof:report :stream sb-sys:*stdout*))

(defvar *last-scene-name* nil)

(define-raylisp-frame-command (com-render-scene :name t)
    ()
  (loop
    (fresh-line)
    (let* ((name (accept 'string :prompt "Scene Name"))
           (scene (gethash (setf *last-scene-name* (intern (string-upcase name) :raylisp))
                           raylisp::*scenes*))
           (s (find-repl)))
      (if scene
          (loop (with-simple-restart (retry "Try rendering ~A again." name)
                  (return-from com-render-scene
                    (render-scene scene (find-pane-named *application-frame* 'canvas)))))
          (format s "No scene named ~S found." name)))))

(define-raylisp-frame-command (com-render-all :name t)
    ()
  (maphash (lambda (name scene)
             (declare (ignore name))
             (render-scene scene (find-pane-named *application-frame* 'canvas)))
           raylisp::*scenes*))

(define-raylisp-frame-command (com-list-scenes :name t)
    ()
  (let ((s (find-repl)))
    (maphash (lambda (name scene)
               (declare (ignore scene))
               (format s "~&~A~%" name))
             raylisp::*scenes*)))

(define-raylisp-frame-command (com-stress :name t)
    ()
  (loop
    (maphash (lambda (name scene)
               (declare (ignore name))
               (render-scene scene (find-pane-named *application-frame* 'canvas)))
             raylisp::*scenes*)))

(defun canvas-color (canvas x y)
  (with-sheet-medium (medium canvas)
    (aref (medium-get-pixels* medium nil x y :width 1 :height 1)
          0 0)))

(define-raylisp-frame-command (com-shoot-ray :name t)
    ()
  (let* ((name *last-scene-name*)
         (scene (gethash name raylisp::*scenes*))
         (canvas (find-pane-named *application-frame* 'canvas))
         (s (find-repl)))
    (cond (scene
           (block point
             (format s "~&Click on the canvas to shoot a ray at that point.~%")
             (tracking-pointer (*standard-output*)
               (:pointer-button-press (&key event x y)
                                      (when (eq canvas (event-sheet event))
                                        (return-from point
                                          (shoot-ray-into-scene scene canvas x y)))))))
          (t
           (if name
               (format s "Oops: scene ~S seems to have vanished!~%" name)
               (format s "No last scene to shoot a ray into!~%"))))))

(define-raylisp-frame-command (pick-color :name t)
    ()
  (let ((canvas (find-pane-named *application-frame* 'canvas))
        (s (find-repl)))
    (block point
      (format s "~&Click on canvas to select a color, outside to stop.")
      (tracking-pointer (*standard-output*)
        (:pointer-button-press (event x y)
                               (if (eq canvas (event-sheet event))
                                   (let ((rgba (canvas-color canvas x y)))
                                     (format s "~&#x~X = ~S" (ldb (byte 24 0) rgba) (rgba-vec rgba)))
                                   (return-from point nil)))))
    t))

(define-raylisp-frame-command (rerender-region :name t)
    ()
  (let* ((name *last-scene-name*)
         (scene (gethash name raylisp::*scenes*))
         (canvas (find-pane-named *application-frame* 'canvas))
         (s (find-repl))
         p1 p2)
    (cond (scene
           (block point
             (format s "~&Click on to select corners of region to render, ~
                        outside canvas to abort.")
             (tracking-pointer (*standard-output*)
               (:pointer-button-press (event x y)
                                      (if (eq canvas (event-sheet event))
                                          (cond (p1
                                                 (format s "~&~S, ~S selected" x y)
                                                 (setf p2 (cons x y))
                                                 (return-from point))
                                                (t
                                                 (format s "~&~S, ~S selected" x y)
                                                 (setf p1 (cons x y))))
                                          (return-from point)))))
           (cond ((and p1 p2)
                  (let ((minx (min (car p1) (car p2)))
                        (maxx (max (car p1) (car p2)))
                        (miny (min (cdr p1) (cdr p2)))
                        (maxy (max (cdr p1) (cdr p2))))
                    (draw-rectangle* canvas minx miny maxx maxy :ink +red+
                                     :filled nil
                                     :line-thickness 1)
                    (render-scene scene canvas
                                  :min (cons minx miny)
                                  :max (cons (1+ maxx) (1+ maxy)))))
                 (t
                  (format s "~&Aborted."))))
          (t
           (format s "~&No scene to rerender found.")))))

(define-raylisp-frame-command (com-toggle-kd :name t)
    ()
  (let ((s (find-repl)))
    (if (setf raylisp::*use-kd-tree* (not raylisp::*use-kd-tree*))
        (format s "~&KD tree now in use.~%")
        (format s "~&KD tree now not in use.~%"))))

(define-raylisp-frame-command (com-again :name t)
    ()
  (let* ((name *last-scene-name*)
         (scene (gethash name raylisp::*scenes*))
         (s (find-repl)))
    (cond (scene
           (render-scene scene (find-pane-named *application-frame* 'canvas)))
          (t
           (format s "No scene named ~A" name)))))

(defun run ()
  (sb-posix:putenv "DISPLAY=:0.0")
  (run-frame-top-level (make-application-frame 'raylisp-frame)))

#+nil
(run)
