(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM" "CANVAS-PANE")
  (:import-from "RAYLISP"
                "V"
                "+ORIGIN+")
  (:export "RUN"))

(in-package "RAYLISP-GUI")

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
   (canvas canvas-pane
           :display-time nil
           :height *canvas-height* :width *canvas-width*)
   (repl
    :interactor
    :min-height 500))
  (:layouts
   (default (vertically (:width *canvas-width* :height (round (* 1.5 *canvas-height*)))
              (3/5 canvas)
              (:fill repl)))))

(defmethod frame-standard-output ((frame raylisp-frame))
  ;; By default, send output to Slime.
  (swank::connection.user-output swank::*emacs-connection*))

(defun find-repl ()
  (find-pane-named *application-frame* 'repl))

(defun find-canvas ()
  (find-pane-named *application-frame* 'canvas))

(defun render-scene (scene canvas &key min max)
  (let* ((region (sheet-region canvas))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (min (or min (cons 0 0)))
         (max (or max (cons width height)))
         (row-stop (- (car max) 1)))
    (declare (optimize speed))
    (declare (fixnum row-stop))
    (let ((*standard-output* (find-repl)))
      (loop
        (with-simple-restart (retry "Try rendering ~A again."
                                    (or (raylisp::scene-name scene) "scene"))
          (return
            (raylisp::render scene (raylisp::scene-default-camera scene)
                             width height
                             (lambda (color i j)
                               (declare (type sb-cga:vec color)
                                        (type fixnum i j)
                                        (optimize speed))
                               ;; FIXME: Gamma...
                               (setf (canvas-rgba canvas i j) (vec-rgba color))
                               (when (= i row-stop)
                                 (repaint-sheet canvas (canvas-dirty-region canvas))))
                             :normalize-camera t
                             :min min
                             :max max
                             :verbose (find-repl))))))))

(defun shoot-ray-into-scene (scene sheet x y)
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (old (canvas-rgba sheet x y))
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

(define-raylisp-frame-command (com-clear-canvas :name t)
    ()
  (break "fixme"))

(define-raylisp-frame-command (com-clear-repl :name t)
    ()
  (window-clear (find-repl)))

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

(defun ensure-scene (&optional (name *last-scene-name*))
  (or (and name (gethash name raylisp::*scenes*))
      (loop
        (let* ((name (accept 'string :prompt "Scene Name"))
               (scene (gethash (setf *last-scene-name* (intern (string-upcase name) :raylisp))
                               raylisp::*scenes*)))
          (when scene
            (return scene))
          (format (find-repl) "No scene named ~S found." name)))))

(define-raylisp-frame-command (com-render-scene :name t)
    ()
  (render-scene (ensure-scene nil) (find-canvas)))

(define-raylisp-frame-command (com-render-all :name t)
    ()
  (maphash (lambda (name scene)
             (declare (ignore name))
             (render-scene scene (find-canvas)))
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
               (render-scene scene (find-canvas)))
             raylisp::*scenes*)))

(define-raylisp-frame-command (com-shoot-ray :name t)
    ()
  (let* ((scene (ensure-scene))
         (canvas (find-canvas))
         (s (find-repl)))
    (block point
      (format s "~&Click on the canvas to shoot a ray at that point.~%")
      (tracking-pointer (canvas)
        (:pointer-button-press (&key event x y)
                               (when (eq canvas (event-sheet event))
                                 (return-from point
                                   (shoot-ray-into-scene scene canvas x y))))))))

(define-raylisp-frame-command (pick-color :name t)
    ()
  (let ((canvas (find-canvas))
        (s (find-repl)))
    (block point
      (format s "~&Click on canvas to select a color, outside to stop.")
      (tracking-pointer (*standard-output*)
        (:pointer-button-press (event x y)
                               (if (eq canvas (event-sheet event))
                                   (let ((rgba (canvas-rgba canvas x y)))
                                     (format s "~&#x~X = ~S" (ldb (byte 24 0) rgba) (rgba-vec rgba)))
                                   (return-from point nil)))))
    t))

(define-raylisp-frame-command (com-clear-selection :name t)
    ()
  (let ((canvas (find-canvas)))
    (setf (canvas-selection canvas) nil)
    (repaint-sheet canvas +everywhere+)))

(define-raylisp-frame-command (render-selection :name t)
    ()
  (let* ((canvas (find-canvas))
         (scene (ensure-scene))
         (selection (canvas-selection canvas)))
    (if selection
      (destructuring-bind (start . end) selection
        (render-scene scene canvas :min start :max end))
      (format (find-repl) "~&No selection.~%"))))

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
           (render-scene scene (find-canvas)))
          (t
           (format s "No scene named ~A" name)))))

(defun run ()
  (sb-posix:putenv "DISPLAY=:0.0")
  (run-frame-top-level (make-application-frame 'raylisp-frame)))

#+nil
(run)
