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

(defclass canvas-pane (application-pane)
  ((raster
    :initform nil
    :accessor canvas-raster)
   (pixmap
    :initform nil
    :accessor canvas-pixmap)
   (selection
    :initform nil
    :accessor canvas-selection)
   (selection-in-progress
    :initform nil
    :accessor canvas-selection-in-progress)))

(defmethod canvas-raster :around ((canvas canvas-pane))
  (let ((raster (call-next-method))
        (w (bounding-rectangle-width canvas))
        (h (bounding-rectangle-height canvas)))
    (unless (and raster (= (raster-width raster) w) (= (raster-height raster) h))
      ;; FIXME: Copy the old raster, scaling contents.
      (setf raster (make-rgba-raster w h)
            (canvas-raster canvas) raster))
    raster))

(defmethod canvas-pixmap :around ((canvas canvas-pane))
  (let ((pixmap (call-next-method))
        (w (bounding-rectangle-width canvas))
        (h (bounding-rectangle-height canvas)))
    (unless (and pixmap (= (pixmap-width pixmap) w) (= (pixmap-height pixmap) h))
      ;; FIXME: Copy the old pixmap, scaling contents.
      (when pixmap
        (deallocate-pixmap pixmap))
      (setf pixmap (allocate-pixmap canvas w h)
            (canvas-pixmap canvas) pixmap))
    pixmap))

(defun canonicalize-selection (selection bounds)
  (when selection
    (let* ((width (bounding-rectangle-width bounds))
           (height (bounding-rectangle-height bounds)))
      (destructuring-bind ((minx . miny) (maxx . maxy)) selection
        (list (cons (max 0 (min minx maxx))
                    (max 0 (min miny maxy)))
              (cons (min (- width 1) (max minx maxx))
                    (min (- height 1) (max miny maxy))))))))

(defmethod (setf canvas-selection) :around (selection canvas)
  (call-next-method (canonicalize-selection selection canvas) canvas))

(defmethod (setf canvas-selection-in-progress) :around (selection canvas)
  (call-next-method (canonicalize-selection selection canvas) canvas))

(defun canvas-current-selection-extents (canvas)
  (let ((selection (or (canvas-selection-in-progress canvas)
                       (canvas-selection canvas))))
    (if selection
        (destructuring-bind ((minx . miny) (maxx . maxy)) selection
          (values minx miny maxx maxy t))
        (values nil nil nil nil nil))))

(defun paint-selection-border (canvas)
  (multiple-value-bind (min-x min-y max-x max-y ok) (canvas-current-selection-extents canvas)
    (when ok
      (with-output-recording-options (canvas :record nil :draw t)
        (draw-rectangle* canvas
                         min-x min-y
                         max-x max-y
                         :filled nil
                         :ink +red+
                         :line-thickness 1)))))

(defmethod handle-repaint :after ((canvas canvas-pane) region)
  (with-slots (pixmap) canvas
    (when pixmap
      (copy-from-pixmap pixmap 0 0 (pixmap-width pixmap) (pixmap-height pixmap)
                        canvas 0 0))
    (paint-selection-border canvas)))

(defmethod dispatch-event ((canvas canvas-pane) (event pointer-button-press-event))
  (handle-event canvas event))

(defun find-common-ancestor (sheet1 sheet2)
  (if (sheet-ancestor-p sheet1 sheet2)
      sheet2
      (find-common-ancestor sheet1 (sheet-parent sheet2))))

(defun map-sheet-position-to-ancestor (sheet ancestor x y)
  (if (eq sheet ancestor)
      (values x y)
      (multiple-value-bind (xt yt) (map-sheet-position-to-parent sheet x y)
        (map-sheet-position-to-ancestor (sheet-parent sheet) ancestor xt yt))))

(defun map-sheet-position-from-ancestor (sheet ancestor x y)
  (if (eq sheet ancestor)
      (values x y)
      (multiple-value-bind (xt yt)
          (map-sheet-position-from-ancestor (sheet-parent sheet) ancestor x y)
        (map-sheet-position-to-child sheet xt yt))))

(defun map-sheet-position-via-ancestor (source target source-x source-y)
  (if (eq source target)
      (values source-x source-y)
      (let ((ancestor (find-common-ancestor source target)))
        (multiple-value-bind (xa ya)
            (map-sheet-position-to-ancestor source ancestor source-x source-y)
          (map-sheet-position-from-ancestor target ancestor xa ya)))))

(defmethod handle-event ((canvas canvas-pane) (start-event pointer-button-press-event))
  (let ((start (cons (pointer-event-x start-event)
                     (pointer-event-y start-event))))
    (setf (canvas-selection-in-progress canvas) (list start start))
    (tracking-pointer (canvas)
      (:pointer-motion
       (&key x y window)
       (multiple-value-bind (x y)
           (map-sheet-position-via-ancestor window canvas x y)
         (setf (canvas-selection-in-progress canvas) (list start (cons x y))))
       (repaint-sheet canvas +everywhere+))
      (:pointer-button-release
       (&key x y window)
       (multiple-value-bind (x y)
           (map-sheet-position-via-ancestor window canvas x y)
         (setf (canvas-selection canvas) (list start (cons x y))
               (canvas-selection-in-progress canvas) nil))
       (repaint-sheet canvas +everywhere+)
       (return-from handle-event)))))

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
              (2/3 canvas)
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
         (x-min (car min))
         (area-width (- (car max) x-min))
         (row-stop (- (car max) 1))
         (raster (canvas-raster canvas))
         (pixmap (canvas-pixmap canvas)))
    (declare (type (simple-array (unsigned-byte 32) (* *)) raster))
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
                               (setf (aref raster j i) (vec-rgba color))
                               (when (= i row-stop)
                                 (medium-draw-pixels* canvas raster x-min j
                                                      :src-x x-min :src-y j
                                                      :width area-width :height 1)
                                 (copy-to-pixmap canvas x-min j area-width 1
                                                 pixmap x-min j)
                                 (paint-selection-border canvas)))
                             :normalize-camera t
                             :min min
                             :max max
                             :verbose (find-repl))))))
    (setf (canvas-raster canvas) raster
          (canvas-pixmap canvas) pixmap)))

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

(define-raylisp-frame-command (com-clear-canvas :name t)
    ()
  (let ((canvas (find-canvas)))
    (setf (canvas-raster canvas) nil)
    (repaint-sheet canvas +everywhere+)))

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

(defun canvas-color (canvas x y)
  (with-sheet-medium (medium canvas)
    (aref (medium-get-pixels* medium nil x y :width 1 :height 1)
          0 0)))

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
                                   (let ((rgba (canvas-color canvas x y)))
                                     (format s "~&#x~X = ~S" (ldb (byte 24 0) rgba) (rgba-vec rgba)))
                                   (return-from point nil)))))
    t))

(define-raylisp-frame-command (clear-selection :name t)
    ()
  (let ((canvas (find-canvas)))
    (setf (canvas-selection canvas) nil
          (canvas-selection-in-progress canvas) nil)
    (repaint-sheet canvas +everywhere+)))

(define-raylisp-frame-command (render-selection :name t)
    ()
  (let* ((canvas (find-canvas))
         (scene (ensure-scene))
         (selection (canvas-selection canvas)))
    (if selection
      (destructuring-bind (start end) selection
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
