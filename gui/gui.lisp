(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM")
  (:import-from "RAYLISP"
                "@"
                "ORIGIN")
  (:export "RUN"))

(in-package "RAYLISP-GUI")

(defun make-rgba-raster (width height)
  (make-array (list height width) :element-type '(unsigned-byte 32)))

(defun raster-height (raster)
  (array-dimension raster 0))

(defun raster-width (raster)
  (array-dimension raster 1))

(defun vector-rgba (vector)
  (declare (type raylisp::vec vector) (optimize speed))
  (flet ((dim (i)
           (floor (* 255 (raylisp::clamp (aref vector i) 0.0 1.0)))))
    (let ((r (dim 0))
          (g (dim 1))
          (b (dim 2)))
      (logior (ash r 16) (ash g 8) b))))

(defparameter *canvas-height* 400)
(defparameter *canvas-width* 600)

(define-application-frame raylisp-frame ()
  ((image :initform (make-rgba-raster *canvas-height* *canvas-width*)
          :accessor frame-image))
  (:panes
   (canvas :application :display-time nil
           :height *canvas-height* :width *canvas-width*)
   (repl :interactor :min-height 500))
  (:layouts
   (default (vertically (:width *canvas-width* :height (round (* 1.5 *canvas-height*)))
              (2/3 canvas)
              (:fill repl)))))

(defun render-scene (scene sheet)
  (declare (optimize speed))
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (end (- width 1))
         (row (make-rgba-raster width 1))
         (row-data (sb-ext:array-storage-vector row)))
    (declare (type (simple-array (unsigned-byte 32) (*)) row-data))
    (declare (fixnum end width height))
    (format t "~&Rendering ~S @ ~S x ~S~%" (raylisp::scene-name scene) width height)
    (finish-output t)
    (raylisp::render scene (raylisp::scene-default-camera scene)
                     width height
                     (lambda (color x y)
                       (declare (type sb-cga:vec color)
                                (type fixnum x y)
                                (optimize speed))
                       ;; FIXME: Gamma...
                       (setf (aref row-data x) (vector-rgba color))
                       (when (= x end)
                         (medium-draw-pixels* sheet row 0 y)))
                     :normalize-camera t)))

(let ((m (make-array 12))
      (m2 (make-array '(3 4)))
      (n 0))
  (dotimes (x 4)
    (dotimes (y 3)
      (setf (aref m (+ x (* y 4)))  (cons y x))
      (setf (aref m2 y x) (cons y x))))
  (list m (sb-ext:array-storage-vector m2)))

(defun shoot-ray-into-scene (scene sheet x y)
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (old (aref (frame-image (pane-frame sheet)) y x)))
    (format t "~&Shooting ray into ~S @~Sx~S point ~S,~S~%" (raylisp::scene-name scene) width height x y)
    (let ((color (raylisp::shoot-ray scene (raylisp::scene-default-camera scene)
                               x y width height
                               :normalize-camera t)))
      (format t "~&Previous color: #x~X, current color: #x~X~%" old (vector-rgba color)))))

(defmethod frame-standard-output ((frame raylisp-frame))
  (find-pane-named frame 'repl))

(define-raylisp-frame-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-raylisp-frame-command (com-update-raylisp :name t)
    ()
  (require :raylisp))

(define-raylisp-frame-command (com-clear-canvas :name t)
    ()
  (window-clear (find-pane-named *application-frame* 'canvas)))

(define-raylisp-frame-command (com-clear-repl :name t)
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
  (sb-sprof:report :type :flat))

(defvar *last-scene-name* nil)

(define-raylisp-frame-command (com-render-scene :name t)
    ()
  (loop
    (fresh-line)
    (let* ((name (accept 'string :prompt "Scene Name"))
           (scene (gethash (setf *last-scene-name* (intern (string-upcase name) :raylisp))
                           raylisp::*scenes*)))
      (if scene
          (loop (with-simple-restart (retry "Try rendering ~A again." name)
                  (return-from com-render-scene
                    (render-scene scene (find-pane-named *application-frame* 'canvas)))))
          (format t "No scene named ~S found." name)))))

(define-raylisp-frame-command (com-render-all :name t)
    ()
  (maphash (lambda (name scene)
             (render-scene scene (find-pane-named *application-frame* 'canvas)))
           raylisp::*scenes*))

(define-raylisp-frame-command (com-list-scenes :name t)
    ()
  (maphash (lambda (name scene)
             (declare (ignore scene))
             (format t "~&~A~%" name))
           raylisp::*scenes*))

(define-raylisp-frame-command (com-stress :name t)
    ()
  (loop
    (maphash (lambda (name scene)
               (render-scene scene (find-pane-named *application-frame* 'canvas)))
             raylisp::*scenes*)))

(define-raylisp-frame-command (com-shoot-ray :name t)
    ()
  (let* ((name *last-scene-name*)
         (scene (gethash name raylisp::*scenes*))
         (canvas (find-pane-named *application-frame* 'canvas)))
    (cond (scene
           (block point
             (format t "~&Click on the canvas to shoot a ray at that point.~%")
             (tracking-pointer (*standard-output*)
               (:pointer-button-press (&key event x y)
                                      (when (eq canvas (event-sheet event))
                                        (return-from point
                                          (shoot-ray-into-scene scene canvas x y)))))))
          (t
           (if name
               (format t "Oops: scene ~S seems to have vanished!~%" name)
               (format t "No last scene to shoot a ray into!~%"))))))

(define-raylisp-frame-command (com-toggle-kd :name t)
    ()
  (if (setf raylisp::*use-kd-tree* (not raylisp::*use-kd-tree*))
      (format t "~&KD tree now in use.~%")
      (format t "~&KD tree now not in use.~%")))

(define-raylisp-frame-command (com-again :name t)
    ()
  (let* ((name *last-scene-name*)
         (scene (gethash name raylisp::*scenes*)))
    (cond (scene
           (render-scene scene (find-pane-named *application-frame* 'canvas)))
          (t
           (format t "No scene named ~A" name)))))

(defun run ()
  (sb-posix:putenv "DISPLAY=:0.0")
  (run-frame-top-level (make-application-frame 'raylisp-frame)))

#+nil
(run)
