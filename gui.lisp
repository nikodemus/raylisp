(require :mcclim)

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM")
  (:import-from "RAYLISP" 
                "@"
                "ORIGIN"))

(in-package "RAYLISP-GUI")

(defun render-test ())

(defun render-scene (scene medium)
  (let* ((pixmap nil)
         (region (sheet-region medium))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (end (- width 1)))
    (unwind-protect
         (progn
           (setf pixmap (allocate-pixmap medium width height))
           (raylisp::render scene (raylisp::scene-default-camera scene)
                            width height
                            (lambda (color x y)
                              (declare (type (simple-array single-float (3)) color)
                                       (type fixnum x y))
                              (let ((rgb (make-rgb-color (aref color 0) (aref color 1) (aref color 2))))
                                (draw-point* pixmap x y :ink rgb)
                                (when (= x end)
                                  (copy-from-pixmap pixmap 0 y width 1 medium 0 y))))
                            :normalize-camera t))
      (when pixmap
        (deallocate-pixmap pixmap)))))

(define-application-frame raylisp-frame ()
  ()
  (:panes
   (canvas :application :display-time nil :height 400 :width 600)
   (repl :interactor))
  (:layouts
   (default (vertically (:width 600 :height 600)
              canvas
              (:fill repl)))))

(defmethod frame-standard-output ((frame raylisp-frame))
  (find-pane-named frame 'repl))

(define-raylisp-frame-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(define-raylisp-frame-command (com-clear-canvas :name t)
    ()
  (window-clear (find-pane-named *application-frame* 'canvas))
  (window-clear (find-pane-named *application-frame* 'repl)))

(define-raylisp-frame-command (com-render-scene :name t)
    ()
  (let* ((name (accept 'string :prompt "Scene Name"))
         (scene (gethash (intern (string-upcase name) :raylisp) raylisp::*scenes*)))
    (if scene
        (render-scene scene (find-pane-named *application-frame* 'canvas))
        (format t "No scene named ~S found." name))))

(define-raylisp-frame-command (com-render-all :name t)
    ()
  (maphash (lambda (name scene)
             (format t "Rendering ~A" name)
             (render-scene scene (find-pane-named *application-frame* 'canvas)))
           raylisp::*scenes*))

(define-raylisp-frame-command (com-stress :name t)
    ()
  (loop
    (maphash (lambda (name scene)
               (format t "Rendering ~A" name)
               (render-scene scene (find-pane-named *application-frame* 'canvas)))
             raylisp::*scenes*)))

#+nil
(run-frame-top-level (make-application-frame 'raylisp-frame))
