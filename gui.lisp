
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage "RAYLISP-GUI"
    (:use "CLIM-LISP" "CLIM")
    (:import-from "RAYLISP" 
                  "@"
                  "ORIGIN")
    (:export "RUN")))

(in-package "RAYLISP-GUI")

(defun render-scene (scene sheet)
  (let* ((region (sheet-region sheet))
         (width (bounding-rectangle-width region))
         (height (bounding-rectangle-height region))
         (end (- width 1))
         (image (make-array (list 1 width) :element-type '(unsigned-byte 32))))
    (raylisp::render scene (raylisp::scene-default-camera scene)
                     width height
                     (lambda (color x y)
                       (declare (type (simple-array single-float (3)) color)
                                (type fixnum x y))
                       ;; FIXME: Gamma...
                       (let ((r (floor (* 255 (aref color 0))))
                             (g (floor (* 255 (aref color 1))))
                             (b (floor (* 255 (aref color 2)))))
                         (setf (aref image 0 x) (logior (ash r 16) (ash g 8) b))
                         (when (= x end)
                           (with-sheet-medium (medium sheet)
                             (medium-draw-pixels* medium image 0 y)))))
                     :normalize-camera t)))

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

(define-raylisp-frame-command (com-toggle-kd :name t)
    ()
  (if (setf raylisp::*use-kd-tree* (not raylisp::*use-kd-tree*))
      (format t "~&KD tree in use.~%")
      (format t "~&KD tree not in used.~%")))


(defun run ()
  (run-frame-top-level (make-application-frame 'raylisp-frame)))
