(require :mcclim)

(in-package :climi)

;;;; Image drawing

(defgeneric clim::medium-draw-pixels* (medium array x y &key &allow-other-keys))
(defgeneric clim::medium-get-pixels* (medium array x y &key width height &allow-other-keys))

(export 'clim::medium-draw-pixels* :clim)
(export 'clim::medium-get-pixels*  :clim)

(in-package :clim-clx)

;;; TODO: Extract indexed pattern drawing and convert to one of these functions.

(defmethod clim::medium-draw-pixels* ((sheet sheet) array x y &key)
  (with-sheet-medium (medium sheet)
    (medium-draw-pixels* medium array x y)))

(defmethod clim::medium-draw-pixels* 
    ((medium clx-medium) array x y &key &allow-other-keys)
  (let* ((width  (array-dimension array 1))
         (height (array-dimension array 0))
         (image (xlib:create-image :width width :height height :data array
                                                               :bits-per-pixel 32
                                                               :depth 24
                                                               :format :z-pixmap)))
    (with-clx-graphics (medium)
      (xlib:put-image mirror gc image :x x :y y :width width :height height))))
 
(defmethod clim::medium-get-pixels* 
    ((medium clx-medium) array x y &key width height &allow-other-keys)
  (let* ((width  (or width (array-dimension array 1)))
         (height (or height (array-dimension array 0))))
    (with-clx-graphics (medium)
      (xlib:image-z-pixarray (xlib:get-image mirror :x x :y y :format :z-pixmap :width width :height height)))))

(in-package :cl-user)

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM" "CLIM-EXTENSIONS")
  (:import-from "RAYLISP" 
                "@"
                "ORIGIN"))

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

#+nil
(run-frame-top-level (make-application-frame 'raylisp-frame))
