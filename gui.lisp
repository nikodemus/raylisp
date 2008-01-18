(require :mcclim)

(in-package "RAYLISP")

(setf (gethash "sphere-1" raylisp-gui::*scenes*)
      (make-scene 
       :objects 
       (list 
        (make-instance 'sphere
                       :shader
                       (make-instance
                        'composite
                        :shaders
                        (list
                         (make-instance 'raytrace
                                        :specular 0.1
                                        :transmit 0.9
                                        :ior 1.6)
                         (make-instance 'phong
                                        :specular 0.5
                                        :size 40.0
                                        :diffuse 0.3
                                        :ambient 0.1
                                        :color yellow))))
        (make-instance 
         'plane
         :location (@ 0 -1 0)
         :shader
         (make-instance 'checker
                        :odd
                        (make-instance 'phong :color black)
                        :even
                        (make-instance 'phong :color white
                                       :ambient 0.1))))
       :lights (list 
                (make-instance 'spotlight
                               :location (@ -30 30 -30)
                               :direction (@ 30 -30 30)
                               :aperture 0.999))
       :background-color blue
       :ambient-light white
       :adaptive-limit 0.01
       :depth-limit 12))

(defvar *default-camera*
  (make-instance 'pinhole
                 :location (@ 0 0.5 -4)
                 :look-at origin
                 :focal-length 3.0))

(defpackage "RAYLISP-GUI"
  (:use "CLIM-LISP" "CLIM"))

(in-package "RAYLISP-GUI")

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

(define-raylisp-frame-command (com-test :name t)
    ()
  (render-test (find-pane-named *application-frame* 'canvas) 200 150))

(define-raylisp-frame-command (com-clear-canvas :name t)
    ()
  (window-clear (find-pane-named *application-frame* 'canvas)))

(defvar *scenes* (make-hash-table :test #'equalp))

(define-raylisp-frame-command (com-render-scene :name t)
    ()
  (let* ((name (accept 'string :prompt "Scene Name"))
         (scene (gethash name *scenes*)))
    (if scene
        (render-scene scene (find-pane-named *application-frame* 'canvas))
        (format t "No scene named ~S found." name))))

(run-frame-top-level (make-application-frame 'raylisp-frame))

(defun render-scene (scene medium)
  (let* ((pixmap nil)
         (width 400)
         (height 300)
         (end (- width 1)))
    (unwind-protect
         (progn
           (setf pixmap (allocate-pixmap medium width height))
           (raylisp::render scene raylisp::*default-camera* width height
                            (lambda (color x y)
                              (declare (type (simple-array single-float (3)) color)
                                       (type fixnum x y))
                              (draw-point* pixmap x y :ink (make-rgb-color (aref color 0) (aref color 1) (aref color 2)))
                              (when (= x end)
                                (copy-from-pixmap pixmap 0 y width 1 medium 0 y)))))
      (deallocate-pixmap pixmap))))

(defun render-test (medium width height)
)

((raylisp::test-render.7 400 300))