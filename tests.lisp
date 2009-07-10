(in-package :raylisp)

;; notional scale: 1 unit = 10cm

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((chessboard
          (make-instance 'checker
                         :odd
                         (make-instance 'phong :color black)
                         :even
                         (make-instance 'phong :color white :diffuse 1.0 :specular 1.0 :ambient 0.5)))
         (bright-red
          (make-instance 'phong :color red :diffuse 1.0 :specular 1.0 :ambient 0.5))
         (bright-blue
          (make-instance 'phong :color blue :diffuse 1.0 :specular 1.0 :ambient 0.5))
         (floor
          (make-instance 'plane :shader chessboard))
         (lamp
          (make-instance 'point-light :location (@ 0 30 0) :color white))
         (sun
          (make-instance 'solar-light :direction (@ 1 1 0) :color white))
         (view
          (make-instance 'pinhole
                         :location (@ 0 18 -30)
                         :look-at +origin+
                         :focal-length 4.0))
         (floor-view
          (make-instance 'pinhole
                         :location (@ 0 0 -30)
                         :look-at +origin+
                         :focal-length 4.0))
         (top-view
          (make-instance 'pinhole
                         :location (@ 0 18 0)
                         :look-at +origin+
                         :focal-length 4.0)))
    (defparameter *chessboard* chessboard)
    (defparameter *bright-red* bright-red)
    (defparameter *bright-blue* bright-blue)
    (defparameter *floor* floor)
    (defparameter *lamp* lamp)
    (defparameter *sun* sun)
    (defparameter *view* view)
    (defparameter *floor-view* floor-view)
    (defparameter *top-view* top-view)))

(defscene test-test-lab
  ;; Sanity check.
  (:objects
   *floor*)
  (:lights
   *lamp*)
  (:camera
   *view*))

(defun xtranslate* (x y z)
  (translate* (float x) (float y) (float z)))
(defun xscale* (x y z)
  (scale* (float x) (float y) (float z)))

(defscene test-kd-split-1
  (:objects
   (loop for j from -10 upto 10
         append (loop for i from -10 upto 10
                      collect (make-instance 'sphere
                                             :radius 1.0
                                             :transform (xtranslate* (* 0.5 i) 0 (* 0.5 j))
                                             :shader *bright-red*))))
  (:lights
   *lamp*)
  (:camera
   *view*))

(defscene test-kd-split-2
  (:objects
   (loop for j from -10 upto 10
         append (loop for i from -10 upto 10
                      collect (make-instance 'sphere
                                             :radius 1.0
                                             :transform (xtranslate* (* 2.5 i) 0 (* 2.5 j))
                                             :shader *bright-red*))))
  (:lights
   *lamp*)
  (:camera
   *view*))

(defscene test-spheres
  ;; All in a grid.
  (:objects
   *floor*
   (loop for i from -3 upto 3
         collect (make-instance 'sphere
                                :radius 1.0
                                :transform (xtranslate* (* 3 i) 1 0)
                                :shader *bright-red*))
   (loop for i from -3 upto 3
         collect (make-instance 'sphere
                                :radius 0.5
                                :location (@ (* 3 i) 3 0)
                                :shader *bright-red*))
   (loop for i from -3 upto 3
         collect (make-instance 'sphere
                                :radius 0.5
                                :transform (xtranslate* (* 3 i) 5 0)
                                :shader *bright-red*))
   (make-instance 'sphere
                  :transform (list (xscale* 5 0.5 0.5) (xtranslate* 0 2 -5))
                  :shader *bright-blue*))
  (:lights
   *lamp*)
  (:camera
   *view*))

(defscene test-sphere-intersection
  ;; No wierd shadows on the floor, etc.
  (:objects
   *floor*
   (make-instance 'csg
                  :type 'intersection
                  :objects (list (make-instance 'sphere 
                                                :location (@ 1.5 -0.1 0)
                                                :radius 4.0
                                                :shader *bright-red*)
                                 (make-instance 'sphere 
                                                :location (@ -1.5 -0.1 0)
                                                :radius 4.0
                                                :shader *bright-blue*))))
  (:lights
   *lamp*)
  (:camera
   *view*))

(defun v (x y z)
  (vec (float x) (float y) (float z)))

(defscene test-plane-intersection
  (:objects
   (make-instance
    'csg
    :type 'intersection
    :objects
    (list
     (make-instance 'plane
                    :normal (v 0 0 -1)
                    :location (@ 0 0 1)
                    :shader *chessboard*)
     (make-instance 'plane
                    :normal (v 1 1 0)
                    :location (@ 1 0 0)
                    :shader *bright-red*)
     (make-instance 'plane
                    :normal (v -1 1 0)
                    :location (@ -1 0 0)
                    :shader *bright-red*)
     *floor*)))
  (:lights
   *lamp*)
  (:camera
   *view*))

(defscene test-1
  (:objects
   (make-instance 'sphere
                  :shader (make-instance 'solid :color red))
   (make-instance 'sphere
                  :location (@ 0 -0.5 0)
                  :transform (scale (@ 3 0.5 0.5))
                  :shader
                  (make-instance
                   'gradient
                   :start (make-instance 'solid :color blue)
                   :end (make-instance 'solid :color green)
                   :axis 0
                   :scale 3.0)))
  (:lights
   (make-instance 'point-light
                  :location (@ 10 5 -20)))
  (:ambient-light (@ 0.1 0.1 0.1))
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 3 -20)
                     :look-at +origin+
                     :focal-length 4.0)))

(defscene test-2
  (:objects
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
                                   :ambient 0.2
                                   :color yellow))))
   (make-instance 'plane
                  :location (@ 0 -1 0)
                  :shader *chessboard*))
  (:lights
   (make-instance 'point-light
                  :location (@ -30 30 -30)))
  (:background-color blue)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 12)
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 0.5 -4)
                     :look-at +origin+
                     :focal-length 3.0)))

(defscene test-3
    (:objects
     (make-instance
      'csg
      :type 'intersection
      :objects (list
                (make-instance 'sphere
                               :location (@ -0.5 0 0)
                               :shader
                               (make-instance 'solid :color red))
                (make-instance 'sphere
                               :location (@ 0.5 0 0)
                               :shader
                               (make-instance 'solid
                                              :color blue))
                (make-instance 'sphere
                               :location (@ 0 -0.1 0)
                               :shader
                               (make-instance 'solid
                                              :color green)))))
  (:lights
   (make-instance 'solar-light :direction y-axis))
  (:background-color black)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 5)
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 1.5 -10)
                     :look-at +origin+
                     :focal-length 3.0)))

(defscene test-3.1
    (:objects
     (make-instance
      'csg
      :type 'intersection
      :objects (list
                (make-instance 'sphere
                               :location (@ -0.5 0 0)
                               :shader
                               (make-instance 'solid :color red))
                (make-instance 'sphere
                               :location (@ 0.5 0 0)
                               :shader
                               (make-instance 'solid
                                              :color blue)))))
  (:lights
   (make-instance 'solar-light :direction y-axis))
  (:background-color black)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 5)
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 1.5 -10)
                     :look-at +origin+
                     :focal-length 3.0)))

(defscene test-4
  (:objects
   (make-instance 'csg
                  :type 'difference
                  :objects
                  (list
                   (make-instance 'sphere
                                  :shader
                                  (make-instance 'solid :color red))
                   (make-instance 'sphere
                                  :location (@ 0 1 0)
                                  :shader
                                  (make-instance 'solid
                                                 :color blue))
                   (make-instance 'sphere
                                  :location (@ 1 0 0)
                                  :shader
                                  (make-instance 'solid
                                                 :color green)))))
  (:lights
   (make-instance 'solar-light :direction (v 1 1 0.5)))
  (:background-color black)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 5)
  (:camera
      (make-instance 'pinhole
                     :location (@ 8 6.5 -2)
                     :look-at +origin+
                     :focal-length 3.0)))

(defscene test-5
  (:objects
   #+nil
   (list   
    (make-instance 'sphere
                   :radius 1.0
                   :transform (translate (@ 1 0 0))
                   :shader (make-instance 'solid :color red))
    (make-instance 'sphere
                   :radius 1.0
                   :transform (translate (@ 0 1 0))
                   :shader (make-instance 'solid :color green))
    (make-instance 'sphere
                   :radius 1.0
                   :transform (translate (@ 0 0 1))
                   :shader (make-instance 'solid :color blue)))
   (make-instance 'plane
                  :normal (v -0.5 1 0)
                  :shader (make-instance 'solid :color white))   
   #+nil
   (loop for i from -100 upto 100
         collect (make-instance 'sphere
                                :radius 0.5
                                :transform (translate (@ 0 0 i))
                                :shader (make-instance 'solid :color purple)))
   (make-instance
    'csg
    :type 'intersection
    :objects
    (list
     (make-instance 'plane
                    :normal (v 0 1 0)
                    :location (@ 0 -1 0)
                    :shader (make-instance 'solid :color yellow))
     (make-instance 'plane
                    :normal (v 0.5 1 0)
                    :shader (make-instance 'solid :color white))
     (make-instance 'plane
                    :normal (v -0.5 1 0)
                    :shader (make-instance 'solid :color purple)))))
  (:lights
   #+nil
   (make-instance 'solar-light :direction y-axis)
   (make-instance 'point-light :location (@ 0 200 0)))
  (:background-color black)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 5)
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 100 30)
                     :look-at +origin+
                     :focal-length 3.0)))

(defvar *test-6-shader*
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

(defscene test-6
  (:objects
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -4 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -3 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -2 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -1 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -0 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 1 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 2 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 3 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 4 0 16)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -4 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -3 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -2 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -1 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -0 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 1 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 2 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 3 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 4 0 12)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -4 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -3 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -2 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -1 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -0 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 1 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 2 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 3 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 4 0 8)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -4 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -3 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -2 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -1 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ -0 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 1 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 2 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 3 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :radius 0.4
                  :location (@ 4 0 4)
                  :shader *test-6-shader*)
   (make-instance 'sphere
                  :shader *test-6-shader*)
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
  (:lights
   (make-instance 'spotlight
                  :location (@ -30 30 -30)
                  :direction (v 30 -30 35)
                  :aperture 0.98))
  (:background-color blue)
  (:ambient-light white)
  (:adaptive-limit 0.01)
  (:depth-limit 16)
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 0.5 -4)
                     :look-at +origin+
                     :focal-length 3.0)))

(defscene test-noise
  (:objects
   (make-instance 'sphere
                  :radius 3.0
                  :shader (make-instance 'noise-shader
                                         :start (make-instance 'solid :color (@ 0.2 0.2 1.0))
                                         :end (make-instance 'solid :color white)
                                         :scale 0.8)))
  (:lights
   (make-instance 'solar-light :direction (v -1 1 -1)))
  (:ambient-light (@ 0.1 0.1 0.1))
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 5 -5)
                     :look-at +origin+)))

(defscene test-x-axis-camera
  (:objects
   (make-instance 'sphere :shader (make-instance 'solid :color red))
   (make-instance 'plane
                  :normal x-axis
                  :location (@ -1 0 0)
                  :shader (make-instance 'checker
                                         :odd (make-instance 'phong :color black)
                                         :even (make-instance 'phong :color white :ambient 0.1))))
  (:lights
   (make-instance 'solar-light :direction (@ 1 1 1)))
  (:camera
      (make-instance 'pinhole
                     :location (@ 10 0 0)
                     :look-at +origin+)))

(defscene test-y-axis-camera
  (:objects
   (make-instance 'sphere :shader (make-instance 'solid :color green))
   (make-instance 'plane
                  :normal y-axis
                  :location (@ 0 -1 0)
                  :shader (make-instance 'checker
                                         :odd (make-instance 'phong :color black)
                                         :even (make-instance 'phong :color white :ambient 0.1))))
  (:lights
   (make-instance 'solar-light :direction (v 1 1 1)))
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 10 0)
                     :look-at +origin+)))

(defscene test-z-axis-camera
  (:objects
   (make-instance 'sphere :shader (make-instance 'solid :color blue))
   (make-instance 'plane
                  :normal z-axis
                  :location (@ 0 0 -1)
                  :shader (make-instance 'checker
                                         :odd (make-instance 'phong :color black)
                                         :even (make-instance 'phong :color white :ambient 0.1))))
  (:lights
   (make-instance 'solar-light :direction (v 1 1 1)))
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 0 10)
                     :look-at +origin+)))

(defscene test-transform
  (:objects
   (make-instance 'sphere :shader (make-instance 'solid :color red))
   (make-instance 'sphere
                  :shader (make-instance 'solid :color blue)
                  :transform (matrix* (xtranslate* 0 2 0)
                                       (rotate-around z-axis 1.5)))
   (make-instance 'plane
                  :location (@ 0 -1 0)
                  :shader (make-instance 'checker
                                         :odd (make-instance 'phong :color black)
                                         :even (make-instance 'phong :color white :ambient 0.1))))
  (:lights
   (make-instance 'solar-light :direction (v 1 1 1)))
  (:camera
      (make-instance 'pinhole
                     :location (@ 1 10 -10)
                     :look-at +origin+)))

(defscene test-perspective
  (:objects
   (make-instance 'sphere
                  :location (@ 10 0 0) :shader (make-instance 'solid :color red))
   (make-instance 'sphere
                  :location (@ -10 0 0) :shader (make-instance 'solid :color green))
   (make-instance 'sphere
                  :location (@ 0 0 0) :shader (make-instance 'solid :color blue))
   (make-instance 'sphere
                  :location (@ 0 0 10) :shader (make-instance 'solid :color (@ 1 1 0)))
   (make-instance 'sphere
                  :location (@ 0 0 -10) :shader (make-instance 'solid :color (@ 0 1 1)))
   (make-instance 'csg
                  :type 'difference
                  :objects (list
                            (make-instance 'plane
                                           :normal (v 0 1 0)
                                           :location (@ 0 -1 0)
                                           :transform (rotate-around z-axis (/ +pi+ -4))
                                           :shader (make-instance 'checker
                                                                  :scale 5
                                                                  :odd (make-instance 'phong :color black)
                                                                  :even (make-instance 'phong :color white :ambient 0.1)))
                            (make-instance 'plane
                                           :normal (v 0 0 -1)
                                           :location (@ 0 0 -0.01)
                                           :shader (make-instance 'checker
                                                                  :scale 5
                                                                  :odd (make-instance 'phong :color black)
                                                                  :even (make-instance 'phong :color white :ambient 0.1))))))
  (:lights
   (make-instance 'solar-light :direction (v 1 1 1)))
  (:camera
      (make-instance 'pinhole
                     :location (@ 0 20 30)
                     :look-at (@ 0 -1 0)
                     :focal-length 3.0)))

(defscene test-mirror
  (:objects
   *floor*
   (make-instance 'plane
                  :normal (v -0.5 0 -1)
                  :location (v 3 0 3)
                  :shader
                  (make-instance 'composite
                                 :shaders
                                 (list
                                  (make-instance 'raytrace
                                                 :specular 0.95
                                                 :transmit 0.0)
                                  (make-instance 'flat
                                                 :ambient 0.05
                                                 :color black))))
   (make-instance 'sphere
                  :radius 3.0
                  :location (v -0.5 3 -4)
                  :shader
                  (make-instance
                   'composite
                   :shaders
                   (list
                    (make-instance 'raytrace
                                   :specular 0.1
                                   :transmit 0.9
                                   :ior 1.2)
                    (make-instance 'phong
                                   :specular 0.5
                                   :size 40.0
                                   :diffuse 0.15
                                   :ambient 0.1
                                   :color yellow))))
   (make-instance 'sphere
                  :radius 1.0
                  :location (v 0 1 0)
                  :shader *bright-red*))
  (:lights
   (make-instance 'point-light
                  :color (v 2.0 2.0 2.0)
                  :location (v -40 20 -5)))
  (:camera
   *view*))

(defscene test-sphere-difference
  (:objects
   *floor*
   (make-instance 'sphere
                  :radius 4.0
                  :location (@ 0 3 0)
                  :transform (list
                              (rotate-around x-axis (/ pi 4))
                              (translate* -4.0 0.0 -2.0))
                  :shader *bright-red*
                  :name "control")
   (make-instance 'csg
                  :type 'difference
                  :transform (rotate-around x-axis (/ pi 4))
                  :objects (list
                            (make-instance 'sphere
                                           :radius 4.0
                                           :location (@ 0 3 0)
                                           :shader *bright-red*
                                           :name "source")
                            (make-instance 'plane
                                           :location (@ 0 -10 0)
                                           :shader *bright-blue*
                                           :name "cut"))))
  (:lights
   *lamp*)
  (:camera
   *view*))

;;;# Tests
;;;
;;; Raylisp includes both test scenes, and a number of functional
;;; regression tests.

(defvar *passed-test-count* 0)
(defvar *failed-tests* nil)

(defmacro test (name form expect)
  (let ((n-expect (gensym))
	(n-form (gensym)))
    `(let ((,n-form ,form)
	   (,n-expect ,expect))
       (cond ((approximates ,n-form ,n-expect)
	      (incf *passed-test-count*))
	     (t
	      (warn "~A failed.~% Expected: ~S, got ~S."
		    ',name ,n-expect ,n-form)
	      (pushnew ',name *failed-tests*)))
       ',name)))

(defun run-tests ()
  (let ((*passed-test-count* 0)
	(*failed-tests* nil))

    (test-math)
    (test-ray)
    (test-sphere)
    (test-plane)

    (let* ((failed (length *failed-tests*))
	   (total (+ *passed-test-count* failed)))
      (when *failed-tests*
	(format t "~A failed tests:~%  ~{~A ~}~%"
		failed
		(reverse *failed-tests*)))
      (format t "~&~A tests of ~A passed. (~A%)~%"
	      *passed-test-count* total
	      (* 100.0 (/ *passed-test-count* total))))))

(defun test-math ()
  (test math.1
	(transform-point y-axis (reorient y-axis (v 1 1 1)))
	(normalize (v 1 1 1)))
  (test math.2 (vec 1.0 2.0 4.654) (@ 1 2 4.654)))

;;; FIXME: OBSOLETE
#+nil
(defun test-ray ()
  (let* ((from (@ 1 0 -1))
	 (direction (normalize (v -1 0 1)))
	 (ray (make-ray :origin from :direction direction)))
    (multiple-value-bind (reflected refracted)
	(spawn-rays (make-intersection :point +origin+
				       :normal (v 0 0 -1)
				       :n.d (dot-product (v 0 0 -1) direction))
		    ray
		    0.2
		    0.7
		    1.8)
      (test ray.1.1
	    (ray-direction reflected)
	    (normalize (v -1 0 -1)))
      (test ray.1.2
	    (ray-direction refracted)
	    (v -0.3928 0 0.9196))
      (multiple-value-bind (reflected refracted)
	  (spawn-rays (make-intersection :point (ray-direction refracted)
					 :normal (v 0 0 -1)
					 :n.d (dot-product (v 0 0 1) (ray-direction refracted)))
		      refracted
		      0.2
		      0.7
		      1.8)
        (declare (ignore reflected))
	(test ray.2
	      (ray-direction refracted)
	      direction)))))

(defun test-sphere ()
  (flet ((make-sphere (radius location &key transform)
	   (compile-scene-object (make-instance 'sphere
                                                :radius radius
                                                :location location
                                                :transform transform)
                                 (make-scene)
                                 (identity-matrix)))
	 (normal (object point)
	   (funcall (object-normal object) point))
	 (intersect (x from dir)
	   (let ((ray (make-ray :origin from :direction dir)))
	     (if (intersect x ray)
		 (ray-extent ray)
		 -1.0))))
    (let ((s (make-sphere 1.0 +origin+)))
      (test sphere.1.1 (intersect s (@ 0 0 -2) z-axis)
	    1.0)
      (test sphere.1.2 (intersect s (@ 1 0 -2) z-axis)
	    2.0)
      (test sphere.1.3 (intersect s (@ 1 0.001 -2) z-axis)
	    -1.0)
      (test sphere.1.4
            (let* ((o (@ 0 0 -2))
		   (d (intersect s o z-axis)))
	      (normal s (vec+ o (vec* z-axis d))))
	    (@ 0 0 -1)))
    (let ((s (make-sphere 0.5 +origin+)))
      (test sphere.2.1 (intersect s (@ 0 0 -2) z-axis)
	    1.5)
      (test sphere.2.2 (intersect s (@ 0 0 -1) z-axis)
	    0.5)
      (test sphere.2.3
	    (let* ((o (@ 0 0 -2))
		   (d (intersect s o z-axis)))
	      (normal s (vec+ o (vec* z-axis d))))
	    (@ 0 0 -1)))
    (let ((s (make-sphere 1.0 (@ 1 0 0))))
      (test sphere.3.1 (intersect s (@ -0.001 0 0) z-axis)
	    -1.0))

    (let* ((s (make-sphere 1.0 (@ 0 -1 0)))
	   (o (@ 0 -1 -2))
	   (dist (intersect s o z-axis))
	   (pos (vec+ o (vec* z-axis dist))))
      (test sphere.4.1 dist 1.0)
      (test sphere.4.2 pos (@ 0 -1 -1))
      (test sphere.4.3 (normal s pos) (@ 0 0 -1)))

    (let ((s (make-sphere 1.0 (@ 0 1 0)
			  :transform (scale (@ 2 1 1))))
	  (x-neg (@ -3 1 0))
	  (y-neg (@ 0 -1 0)))
      (test sphere.5.1 (intersect s x-neg x-axis) 1.0)
      (test sphere.5.2 (intersect s y-neg y-axis) 1.0))))

(defun test-plane ()
  (flet ((make-plane (n v)
	   (compile-scene-object (make-instance 'plane :normal n :location v)
                                 (make-scene)
                                 (identity-matrix)))
	 (normal (x v)
	   (funcall (object-normal x) v))
	 (intersect (x from dir)
	   (let ((ray (make-ray :origin from :direction dir)))
	     (if (intersect x ray)
		 (ray-extent ray)
		 -1.0))))
    (let ((p (make-plane y-axis +origin+)))
      (test plane.1.1
	    (intersect p (@ 0 1 0) (@ 0 -1 0))
	    1.0))
    (let ((p (make-plane (@ 1 1 0) +origin+)))
      (test plane.2.1
	    (intersect p (@ 0 1 0) (@ -1 0 0))
	    1.0)
      (test plane.2.2
	    (plusp (intersect p (@ 0 1 0) (@ -1 0.1 0)))
	    t)
      (let ((o (@ 0 1 0))
	    (d (@ -1 0.1 0)))
	(test plane.2.3
	      (normal p (adjust-vec o d (intersect p o d)))
	      (normalize (@ 1 1 0)))))
    (let ((p (make-plane (@ 1 1 0) (@ -1 0 0))))
      (test plane.3.1
	    (intersect p +origin+ (@ 0 -1 0))
	    1.0))))


