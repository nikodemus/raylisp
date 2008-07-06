(in-package :raylisp)

;;;# Scene Representation
;;;
;;; Prior to rendering scene is composed of freely mutable instances:
;;; scene objects, lights, shaders, and camera are all represented by
;;; instances of various CLOS classes.
;;;
;;; Just before the rendering the scene is compiled to a more efficient
;;; representation, composed of structures and functions. This compilation
;;; is controlled by various scene protocols.

(defstruct scene 
  (objects nil :type list)
  (lights nil :type list)
  (depth-limit (find-default :depth-limit 'fixnum) :type fixnum)
  (adaptive-limit 
   (find-default :adaptive-limit '(float 0.0 1.0)) :type (float 0.0 1.0))
  (background-color (find-default :background-color 'vector) :type vector)
  (ambient-light (find-default :ambient-light 'vector) :type vector)
  (default-camera)
  (compiled-scene))

(defvar *scenes* (make-hash-table))

(defmacro define-scene (name &body alist)
  (flet ((get-key (name &optional use-default-type)
           (let ((forms (cdr (assoc name alist))))
             (cond (use-default-type
                    (assert (not (cdr forms)))
                    (or (car forms) (find-default name use-default-type)))
                   (t
                    `(list ,@forms))))))
    `(progn
       (setf (gethash ',name *scenes*)
             (make-scene :objects ,(get-key :objects)
                         :lights ,(get-key :lights)
                         :background-color ,(get-key :background-color 'vector)
                         :ambient-light ,(get-key :ambient-light 'vector)
                         :adaptive-limit ,(get-key :adaptive-limit '(float 0.0 1.0))
                         :depth-limit ,(get-key :depth-limit 'fixnum)
                         :default-camera ,(get-key :default-camera t)))
       ',name)))

(defstruct compiled-scene
  (objects nil :type list)
  (lights nil :type list)
  (tree nil :type (or null kd-node)))

(defparameter *use-kd-tree* nil)

(defun compile-scene (scene)
  (let ((c-scene (make-compiled-scene)))
    (setf (scene-compiled-scene scene) c-scene)
    (let ((c-objs (mapcar (lambda (obj) 
                                  (compile-scene-object obj scene))
                                (scene-objects scene))))
            (if *use-kd-tree*
                (multiple-value-bind (kd unbounded) (make-kd-tree c-objs)
                  (setf (compiled-scene-objects c-scene) unbounded
                        (compiled-scene-tree c-scene) kd))
                (setf (compiled-scene-objects c-scene) c-objs)))    
    (setf (compiled-scene-lights c-scene) 
	  (mapcar (lambda (light)
		    (compile-scene-light light scene))
		  (scene-lights scene))))
  scene)

;;;## Shaders
;;;
;;; As a convenience feature NIL is also accepted as a shader (representing
;;; constant black).

(defclass shader () ())

(deftype compiled-shader ()
  `(function (vector vector float ray counter-vector) vector))

;;;## Objects

(defclass scene-object ()
  ((transform 
    :initform (find-default :transform '(or null matrix)) :initarg :transform 
    :accessor transform-of)
   (shader 
    :initform (find-default :shader '(or null shader)) :initarg :shader 
    :accessor shader-of)
   (name
    :initarg :name
    :initform nil
    :accessor name-of)))

(defstruct (compiled-object (:conc-name object-))
  (intersection (required-argument :intersection)
                :type (function (ray) (values boolean &optional compiled-object)))
  (normal (required-argument :normal) :type (function (vector) vector))
  (shader (required-argument :shader) :type compiled-shader)
  (min nil :type (or null vector))
  (max nil :type (or null vector))
  (name nil))

;;;## Lights

(defclass scene-light () ())

(defstruct (compiled-light (:conc-name light-))
  (direction (required-argument :illumination) :type (function (vector) vector))
  (illumination 
   (required-argument :illumination) 
   :type (function (vector vector) (values vector float &optional))))

