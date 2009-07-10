(in-package :raylisp)

(defclass name-mixin ()
  ((name :initarg :name :reader name-of)))

(defmethod name-of ((name symbol))
  name)

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
  (name nil)
  (objects nil :type list)
  (lights nil :type list)
  (depth-limit (find-default :depth-limit 'fixnum) :type fixnum)
  (adaptive-limit
   (find-default :adaptive-limit '(float 0.0 1.0)) :type (float 0.0 1.0))
  (background-color (find-default :background-color 'vec) :type vec)
  (ambient-light (find-default :ambient-light 'vec) :type vec)
  (default-camera)
  (compiled-scene))

(defparameter *scenes* (make-hash-table))

(defmacro defscene (name &body alist)
  (flet ((get-key (name &optional use-default-type)
           (let* ((cell (assoc name alist))
                  (forms (cdr cell)))
             (when cell
               (setf alist (remove cell alist)))
             (cond (use-default-type
                    (assert (not (cdr forms)))
                    (or (car forms) (find-default name use-default-type)))
                   (t
                    `(list ,@forms))))))
    (prog1
        `(progn
           (setf (gethash ',name *scenes*)
                 (make-scene :name ',name
                             :objects (flatten ,(get-key :objects))
                             :lights (flatten ,(get-key :lights))
                             :background-color ,(get-key :background-color 'vec)
                             :ambient-light ,(get-key :ambient-light 'vec)
                             :adaptive-limit ,(get-key :adaptive-limit '(float 0.0 1.0))
                             :depth-limit ,(get-key :depth-limit 'fixnum)
                             :default-camera ,(get-key :camera t)))
           ',name)
      (when alist
        (cerror "Nevermind them." "Unrecognized scene options: ~S" alist)))))

;; +;;; Symbols can be used to refer to named scene components
;; +
;; +(macrolet ((def (type &key (short type))
;; +             (let ((table (format-symbol :raylisp "*NAMED-~AS*" type))
;; +                   (find (format-symbol :raylisp "FIND-~A" short)))
;; +               `(progn
;; +                  (defparameter ,table (make-hash-table))
;; +                  (defmacro ,(format-symbol :raylisp "DEF~A" short) (name (&key
;; +                    `(setf (gethash ',name ,',table)
;; +                           (let ((thing (make-instance ',class ,@args :name ',n
;; +                             (check-type thing (or symbol ,',type))
;; +                             (cons thing ',args))))
;; +                  (defun ,find (name)
;; +                    (check-type name symbol)
;; +                    (multiple-value-bind (info ok) (gethash name ,table)
;; +                      (unless ok
;; +                        (error "Undefined ~A: ~S" ',type name))
;; +                      (car info)))
;; +                  (defun ,(format-symbol :raylisp "ENSURE-~A" short) (thing)
;; +                    (if (typep thing ',type)
;; +                        thing
;; +                        (,find thing)))
;; +                  (defun ,(format-symbol :raylisp "~S-SOURCE" short) (thing)
;; +                    (let ((name (name-of thing)))
;; +                      (when name
;; +                        (multiple-value-bind (info ok) (gethash name ,table)
;; +                          (when ok
;; +                            `(make-instance ',(class-name (class-of (car info))
;; +  (def shader)
;; +  (def scene-object :short object)
;; +  (def scene-light :short light)
;; +  (def camera))

(defstruct compiled-scene
  (objects nil :type list)
  (lights nil :type list)
  (tree nil :type (or null kd-node)))

(defparameter *use-kd-tree* t)

(defun compile-scene (scene)
  (let ((c-scene (make-compiled-scene)))
    (setf (scene-compiled-scene scene) c-scene)
    (let ((c-objs (mapcar (lambda (obj)
                                  (compile-scene-object obj scene (identity-matrix)))
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

(defclass shader (name-mixin)
  ())

(deftype compiled-shader ()
  `(function (vec vec float ray counter-vector) vec))

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

(defmethod transform-of :around ((object scene-object))
  (let ((transform (call-next-method)))
    (etypecase transform
      (cons (apply #'xmatrix* (reverse transform)))
      ((or null matrix) transform))))

(defstruct (compiled-object (:conc-name object-))
  (intersection (required-argument :intersection)
                :type (function (ray) (values boolean &optional compiled-object)))
  (normal (required-argument :normal) :type (function (vec) vec))
  (shader (required-argument :shader) :type compiled-shader)
  (transform (required-argument :transform) :type matrix)
  (min nil :type (or null vec))
  (max nil :type (or null vec))
  (name nil))

;;;## Lights

(defclass scene-light (name-mixin) ())

(defstruct (compiled-light (:conc-name light-))
  (direction (required-argument :illumination) :type (function (vec) vec))
  (illumination
   (required-argument :illumination)
   :type (function (vec vec counter-vector) (values vec float &optional))))

