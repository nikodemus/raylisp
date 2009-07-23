;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :raylisp)

(defclass scene-object (transformable light-group-mixin)
  ((shader
    :initform (find-default :shader '(or null shader)) :initarg :shader
    :accessor shader-of)
   (name
    :initarg :name
    :initform nil
    :accessor name-of)
   (bounding-box-only
    :initform nil
    :initarg :bounding-box-only
    :reader bounding-box-only-p)))

(defgeneric compute-object-properties (scene-object scene transform &key shading-object))

(defmethod compute-object-properties :around ((obj scene-object) scene transform
                                              &key shading-object)
  (if (bounding-box-only-p obj)
      (multiple-value-bind (min max) (compute-object-extents obj (identity-matrix))
        (compute-object-properties (make-instance 'box
                                                  :min min
                                                  :max max
                                                  :shader (shader-of obj))
                                   scene
                                   transform
                                   :shading-object shading-object))
      (call-next-method)))

(defgeneric compute-object-extents (scene-object transform))

(defmethod compute-object-extents ((object scene-object) transform)
  nil)

(defun compile-scene-object (object scene transform &key shading-object)
  ;; If this is a shading object for another object, the transform
  ;; has already been applied.
  (let ((m (if shading-object
               transform
               (matrix* transform (transform-of object)))))
    (destructuring-bind (&key intersection normal)
        (compute-object-properties object scene m :shading-object shading-object)
     (assert normal)
     (let ((shader (compute-shader-function (shader-of object)
                                            ;; If this is a shading object, tell the
                                            ;; shader about the real object.
                                            (or shading-object object)
                                            scene
                                            m)))
       (if shading-object
           (make-shading-object
            :normal normal
            :shader shader)
           (multiple-value-bind (min max) (compute-object-extents object m)
             (assert intersection)
             (make-intersection-object
              :intersection intersection
              :normal normal
              :shader shader
              :min min :max max
              :scene-object object)))))))
