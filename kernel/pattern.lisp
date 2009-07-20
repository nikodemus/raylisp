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

;;;; PATTERNS
;;;;
;;;; Subclasses of INTERPOLATED-PATTERN and INDEXED-PATTERN implement
;;;; different patterns.
;;;;
;;;; COMPUTE-PATTERN-KEY-FUNCTION is called for patterns used in the scene.
;;;; The returned function is called with a point during rendering to obtain
;;;; either a single-float in range 0.0-1.0 (for interpolated patterns) or
;;;; non-negative integer (for indexed patterns), which in turn is used to
;;;; select entries in the map associated with the pattern.
;;;;
;;;; COMPUTE-PATTERN-FUNCTION is called for map entries of patterns used
;;;; in the scene. During rendering, after map entries (explained above)
;;;; have been selected, their pattern functions are then called to obtain
;;;; values for pattern at that point.
;;;;
;;;; In case of indexed patterns only the function for the entry corresponding
;;;; to the computed index is called, whereas for interpolated patterns
;;;; pattern functions on each side of the returned value are called after
;;;; which their values are interpolated. (Except when the key value
;;;; corresponds exactly to one of the map keys.)
;;;;
;;;; Pattern maps are specified as lists for indexed patterns, and association
;;;; lists of the form (<single-float> <entry>) for interpolated patterns.
;;;;
;;;; Each pattern also has an associated type, which specifies the contexts
;;;; it is meant for: map entries are verified against this type when the
;;;; pattern object is created, but it has no purpose besides enabling such
;;;; sanity checking.
;;;;
;;;; Currently implemented pattern types are :SHADER and :COLOR. More can be
;;;; added by using DEFINE-PATTERN-TYPE.
;;;;
;;;; Currently all patterns return VEC values, but this is not a deep design:
;;;; if eg. we stop punning RGB colors to vectors, we need to teach the pattern
;;;; framework about interpolating them -- but that is not a big deal.

(defclass pattern (transform-mixin)
  ((type
    :initarg :type
    :initform (required-argument :type)
    :reader pattern-type)
   (map
    :initarg :map
    :initform (required-argument :map)
    :reader pattern-map)))

(defvar *pattern-types* (make-hash-table))

(defmacro define-pattern-type (name (object) &body body)
  `(setf (gethash ',name *pattern-types*)
         (lambda (,object)
           ,@body)))

(define-pattern-type :shader (object)
  ;; SHADER objects are obviously good. PATTERN-FUNCTION for a COLOR returns a
  ;; function that always returns that color for any number of arguments. A
  ;; pattern of type :SHADER is also cosher, of course.
  (or (typep object '(or shader color))
      (and (typep object 'pattern) (eq :shader (pattern-type object)))))

(define-pattern-type :color (object)
  (or (typep object 'color)
      (and (typep object 'pattern) (eq :color (pattern-type object)))))

(defgeneric compute-pattern-key-function (pattern transform))
(defgeneric pattern-map-values (pattern transform &rest args))
(defgeneric pattern-function (pattern transform &rest args))

(defmethod pattern-function ((pattern #.(class-of (alloc-vec))) transform &rest args)
  (declare (type vec pattern)
           (ignore args))
  (sb-int:named-lambda constant-pattern-function (point &rest args)
    (declare (type vec point)
             (ignore point args)
             (optimize (safety 0)))
    pattern))

(defclass interpolated-pattern (pattern)
  ())

;;;; Check that the contents of the map match the promised type, and also give
;;;; the keys a once-over.
(defmethod initialize-instance :after ((pattern pattern) &key)
  (let* ((type (pattern-type pattern))
         (verifier (or (gethash type *pattern-types*)
                       (error "Unknown pattern type: ~S" type)))
         (map (pattern-map pattern)))
    (flet ((oops (value)
             (error "~S is not a valid value in a pattern of type ~S."
                    value type)))
      (if (typep pattern 'interpolated-pattern)
          (let (last)
            (dolist (spec map)
              (destructuring-bind (key value) spec
                (unless (typep key '(real 0 1))
                  (error "~S is not a valid key for an interpolated pattern: must be a ~S."
                         key '(real 0 1)))
                (unless (or last (= key 0))
                  (error "Interpolated patterns must start at 0.0."))
                (unless (or (not last) (> key last))
                  (error "Interpolated patterns must be strictly increasing."))
                (unless (funcall verifier value)
                  (oops value))
                (setf last key)))
            (unless (= last 1)
              (error "Interpolated patterns must end at one 1.0.")))
          (let ((real-size (length map))
                (correct-size (indexed-pattern-size pattern)))
            (unless (= real-size correct-size)
              (error "Indexed pattern ~S requires a map of ~S elements, but ~S were found."
                     (class-name (class-of pattern)) correct-size real-size))
            (dolist (spec (pattern-map pattern))
              (unless (funcall verifier spec)
                (oops spec))))))))

(defmethod pattern-map-values ((pattern interpolated-pattern) transform &rest args)
  (let* ((map (pattern-map pattern))
         (keys (make-array (length map) :element-type 'single-float))
         (values (make-array (length map)))
         (p 0))
    ;; FIXME: Cache the float vector in the pattern.
    (dolist (elt map)
      (destructuring-bind (key value) elt
        (setf (aref keys p) (coerce key 'single-float)
              (aref values p) (apply #'pattern-function value transform args))
        (incf p)))
    (values keys values)))

(defmethod pattern-function ((pattern interpolated-pattern) transform &rest args)
  (let* ((matrix (matrix* transform (transform-of pattern)))
         (key-function
          (the function
            (compute-pattern-key-function pattern matrix))))
    (multiple-value-bind (keys values) (apply #'pattern-map-values pattern matrix args)
      (declare (type (simple-array single-float (*)) keys)
               (type (simple-array t (*)) values))
      (let ((map-size (length keys)))
        (if (eq :shader (pattern-type pattern))
            (compute-interpolated-shader-pattern-function key-function keys values)
            (sb-int:named-lambda pattern-at-point (point &rest args)
              (declare (optimize speed) (dynamic-extent args))
              (block pattern-at-point
                (let ((value (funcall key-function point))
                      (tmp 0.0))
                  (declare (type (single-float 0.0 1.0) value tmp))
                  ;; FIXME: Linear search is not so good with big maps!
                  (let* ((index (loop for i from 0 below map-size
                                      do (let ((this (aref keys i)))
                                           (cond ((= value this)
                                                  ;; Exact hit, no need to compute anything else.
                                                  (return-from pattern-at-point
                                                    (apply (the function (aref values i)) point args)))
                                                 ((< tmp value this)
                                                  (setf tmp (- 1.0 (/ (- this value) (- this tmp))))
                                                  (return i))
                                                 (t
                                                  (setf tmp this)))))))
                    ;; Blend between the two values.
                    (vec-lerp (apply (the function (aref values (- index 1))) point args)
                              (apply (the function (aref values index)) point args)
                              tmp))))))))))

(defun compute-interpolated-shader-pattern-function (key-function keys values)
  (declare (type function key-function)
           (type (simple-array single-float (*)) keys)
           (type simple-vector values))
  (let ((map-size (length keys)))
    (shader-lambda interpolated-shader-pattern (result point normal n.d ray counters)
      (declare (optimize speed))
      (flet ((call (res function)
               (values (funcall (the function function) res point normal n.d ray counters))))
        (let ((value (funcall key-function point))
              (blend 0.0))
          (declare (type (single-float 0.0 1.0) value blend))
          ;; FIXME: Linear search is not so good with big maps!
          (let* ((index (loop for i from 0 below map-size
                              do (let ((this (aref keys i)))
                                   (cond ((= value this)
                                          ;; Exact hit, no need to compute anything else.
                                          (return-from interpolated-shader-pattern
                                            (call result (aref values i))))
                                         ((< blend value this)
                                          (setf blend (- 1.0 (/ (- this value) (- this blend))))
                                          (return i))
                                         (t
                                          (setf blend this))))))
                 (tmp (alloc-vec)))
            (declare (dynamic-extent tmp))
            ;; Blend between the two values.
            (%vec-lerp result
                       (call result (aref values (- index 1)))
                       (call tmp (aref values index))
                       blend)))))))

(defclass indexed-pattern (pattern)
  ())

(defgeneric pattern-map-size (pattern))

(defmethod pattern-map-values ((pattern indexed-pattern) transform &rest args)
  (let* ((map (slot-value pattern 'map))
         (values (make-array (length map)))
         (p 0))
    (dolist (elt map)
      (setf (aref values p) (apply #'pattern-function elt transform args))
      (incf p))
    values))

(defmethod pattern-function ((pattern indexed-pattern) transform &rest args)
  (let* ((matrix (matrix* transform (transform-of pattern)))
         (key-function
          (the function
            (compute-pattern-key-function pattern matrix)))
         (values (apply #'pattern-map-values pattern matrix args)))
    (declare (type (simple-array t (*)) values))
    (if (eq :shader (pattern-type pattern))
        (shader-lambda indexed-shader-pattern (result point normal n.d ray counters)
          (declare (optimize speed))
          (funcall (the function (aref values (funcall key-function point)))
                   result point normal n.d ray counters))
        (lambda (point)
          (funcall (the function (aref values (funcall key-function point))) point)))))

;;; Interaction with shaders...

(defmethod pattern-function ((shader shader) transform &rest args)
  ;; FIXME: this is really ugly
  (destructuring-bind (object scene) args
    (check-type object scene-object)
    (check-type scene scene)
    (compute-shader-function shader object scene transform)))

(defmethod compute-shader-function ((pattern pattern) object scene transform)
  ;; Patterns can act as shaders -- but COMPUTE-PATTERN-FUNCTION will apply
  ;; the pattern transform, so don't do it here!
  (pattern-function pattern transform object scene))
