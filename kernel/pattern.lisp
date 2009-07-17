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

;;;; What is this stuff?
;;;;
;;;; * Patterns can be used to blend between different color:
;;;;   a pattern can always be used in an :COLOR initarg.
;;;;
;;;; * Patterns can be used to blend between different shaders:
;;;;   a pattern can always be used in an :SHADER initarg.
;;;;
;;;; Other types of patterns can be added easily: just DEFINE-PATTERN-TYPE and
;;;; you are good to go.
;;;;
;;;; Individual pattern classes provide different behaviours. At their core
;;;; they provide a mapping from 3D coordinates (and other optional parameters)
;;;; to _something_ which can either be selected from an indexed pattern map,
;;;; or interpolated using an interpolating pattern map.

(in-package :raylisp)

(defclass pattern (transform-mixin)
  ((type
    :initarg :type
    :initform (required-argument :type)
    :reader pattern-type)
   (map
    :initarg :map
    :initform (required-argument :map)
    :reader pattern-map)))

;;;; Every pattern has a defined type: This is used to sanity check the values
;;;; the pattern produces.

(defvar *pattern-types* (make-hash-table))

(defmacro define-pattern-type (name (object) &body body)
  `(setf (gethash ',name *pattern-types*)
         (lambda (,object)
           ,@body)))

(define-pattern-type :shader (object)
  ;; SHADER objects are obviously good. PATTERN-FUNCTION for a VEC returns a
  ;; function that always returns that vector for any number of arguments.
  ;; Since we pun VECs and RGB colors this means that a VEC in a pattern map
  ;; is fine for a shader. A pattern of type :SHADER is also cosher, of
  ;; course. Less obviously patterns of type :COLOR are good, since they
  ;; ignore all but the first argument.
  (or (typep object '(or shader vec))
      (and (typep object 'pattern) (eq :shader (pattern-type object)))))

(define-pattern-type :color (object)
  ;; Colors are more restricted: we want a function calladble with just a
  ;; single point --- so shaders are not good.
  (or (typep object 'vec)
      (and (typep object 'pattern) (eq :color (pattern-type object)))))

(defgeneric compute-pattern-key-function (pattern transform))
(defgeneric pattern-map-values (pattern transform &rest args))
(defgeneric pattern-function (pattern transform &rest args))

(defmethod pattern-function ((pattern #.(class-of (alloc-vec))) transform &rest args)
  (declare (ignore args))
  (constantly pattern))

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
        (unless (= map-size (length values))
          (error "Corrupt pattern: lengths of keys and values don't match."))
        (sb-int:named-lambda pattern-at-point (point &rest args)
          (declare (optimize speed) (dynamic-extent args))
          (block pattern-at-point
            (let ((value (funcall key-function point))
                  (tmp 0.0))
              (declare (single-float value tmp))
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
                          tmp)))))))))

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
    (sb-int:named-lambda pattern-at-point (point &rest args)
      (declare (optimize speed) (dynamic-extent args))
      (apply (the function (aref values (funcall key-function point))) point args))))

;;; Interaction with shaders...

(defmethod compute-shader-function ((pattern pattern) object scene transform)
  (pattern-function pattern transform object scene))

(defmethod pattern-function ((shader shader) transform &rest args)
  (destructuring-bind (object scene) args
    (check-type object scene-object)
    (check-type scene scene)
    (compute-shader-function shader object scene transform)))
