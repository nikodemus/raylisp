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

;;;;; INTERPOLATED PATTERN

(defclass interpolated-pattern (pattern)
  ())

(defgeneric compute-interpolated-pattern-function (pattern matrix))
;;; KLUDGE: DEFGENERIC seems to nuke the declaimed type... I should fix that...
(declaim (ftype (function (pattern matrix)
                          interpolated-pattern-function)
                compute-interpolated-pattern-function))

(defmethod compute-interpolated-pattern-function :around (pattern matrix)
  (check-function-type (call-next-method) 'interpolated-pattern-function))

(defmethod compute-interpolated-pattern-function :around ((pattern transform-mixin) matrix)
  (call-next-method pattern (matrix* matrix (transform-of pattern))))

(defun expand-pattern-lambda (result-type name point body whole)
  (multiple-value-bind (forms declarations doc)
      (parse-body body :documentation t :whole whole)
    `(sb-int:named-lambda ,name (,point)
       ,@(when doc (list doc))
       (declare (type point ,point)
                (optimize (sb-c::recognize-self-calls 0)
                          (sb-c::type-check 0)
                          (sb-c::verify-arg-count 0)))
       (the ,result-type
         (values
          (block ,name
            (locally
                (declare (optimize (sb-c::type-check 1) (sb-c::verify-arg-count 1)))
              (let ((,point ,point))
                ,@declarations
                ,@forms))))))))

(defmacro interpolated-pattern-lambda (&whole form name (point) &body body)
  (expand-pattern-lambda '(single-float 0.0 1.0) name point body form))

(defun compile-interpolated-pattern (pattern value-function)
  (declare (function value-function))
  (let* ((map (pattern-map pattern))
         (keys (make-array (length map) :element-type 'single-float))
         (values (make-array (length map)))
         (p 0))
    ;; FIXME: Cache the float vector in the pattern.
    (dolist (elt map)
      (destructuring-bind (key value) elt
        (setf (aref keys p) (coerce key 'single-float)
              (aref values p) (funcall value-function value))
        (incf p)))
    (values keys values)))

(declaim (ftype (function (single-float (simple-array single-float (*)))
                          (values array-index single-float &optional))
                find-interpolated-index))
(defun find-interpolated-index (value mapkeys)
  (declare (optimize speed))
  (let ((prev 0.0))
    (declare (single-float prev))
    (dotimes (i (length mapkeys))
      (let ((this (aref mapkeys i)))
        (cond ((= value this)
               ;; Exact hit!
               (return-from find-interpolated-index
                 (values i 0.0)))
              ((< prev value this)
               ;; Between the previous and this one
               (return-from find-interpolated-index
                 (values (1- i) (- 1.0 (/ (- this value) (- this prev))))))
              (t
               (setf prev this)))))
    (error "Could not find ~S in interpolated pattern map keys:2~%  ~S"
           value mapkeys)))

(defmethod compute-pigment-function ((pattern interpolated-pattern) matrix)
  (assert (eq :color (pattern-type pattern)))
  (let ((pat (compute-interpolated-pattern-function pattern matrix)))
    (declare (interpolated-pattern-function pat))
    (multiple-value-bind (mapkeys mapvalues)
        (compile-interpolated-pattern
         pattern (lambda (val)
                   (compute-pigment-function val matrix)))
      (declare (type (simple-array single-float (*)) mapkeys)
               (type (simple-array pigment-function (*)) mapvalues))
      (pigment-lambda interpolated-pattern-pigment (result point)
        (declare (optimize speed))
        (multiple-value-bind (index blend)
            (find-interpolated-index (funcall pat point) mapkeys)
          (if (= blend 0.0)
              (funcall (aref mapvalues index) result point)
              (let ((tmp1 (alloc-vec))
                    (tmp2 (alloc-vec)))
                (declare (dynamic-extent tmp1 tmp2))
                (%vec-lerp result
                           (funcall (aref mapvalues index) tmp1 point)
                           (funcall (aref mapvalues (1+ index)) tmp2 point)
                           blend))))))))

(defmethod compute-shader-function ((pattern interpolated-pattern) object scene matrix)
  (assert (eq :shader (pattern-type pattern)))
  (let ((pat (compute-interpolated-pattern-function pattern matrix)))
    (declare (interpolated-pattern-function pat))
    (multiple-value-bind (mapkeys mapvalues)
        (compile-interpolated-pattern
         pattern (lambda (val)
                   (compute-shader-function val object scene matrix)))
      (declare (type (simple-array single-float (*)) mapkeys)
               (type (simple-array shader-function (*)) mapvalues))
      (shader-lambda interpolated-pattern-shader (result point normal n.d ray counters)
        (declare (optimize speed))
        (flet ((call (res i)
                 (values (funcall (aref mapvalues i) res point normal n.d ray counters))))
          (multiple-value-bind (index blend)
              (find-interpolated-index (funcall pat point) mapkeys)
            (if (= blend 0.0)
                (call result index)
                (let ((tmp1 (alloc-vec))
                      (tmp2 (alloc-vec)))
                  (declare (dynamic-extent tmp1 tmp2))
                  (%vec-lerp result (call tmp1 index) (call tmp2 (1+ index))
                             blend)))))))))

;;;; INDEXED PATTERN

(defclass indexed-pattern (pattern)
  ())

(defgeneric indexed-pattern-size (pattern))

(defgeneric compute-index-pattern-function (pattern matrix))

(defmethod compute-index-pattern-function :around (pattern matrix)
  (check-function-type (call-next-method) 'indexed-pattern-function))

(defmethod compute-index-pattern-function :around ((pattern transform-mixin) matrix)
  (call-next-method pattern (matrix* matrix (transform-of pattern))))

(defmacro indexed-pattern-lambda (&whole form name (point) &body body)
  (expand-pattern-lambda 'array-index name point body form))

(defmethod compile-indexed-pattern (pattern value-function)
  (declare (function value-function))
  (let* ((map (slot-value pattern 'map))
         (canon-size (indexed-pattern-size pattern))
         (size (length map))
         (values (make-array size))
         (p 0))
    (unless (= canon-size size)
      (error "~S needs ~S element in its map, but ~S were provided:~2%  ~S"
             pattern canon-size size map))
    (dolist (elt map)
      (setf (aref values p) (funcall value-function elt))
      (incf p))
    values))

(defmethod compute-pigment-function ((pattern indexed-pattern) matrix)
  (let ((pat (compute-indexed-pattern-function pattern matrix))
        (mapvalues (compile-indexed-pattern
                    pattern
                    (lambda (val)
                      (compute-pigment-function val matrix)))))
    (declare (type (simple-array pigment-function (*)) mapvalues)
             (indexed-pattern-function pat))
    (pigment-lambda indexed-pattern-pigment (result point)
      (declare (optimize speed))
      (funcall (aref mapvalues (funcall pat point)) result point))))

(defmethod compute-shader-function ((pattern indexed-pattern) object scene matrix)
  (let ((pat (compute-indexed-pattern-function pattern matrix))
        (mapvalues (compile-indexed-pattern
                    pattern
                    (lambda (val)
                      (compute-shader-function val object scene matrix)))))
    (declare (type (simple-array shader-function (*)) mapvalues)
             (indexed-pattern-function pat))
    (shader-lambda indexed-pattern-shader (result point normal n.d ray counters)
      (funcall (aref mapvalues (funcall pat point))
               result point normal n.d ray counters))))

