(in-package :raylisp)

;;;; COLORS AND PATTERNS
;;;;
;;;; It might also be worthwhie to have a macro
;;;;
;;;; (with-color-function (color pattern-or-color) ...body..)
;;;;
;;;; which expands into
;;;;
;;;; (ecase pattern-or-color
;;;;  (vec (flet (((color (point) pattern-or-color)) ...body...)))
;;;;  (pattern (let ((f (compute-color-function pattern-or-color))) (flet ((color (point) (funcall f))) ...body...))))
;;;;
;;;; so that solid colors don't pay extra.

(defclass pattern (transform-mixin)
  ((map
    :initarg :map
    :initform (required-argument :map))
   (keys
    :initform nil)
   (values
    :initform nil)))

(defgeneric pattern-map (pattern)
  (:method ((pattern pattern))
    (let ((keys (slot-value pattern 'keys))
          (values (slot-value pattern 'values)))
      (cond ((and keys values)
             (values keys values))
            (t
             (let* ((map (slot-value pattern 'map))
                    (map-size (length map))
                    (values (make-array (length map)))
                    (keys (make-array (length map) :element-type 'single-float))
                    (last -1.0)
                    (p 0))
               (unless map
                 (error "Pattern's map is empty: ~S" pattern))
               ;; Verify the map and build VALUES and VALUES.
               (dolist (elt map)
                 (destructuring-bind (key value) elt
                   (cond ((not (typep key '(real 0 1)))
                          (error "Pattern-map has bogus key ~S:~%  ~S" key map))
                         ((not (and (typep value 'sequence) (= 3 (length value))
                                    (every (lambda (c) (typep c '(real 0 1))) value)))
                          (error "Pattern-map has bogus value ~S:~%  ~S" value map))
                         ((and (= p 0) (/= 0.0 key))
                          (error "Pattern-map does not start at 0.0:~%  ~S" map))
                         ((and (= p (1- map-size)) (/= 1.0 key))
                          (error "Pattern-map does not end at 1.0:~%  ~S" map))
                         ((>= last key)
                          (error "Pattern-map is not strictly increasing:~%  ~S" map)))
                   (setf (aref keys p) (coerce key 'single-float)
                         (aref values p) (coerce value '(simple-array single-float (3)))
                         last key)
                   (incf p)))
               ;; Cache
               (setf (slot-value pattern 'keys) keys
                     (slot-value pattern 'values) values)
               (values keys values)))))))

(defgeneric compute-pattern-function (pattern matrix))

(defgeneric compute-color-function (color transform))

(defmethod compute-color-function ((color #.(class-of (alloc-vec))) transform)
  (constantly color))

(defmethod compute-color-function ((pattern pattern) transform)
  (let ((pattern-function
         (the function
           (compute-pattern-function pattern (matrix* transform (transform-of pattern))))))
    (multiple-value-bind (keys values) (pattern-map pattern)
      (declare (type (simple-array single-float (*)) keys)
               (type (simple-array t (*)) values))
      (let ((map-size (length keys)))
        (unless (= map-size (length values))
          (error "Corrupt pattern: lengths of keys and values don't match."))
        (sb-int:named-lambda color-function (point)
          (declare (optimize speed))
          (block color-function
            (let ((value (funcall pattern-function point))
                  (tmp 0.0))
              (declare (single-float value tmp))
              ;; FIXME: Linear search is not so good with big maps!
              (let* ((index (loop for i from 0 below map-size
                                  do (let ((this (aref keys i)))
                                       (cond ((= value this)
                                              ;; Exact hit, no need to compute anything else.
                                              (return-from color-function (aref values i)))
                                             ((< tmp value this)
                                              (setf tmp (- 1.0 (/ (- this value) (- this tmp))))
                                              (return i))
                                             (t
                                              (setf tmp this)))))))
                ;; Blend between the two values.
                (vec-lerp (aref values (- index 1)) (aref values index) tmp)))))))))
