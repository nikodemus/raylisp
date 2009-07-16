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
    :initform (required-argument :map))))

(defgeneric compute-pattern-key-function (pattern transform))
(defgeneric pattern-map-values (pattern transform &rest args))
(defgeneric pattern-function (pattern transform &rest args))

(defmethod pattern-function ((pattern #.(class-of (alloc-vec))) transform &rest args)
  (declare (ignore args))
  (constantly pattern))

(defclass interpolated-pattern (pattern)
  ())

(defmethod pattern-map-values ((pattern interpolated-pattern) transform &rest args)
  (let* ((map (slot-value pattern 'map))
         (map-size (length map))
         (keys (make-array (length map) :element-type 'single-float))
         (values (make-array (length map)))
         (last -1.0)
         (p 0))
    (unless map
      (error "Pattern's map is empty: ~S" pattern))
    ;; Verify the map and build VALUES and VALUES.
    (dolist (elt map)
      (destructuring-bind (key value) elt
        (cond ((not (typep key '(real 0 1)))
               (error "Pattern-map has bogus key ~S:~%  ~S" key map))
              ((and (= p 0) (/= 0.0 key))
               (error "Pattern-map does not start at 0.0:~%  ~S" map))
              ((and (= p (1- map-size)) (/= 1.0 key))
               (error "Pattern-map does not end at 1.0:~%  ~S" map))
              ((>= last key)
               (error "Pattern-map is not strictly increasing:~%  ~S" map)))
        (setf (aref keys p) (coerce key 'single-float)
              (aref values p) (apply #'pattern-function value transform args)
              last key)
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
         (map-size (length map))
         (correct-size (pattern-map-size pattern))
         (values (make-array map-size))
         (p 0))
    (unless (= map-size correct-size)
      (error "Map of ~S has the wrong size: ~S elements expected, got ~S.~%  ~S"
             pattern correct-size map-size map))
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
