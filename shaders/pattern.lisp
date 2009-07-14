;;; Composition of various shaders on a sliding scale

(in-package :raylisp)

(defclass pattern-shader (shader)
  ((pattern
    :initform (required-argument :pattern)
    :initarg :pattern)
   (map
    :initform (required-argument :map)
    :initarg :map)))

(defmethod compute-shader-function ((shader pattern-shader) object scene transform)
  (let* ((matrix (matrix* transform (transform-of shader)))
         (inverse (inverse-matrix (matrix* matrix (transform-of object))))
         (map (slot-value shader 'map))
         (map-size (length map))
         (shadermap (make-array map-size :element-type 'single-float))
         (shaders (make-array map-size))
         (pattern-function (find-pattern (slot-value shader 'pattern)))
         (p 0)
         (last -1.0))
    (declare (function pattern-function))
    ;; Build the map and verify it.
    (dolist (elt map)
      (destructuring-bind (value shader) elt
        (cond ((and (= p 0) (/= 0.0 value))
               (error "Pattern shader's map does not start at 0.0:~%  ~S" map))
              ((and (= p (1- map-size)) (/= 1.0 value))
               (error "Pattern shader's map does not end at 1.0:~%  ~S" map))
              ((>= last value)
               (error "Pattern shader's map is not strictly increasing:~%  ~S" map)))
        (setf (aref shadermap p) value
              (aref shaders p) (compile-shader shader object scene matrix)
              last value)
        (incf p)))
    (sb-int:named-lambda shader-pattern (object point normal dot ray counters)
      (declare (optimize speed))
      (block shade-pattern
        (let* ((point2 (transform-point point inverse))
               (value (funcall pattern-function point2))
               (tmp 0.0))
          (declare (dynamic-extent point2)
                   (single-float tmp value))
          ;; FIXME: Linear search is not so good with big maps!
          (let* ((index (loop for i from 0 below map-size
                              do (let ((this (aref shadermap i)))
                                   (cond ((= value this)
                                          ;; Exact hit, no need to compute anything else.
                                          (return-from shade-pattern
                                            (funcall (the function (aref shaders i)) object point normal dot ray counters)))
                                         ((< tmp value this)
                                          (setf tmp (- 1.0 (/ (- this value) (- this tmp))))
                                          (return i))
                                         (t
                                          (setf tmp this))))))
                 (c1 (funcall (the function (aref shaders (1- index)))
                              object point normal dot ray counters))
                 (c2 (funcall (the function (aref shaders index))
                              object point normal dot ray counters)))
            (vec-lerp c1 c2 tmp)))))))

(defun find-pattern (name)
  ;; FIXME: This is just a placeholder: we really want users to be able
  ;; to define new patterns as well.
  (ecase name
    (checker-pattern
     (lambda (point)
       (declare (type vec point)
                (optimize speed))
       (macrolet ((dim (n)
                    `(ifloor (+ epsilon (aref point ,n)) 1.0)))
         (if (oddp (+ (dim 0) (dim 1) (dim 2)))
             0.0
             1.0))))
    (square-pattern
     (lambda (point)
       (declare (type vec point)
                (optimize speed))
       (macrolet ((dim (n)
                    `(abs (nth-value 1 (round (aref point ,n))))))
         (/ (+ (dim 0) (dim 1) (dim 2)) 1.5))))
    (gradient-pattern
     (lambda (point)
       (imod (* (aref point 0) 0.3) 1.0)))))

