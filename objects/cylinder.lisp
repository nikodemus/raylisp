(in-package :raylisp)

(defclass cylinder (scene-object)
  ((start :initform (required-argument :start)
          :initarg :start
          :reader start-of)
   (end :initform (required-argument :end)
        :initarg :end
        :reader end-of)
   (radius :initform 1.0
           :initarg :radius
           :reader radius-of)))

(defun cylinder-matrix-and-length (cylinder)
  (let* ((start (start-of cylinder))
         (end (end-of cylinder))
         (r (radius-of cylinder))
         (axis (vec- end start)))
    (values (matrix* (transform-of cylinder)
                     ;; translate start from origin
                     (translate start)
                     ;; align with desired axis
                     (reorient z-axis axis)
                     ;; scale for radius
                     (scale* r r 1.0))
            (vec-length axis))))

(defmethod compute-object-properties ((cylinder cylinder) scene transform)
  (multiple-value-bind (matrix length) (cylinder-matrix-and-length cylinder)
    (declare (single-float length))
    (let* ((inverse (inverse-matrix (matrix* transform matrix)))
           ;; For normal computation, we first apply the inverse to get
           ;; into the space where the z-aligned cylinder lives. Then
           ;; we zero out the Z coordinate, which leaves us with just
           ;; the X and Y which are the normal -- the use the adjunct
           ;; to return to real space.
           (normal-matrix (matrix* (transpose-matrix inverse)
                                   (scale* 1.0 1.0 0.0)
                                   inverse)))
      (list
       :intersection
       (sb-int:named-lambda cylinder-intersection (ray)
         (declare (optimize speed))
         (let* ((o (transform-point (ray-origin ray) inverse))
                (d (transform-direction (ray-direction ray) inverse))
                (ox (aref o 0))
                (oy (aref o 1))
                (dx (aref d 0))
                (dy (aref d 1)))
           (declare (dynamic-extent o d))
           (multiple-value-bind (t1 t2) (pos-quad-roots (+ (square dx) (square dy))
                                                        (+ (* 2.0 ox dx) (* 2.0 oy dy))
                                                        (+ (square ox) (square oy) -1.0))
             (unless (= t1 -1.0)
               (cond ((and (< t1 (ray-extent ray))
                           (< 0.0 (+ (aref o 2) (* t1 (aref d 2))) length))
                      (setf (ray-extent ray) t1)
                      t)
                     ((and (< 0.0 t2 (ray-extent ray))
                           (< 0.0 (+ (aref o 2) (* t2 (aref d 2))) length))
                      (setf (ray-extent ray) t2)
                      t))))))
       :normal
       (lambda (point)
         (let ((p (transform-point point normal-matrix)))
           (declare (optimize speed))
           (%normalize p p)))))))
