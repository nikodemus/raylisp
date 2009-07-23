(in-package :raylisp)

(defclass cylinder (scene-object)
  ((start :initform nil
          :initarg :start
          :reader start-of)
   (end :initform nil
        :initarg :end
        :reader end-of)
   (axis :initform nil
         :initarg :axis
         :reader axis-of)
   (start-cap :initform nil
              :initarg :start-cap
              :reader start-cap-p)
   (end-cap :initform nil
            :initarg :end-cap
              :reader end-cap-p)
   (start-cap-shader :initform nil
                     :initarg :start-cap-shader
                     :reader start-cap-shader)
   (end-cap-shader :initform nil
                     :initarg :end-cap-shader
                     :reader end-cap-shader)
   (radius :initform 1.0
           :initarg :radius
           :reader radius-of)))

(defmethod axis-of :around ((cylinder cylinder))
  (or (call-next-method)
      (let ((start (start-of cylinder))
            (end (end-of cylinder)))
        (setf (slot-value cylinder 'axis)
              (if (and start end)
                  (vec- end start)
                  +z+)))))

(defmethod initialize-instance :before ((cylinder cylinder) &key start end axis)
  ;; Error checking
  (when (and start end axis)
    (error "Invalid CYLINDER: :AXIS specified with :START and :END"))
  (when (and end (not start))
    (error "Invalid CYLINDER: :END specified without :START.")))

(defun cylinder-values (cylinder)
  (let* ((start (start-of cylinder))
         (end (end-of cylinder))
         (r (radius-of cylinder))
         (axis (axis-of cylinder)))
    (if start
        (values (matrix* ;; translate start from origin
                         (translate start)
                         ;; align with desired axis
                         (reorient +z+ axis)
                         ;; scale for radius
                         (scale* r r 1.0))
                (when end (vec-length axis))
                t)
        (values (matrix* ;; align with desired axis
                         (reorient +z+ axis)
                         ;; scale for radius
                         (scale* r r 1.0))
                nil
                nil))))

(defmethod compute-object-properties ((cylinder cylinder) scene transform &key shading-object)
  (multiple-value-bind (matrix length startp) (cylinder-values cylinder)
    (declare (type (or null single-float) length))
    (let* ((m (matrix* transform matrix))
           (inverse (inverse-matrix m))
           ;; For normal computation, we first apply the inverse to get
           ;; into the space where the z-aligned cylinder lives. Then
           ;; we zero out the Z coordinate, which leaves us with just
           ;; the X and Y which are the normal -- the use the adjunct
           ;; to return to real space. End caps get their normals from
           ;; the plane-objects below.
           (normal-matrix (matrix* (transpose-matrix inverse)
                                   (scale* 1.0 1.0 0.0)
                                   inverse))
           (shader (shader-of cylinder))
           (start-cap (when (and (not shading-object) startp (start-cap-p cylinder))
                        (compile-scene-object
                         (make-instance 'plane
                                        :normal #.(vec 0.0 0.0 -1.0)
                                        :location +origin+
                                        :shader (or (start-cap-shader cylinder) shader))
                         scene m :shading-object cylinder)))
           (end-cap (when (and (not shading-object) length (end-cap-p cylinder))
                      (compile-scene-object
                       (make-instance 'plane
                                      :normal +z+
                                      :location (vec 0.0 0.0 length)
                                      :shader (or (end-cap-shader cylinder) shader))
                       scene m :shading-object cylinder))))
      (list
       :intersection
       ;; FIXME: We have three versions here that are virtually identical -- not
       ;; good. Either pay the runtime hit and manage with a single version,
       ;; or macroize this.
       (unless shading-object
         (cond (length
                ;; Bounded at both ends
                (sb-int:named-lambda cylinder-intersection (ray)
                  (declare (optimize speed))
                  (block cylinder-intersection
                    (let* ((o (transform-point (ray-origin ray) inverse))
                           (d (transform-direction (ray-direction ray) inverse))
                           (ox (aref o 0))
                           (oy (aref o 1))
                           (dx (aref d 0))
                           (dy (aref d 1))
                           (limit epsilon))
                      (declare (dynamic-extent o d))
                      (multiple-value-bind (t1 t2)
                          (quadratic-roots-above limit
                                                 (+ (square dx) (square dy))
                                                 (+ (* 2.0 ox dx) (* 2.0 oy dy))
                                                 (+ (square ox) (square oy) -1.0))
                        (when (> t1 limit)
                          (let* ((ext (ray-extent ray))
                                 (oz (aref o 2))
                                 (dz (aref d 2))
                                 (z1 (+ oz (* t1 dz)))
                                 (z2 (+ oz (* t2 dz)))
                                 (t0 (cond ((and (< t1 ext) (< limit z1 length))
                                            t1)
                                           ((and (< limit t2 ext) (< limit z2 length))
                                            t2)
                                           (t
                                            limit))))
                            (when (> t2 limit)
                              ;; May have hit a cap.
                              (when start-cap
                                (let ((t3 (/ (- oz) dz)))
                                  (when (and (< limit t3 ext) (or (< t3 t0) (= t0 limit))
                                             (or (< z1 0.0 z2) (< z2 0.0 z1)))
                                    ;; Two intersections, one on both side of the start cap:
                                    ;; real intersection is on the cap.
                                    (setf (ray-extent ray) t3)
                                    (return-from cylinder-intersection (values t start-cap)))))
                              (when end-cap
                                (let ((t4 (/ (- length oz) dz)))
                                  (when (and (< limit t4 ext) (or (< t4 t0) (= t0 limit))
                                             (or (< z1 0.0 z2) (< z2 0.0 z1)))
                                    ;; Ditto for the end cap.
                                    (setf (ray-extent ray) t4)
                                    (return-from cylinder-intersection (values t end-cap))))))
                            (when (> t0 limit)
                              (setf (ray-extent ray) t0)
                              t))))))))
               (startp
                ;; Bounded at one end
                (sb-int:named-lambda cylinder-intersection (ray)
                  (declare (optimize speed))
                  (block cylinder-intersection
                    (let* ((o (transform-point (ray-origin ray) inverse))
                           (d (transform-direction (ray-direction ray) inverse))
                           (ox (aref o 0))
                           (oy (aref o 1))
                           (dx (aref d 0))
                           (dy (aref d 1))
                           (limit epsilon))
                      (declare (dynamic-extent o d))
                      (multiple-value-bind (t1 t2)
                          (quadratic-roots-above limit
                                                 (+ (square dx) (square dy))
                                                 (+ (* 2.0 ox dx) (* 2.0 oy dy))
                                                 (+ (square ox) (square oy) -1.0))
                        (when (> t1 limit)
                          (let* ((ext (ray-extent ray))
                                 (oz (aref o 2))
                                 (dz (aref d 2))
                                 (z1 (+ oz (* t1 dz)))
                                 (z2 (+ oz (* t2 dz)))
                                 (t0 (cond ((and (< t1 ext) (< 0.0 z1))
                                            t1)
                                           ((and (< 0.0 t2 ext) (< 0.0 z2))
                                            t2)
                                           (t
                                            limit))))
                            (when (> t2 limit)
                              ;; May have hit a cap.
                              (when start-cap
                                (let ((t3 (/ (- oz) dz)))
                                  (when (and (< epsilon t3 ext) (or (< t3 t0) (= t0 limit))
                                             (or (< z1 0.0 z2) (< z2 0.0 z1)))
                                    ;; Two intersections, one on both side of the start cap:
                                    ;; real intersection is on the cap.
                                    (setf (ray-extent ray) t3)
                                    (return-from cylinder-intersection (values t start-cap))))))
                            (when (> t0 limit)
                              (setf (ray-extent ray) t0)
                              t))))))))
               (t
                ;; Unbounded at both ends
                (sb-int:named-lambda unbounded-cylinder-intersection (ray)
                  (declare (optimize speed))
                  (block cylinder-intersection
                    (let* ((o (transform-point (ray-origin ray) inverse))
                           (d (transform-direction (ray-direction ray) inverse))
                           (ox (aref o 0))
                           (oy (aref o 1))
                           (dx (aref d 0))
                           (dy (aref d 1))
                           (limit epsilon))
                      (declare (dynamic-extent o d))
                      (multiple-value-bind (t1 t2)
                          (quadratic-roots-above limit
                                                 (+ (square dx) (square dy))
                                                 (+ (* 2.0 ox dx) (* 2.0 oy dy))
                                                 (+ (square ox) (square oy) -1.0))
                        (when (> t1 limit)
                          (let* ((ext (ray-extent ray))
                                 (t0 (cond ((and (< t1 ext))
                                            t1)
                                           ((and (< 0.0 t2 ext))
                                            t2)
                                           (t
                                            limit))))
                            (when (> limit)
                              (setf (ray-extent ray) t0)
                              t))))))))))
       :normal
       (lambda (point)
         (let ((p (transform-point point normal-matrix)))
           (declare (optimize speed))
           (%normalize p p)))))))

(defmethod compute-object-extents ((cylinder cylinder) transform)
  (when (and (start-of cylinder) (end-of cylinder))
    (multiple-value-bind (matrix length) (cylinder-values cylinder)
      (transform-bounds (vec -1.0 -1.0 0.0)
                        (vec 1.0 1.0 length)
                        (matrix* transform matrix)))))
