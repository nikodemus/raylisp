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

(deftype axis ()
  '(member 0 1 2))

(declaim (inline next-axis prev-axis))
(defun next-axis (axis)
  (declare (axis axis))
  (if (= axis 2)
      0
      (1+ axis)))

(defun prev-axis (axis)
  (declare (axis axis))
  (if (= axis 0)
      2
      (1- axis)))

(defstruct (kd-node  (:constructor nil)
                     (:conc-name "KD-")
                     (:predicate nil)
                     (:copier nil))
  (min (required-argument) :type vec)
  (max (required-argument) :type vec))

(declaim (inline make-kd-interior-node))
(defstruct (kd-interior-node (:include kd-node))
  (left (required-argument :left) :type kd-node)
  (right (required-argument :right) :type kd-node)
  (axis (required-argument :axis) :type axis)
  (plane-position (required-argument :plane-position) :type single-float))

(declaim (inline make-kd-leaf-node))
(defstruct (kd-leaf-node (:include kd-node) (:predicate kd-leaf-p))
  objects)

(declaim (inline kd-left kd-right kd-axis kd-plane-position kd-depth))

(defun kd-left (kd-node)
  (kd-interior-node-left kd-node))

(defun kd-right (kd-node)
  (kd-interior-node-right kd-node))

(defun kd-axis (kd-node)
  (kd-interior-node-axis kd-node))

(defun kd-plane-position (kd-node)
  (kd-interior-node-plane-position kd-node))

(defun kd-depth (kd-node)
  (labels ((rec (node)
             (if (kd-interior-node-p node)
                 (1+ (max (rec (kd-interior-node-left node))
                          (rec (kd-interior-node-right node))))
                 1)))
    (rec kd-node)))

(defun kd-leaf-info (kd-node)
  (let ((empty 0)
        (non-empty nil))
    (labels ((rec (node)
               (cond ((kd-leaf-p node)
                      (let ((objects (kd-objects node)))
                        (if objects
                            (push (length objects) non-empty)
                            (incf empty))))
                     (t
                      (rec (kd-left node))
                      (rec (kd-right node))))))
      (rec kd-node)
      (let* ((n (length non-empty))
             (total (+ empty n)))
        (list :total total
              :empty (float (/ empty total))
              :mean (float (mean non-empty))
              :median (float (median non-empty))
              :max (reduce #'max non-empty)
              :min (reduce #'min non-empty))))))

(declaim (inline kd-objects))

(defun kd-objects (kd-node)
  (kd-leaf-node-objects kd-node))

(defconstant +kd-stack-depth+ 50)

(defconstant +kd-pointer-stack-node-offset+ 0)
(defconstant +kd-pointer-stack-prev-offset+ 1)
(defconstant +kd-pointer-stack-entry-size+  2)

(defconstant +kd-float-stack-distance-offset+ 0)
(defconstant +kd-float-stack-point-offset+    1) ; and 2, 3
(defconstant +kd-float-stack-entry-size+      4)

(declaim (inline make-kd-float-stack make-kd-pointer-stack))
(defun make-kd-float-stack ()
  ;; 0 for the distance, 1-3 for the point
  (make-array (* 4 +kd-stack-depth+) :element-type 'single-float))
(defun make-kd-pointer-stack ()
  ;; 0 for node index, 1 for prev index, 2 for entry index.
  (make-array (* 3 +kd-stack-depth+) :element-type 't))

(declaim (inline push-kd-root-entry))
(defun push-kd-root-entry (pointer-stack float-stack entry-distance exit-distance
                           origin direction)
  ;; Entry information
  (setf (aref float-stack +kd-float-stack-distance-offset+) entry-distance)
  (if (>= entry-distance 0.0)
      (macrolet ((dim (n)
                   `(setf (aref float-stack (+ , n +kd-float-stack-point-offset+))
                          (+ (aref origin ,n) (* (aref direction ,n) entry-distance)))))
        (dim 0)
        (dim 1)
        (dim 2))
      (macrolet ((dim (n)
                   `(setf (aref float-stack (+ , n +kd-float-stack-point-offset+))
                          (aref origin ,n))))
        (dim 0)
        (dim 1)
        (dim 2)))
  ;; Exit information
  (setf (aref float-stack (+ +kd-float-stack-distance-offset+
                             +kd-float-stack-entry-size+))
        exit-distance)
  (macrolet ((dim (n)
               `(setf (aref float-stack (+ ,n
                                           +kd-float-stack-point-offset+
                                           +kd-float-stack-entry-size+))
                      (+ (aref origin ,n) (* (aref direction ,n) exit-distance)))))
    (dim 0)
    (dim 1)
    (dim 2))
  (setf (aref pointer-stack (+ +kd-pointer-stack-node-offset+ +kd-pointer-stack-entry-size+))
        nil))

(declaim (inline push-kd-traversal))
(defun push-kd-traversal (pointer-stack float-stack pointer prev node distance origin direction axis split)
  (let ((base (* pointer +kd-pointer-stack-entry-size+)))
    (setf (aref pointer-stack (+ +kd-pointer-stack-prev-offset+ base)) prev
          (aref pointer-stack (+ +kd-pointer-stack-node-offset+ base)) node))
  (let ((base (* pointer +kd-float-stack-entry-size+)))
    (setf (aref float-stack (+ +kd-float-stack-distance-offset+ base)) distance)
    (let ((next-axis (next-axis axis))
          (prev-axis (prev-axis axis))
          (point-base (+ +kd-float-stack-point-offset+ base)))
      (setf (aref float-stack (+ point-base axis)) split)
      (setf (aref float-stack (+ point-base next-axis))
            (+ (aref origin next-axis) (* distance (aref direction next-axis))))
      (setf (aref float-stack (+ point-base prev-axis))
            (+ (aref origin prev-axis) (* distance (aref direction prev-axis)))))))

(declaim (inline kd-stack-projection))
(defun kd-stack-projection (float-stack pointer axis)
  (aref float-stack (+ axis
                       +kd-float-stack-point-offset+
                       (* pointer +kd-float-stack-entry-size+))))

(declaim (inline kd-stack-distance))
(defun kd-stack-distance (float-stack pointer)
  (let ((base (* pointer +kd-float-stack-entry-size+)))
    (aref float-stack (+ +kd-float-stack-distance-offset+ base))))

(declaim (inline kd-stack-node))
(defun kd-stack-node (pointer-stack pointer)
  (let ((base (* pointer +kd-pointer-stack-entry-size+)))
    (aref pointer-stack (+ +kd-pointer-stack-node-offset+ base))))

(declaim (inline kd-stack-prev))
(defun kd-stack-prev (pointer-stack pointer)
  (let ((base (* pointer +kd-pointer-stack-entry-size+)))
    (aref pointer-stack (+ +kd-pointer-stack-prev-offset+ base))))

(defun find-intersection-in-kd-tree (ray root counters shadowp)
  (flet ((kd-intersect (objects min max)
           (%find-intersection ray objects min max counters shadowp)))
    (declare (dynamic-extent #'kd-intersect))
    (kd-traverse #'kd-intersect ray root)))

;;;; TRAVERSING A KD-TREE

(declaim (inline ray/box-intersections))
(defun ray/box-intersections (ray bmin bmax)
  (declare (type vec bmin bmax)
           (optimize speed))
  (let ((dir (ray-direction ray))
        (orig (ray-origin ray)))
    (with-arrays (dir orig)
     (let ((ox (orig 0))
           (oy (orig 1))
           (oz (orig 2))
           (dx (dir 0))
           (dy (dir 1))
           (dz (dir 2)))
       (flet ((sides (axis oc dc)
                (if (not (= dc 0.0))
                    (let ((t1 (/ (- (aref bmin axis) oc) dc))
                          (t2 (/ (- (aref bmax axis) oc) dc)))
                      (if (> t1 t2)
                          (values t2 t1)
                          (values t1 t2)))
		    (values 0.0 float-positive-infinity))))
         (declare (inline sides))
         (let-values (((x1 x2) (sides 0 ox dx))
                      ((y1 y2) (sides 1 oy dy))
                      ((z1 z2) (sides 2 oz dz)))
           (let ((t1 (max x1 y1 z1))
                 (t2 (min x2 y2 z2)))
             (if (> t1 t2)
                 (values nil 0.0)
                 (values t1 t2)))))))))

;;; RayTravAlgRECB from Appendix C.
(defun kd-traverse (function ray root)
  (declare (kd-node root)
           (function function)
           (ray ray)
           (optimize speed))
  (multiple-value-bind (entry-distance exit-distance)
      (ray/box-intersections ray (kd-min root) (kd-max root))
    (declare (type (or null float) entry-distance)
             (float exit-distance))
    (when entry-distance
      (unless (kd-interior-node-p root)
        (let ((objects (kd-objects root)))
          (return-from kd-traverse
            (when objects (funcall function objects nil nil)))))
      (let ((float-stack (make-kd-float-stack))
            (pointer-stack (make-kd-pointer-stack))
            (current-node root)
            (entry-pointer 0)
            (exit-pointer 1)
            (far-child nil)
            (ray-origin (ray-origin ray))
            (ray-direction (ray-direction ray)))
        (declare (dynamic-extent float-stack pointer-stack)
                 (fixnum entry-pointer exit-pointer))
        (push-kd-root-entry pointer-stack float-stack
                            entry-distance exit-distance ray-origin ray-direction)
        (loop while current-node
              do (loop until (kd-leaf-p current-node)
                       do (tagbody
                             (let* ((split (kd-plane-position current-node))
                                    (axis (kd-axis current-node))
                                    (entry-projection
                                     (kd-stack-projection float-stack entry-pointer axis))
                                    (exit-projection
                                     (kd-stack-projection float-stack exit-pointer axis)))
                               (cond ((<= entry-projection split)
                                      (cond ((<= exit-projection split)
                                             (setf current-node (kd-left current-node))
                                             (go :cont))
                                            ((= exit-projection split)
                                             (setf current-node (kd-right current-node))
                                             (go :cont))
                                            (t
                                             (setf far-child (kd-right current-node)
                                                   current-node (kd-left current-node)))))
                                     ((< split exit-projection)
                                      (setf current-node (kd-right current-node))
                                      (go :cont))
                                     (t
                                      (setf far-child (kd-left current-node)
                                            current-node (kd-right current-node))))
                               (let ((distance (/ (- split (aref ray-origin axis))
                                                  (aref ray-direction axis)))
                                     (prev-pointer exit-pointer))
                                 (incf-fixnum exit-pointer)
                                 (when (= exit-pointer entry-pointer)
                                   (incf-fixnum exit-pointer))
                                 (push-kd-traversal pointer-stack float-stack exit-pointer
                                                    prev-pointer far-child
                                                    distance ray-origin ray-direction
                                                    axis split)))
                           :cont))
                 (when current-node
                   (let ((objects (kd-objects current-node)))
                     (when objects
                       (multiple-value-bind (result info)
                           (funcall function objects
                                    (kd-stack-distance float-stack entry-pointer)
                                    (kd-stack-distance float-stack exit-pointer))
                         (when result
                           (return-from kd-traverse (values result info)))))))
                 (setf entry-pointer exit-pointer
                       current-node (kd-stack-node pointer-stack exit-pointer)
                       exit-pointer (kd-stack-prev pointer-stack entry-pointer)))))))

;;;; KD TREE BUILDING in O(N log N)
;;;;
;;;; From "On building fast kd-Trees for Ray Tracing, and on doing that in O(N
;;;; log N)" by Ingo Wald and Vlastimil Havran, 2006
;;;;
;;;; See: papers/ingo06rtKdtree.pdf
;;;;
;;;; This is essentially what the paper says, except we don't do perfect splits.
;;;;
;;;; The interface is somewhat generic: define appropriate methods on
;;;;
;;;;   KD-SET-SIZE set
;;;;   MAP-KD-SET function set (calls function with ub32 ids)
;;;;   MAKE-KD-SUBSET ids set
;;;;   KD-OBJECT-MIN id set
;;;;   KD-OBJECT-MAX id set
;;;;
;;;; and you can hand your own data to the implemntation and cast rays at it
;;;; using KD-TRAVERSE. See Eg. objects/mesh.lisp for what this is good for.
;;;;
;;;; Ideally you should always dispatch on SET -- and the two first ones you
;;;; really have no option.

(defgeneric kd-set-size (set))
(defgeneric map-kd-set (function set))
(defgeneric make-kd-subset (subset set))
(defgeneric kd-object-min (object set))
(defgeneric kd-object-max (object set))

;;;; SPLIT EVENTS
;;;;
;;;; Pack them into a vector of ub32: less memory indirection, less work
;;;; for the GC (no pointers), less space taken up.

;;; Start, parellel, and end events.
(defconstant .e+ 0)
(defconstant .e! 1)
(defconstant .e- 2)

(declaim (inline make-events))
(defun make-events (n)
  (make-array (* 2 n) :element-type '(unsigned-byte 32)))

(defun no-events ()
  (load-time-value (make-array 0 :element-type '(unsigned-byte 32)) t))

(declaim (inline %shrink-events))
(defun %shrink-events (vector n)
  (sb-kernel:%shrink-vector vector (* 2 n)))

(declaim (inline event-count))
(defun event-count (vector)
  (truncate (length vector) 2))

(declaim (inline save-event))
(defun save-event (vector n &key type id k e)
  (let ((p (* 2 n)))
    (setf (aref vector p) (logior (ash id 4) (ash type 2) k)
          (aref vector (1+ p)) (pack-single e))
    n))

(declaim (inline event-data))
(defun event-data (vector n)
  (aref vector (* 2 n)))

(declaim (inline event-e event-packed-e))
(defun event-packed-e (vector n)
  (aref vector (1+ (* 2 n))))
(defun event-e (vector n)
  (unpack-single (event-packed-e vector n)))

(declaim (inline event-data-k event-data-type event-data-id))
(defun event-data-k (data)
  (ldb (byte 2 0) data))
(defun event-data-type (data)
  (ldb (byte 2 2) data))
(defun event-data-id (data)
  (ldb (byte 28 4) data))

(declaim (inline swap-events))
(defun swap-events (vector i j)
  (let* ((ni (* 2 i))
         (nj (* 2 j))
         (i-data (aref vector ni))
         (i-e (aref vector (1+ ni)))
         (j-data (aref vector nj))
         (j-e (aref vector (1+ nj))))
    (setf (aref vector ni) j-data
          (aref vector (1+ ni)) j-e)
    (setf (aref vector nj) i-data
          (aref vector (1+ nj)) i-e)
    vector))

(declaim (inline set-event))
(defun set-event (vector i data packed-e)
  (let ((p (* 2 i)))
    (setf (aref vector p) data
          (aref vector (1+ p)) packed-e)
    vector))

(declaim (inline vector-event<))
(defun vector-event< (vector i j)
  (let* ((ni (* 2 i))
         (nj (* 2 j))
         (ie (unpack-single (aref vector (1+ ni))))
         (je (unpack-single (aref vector (1+ nj)))))
    (or (< ie je)
        (and (= ie je)
             (< (event-data-type (aref vector ni))
                (event-data-type (aref vector nj)))))))

(defun print-event (vector i &optional (stream *standard-output*))
  (let ((data (event-data vector i)))
    (format stream "~A ~A:~A(~S)"
            (ecase (event-data-type data)
              (#..e+ '+)
              (#..e! '!)
              (#..e- '-))
            (event-data-k data)
            (event-e vector i)
            (event-data-id data))))

(defun print-events (vector &key (start 0) end (stream *standard-output*))
  (unless end
    (setf end (event-count vector)))
  (flet ((print-it (s)
           (loop for i from start below end
                 do (format s "(~A)  " i)
                    (print-event vector i s)
                    (terpri s))))
    (if stream
        (print-it stream)
        (with-output-to-string (s)
          (print-it s)))))

(defun sort-events (vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) vector)
           (optimize speed))
  (let ((scratch (make-array (length vector) :element-type '(unsigned-byte 32))))
    (labels ((mergesort (left right)
               (declare (fixnum left right) (optimize (safety 0)))
               (unless (= right (1+ left))
                 (let* ((size (- right left))
                        (middle (logand most-positive-fixnum (+ left (truncate size 2)))))
                   (declare (fixnum size))
                   ;; Sort both halves
                   (mergesort left middle)
                   (mergesort middle right)
                   ;; Merge the results
                   (let ((left-pointer left)
                         (right-pointer middle))
                     (declare (fixnum left-pointer right-pointer))
                     (loop for i from 0 below size
                           for 2i = (logand most-positive-fixnum (ash i 1))
                           when (and (< left-pointer middle)
                                      (or (>= right-pointer right)
                                          (vector-event< vector left-pointer right-pointer)))
                           do (let ((p (* 2 left-pointer)))
                                (setf (aref scratch 2i) (aref vector p)
                                      (aref scratch (1+ 2i)) (aref vector (1+ p)))
                                (incf left-pointer 1))
                           else
                           do (let ((p (* 2 right-pointer)))
                                (setf (aref scratch 2i) (aref vector p)
                                      (aref scratch (1+ 2i)) (aref vector (1+ p)))
                                (incf right-pointer 1)))
                     (replace vector scratch :start1 (* 2 left) :start2 0 :end2 (* 2 size)))))))
      (mergesort 0 (event-count vector))
      vector)))

(defun events->subset (events set)
  (declare (type (simple-array (unsigned-byte 32) (*)) events))
  (let (ids)
    (dotimes (i (event-count events))
      (pushnew (event-data-id (event-data events i)) ids))
    (make-kd-subset ids set)))

(declaim (single-float *kd-traversal-cost* *intersection-cost*))
(defparameter *kd-traversal-cost* 0.2)
(defparameter *intersection-cost* 0.05)
(defparameter *kd-gc-threshold* (* 1024 1024 256))

(defun build-kd-tree (set min max &key verbose (name "KD-tree") (type "objects"))
  (let* ((size (kd-set-size set))
         ;; The INFO vector is used by SPLIT-EVENTS to record object information.
         ;; The INFO-TAG is a semi-unique 29-bit ID for the build-round.
         (info (make-array size :element-type '(unsigned-byte 32)))
         (info-tag 0)
         (tree nil)
         (gc-threshold (sb-ext:bytes-consed-between-gcs)))
    (flet ((build-it ()
             (labels ((rec (n events min max)
                        (macrolet ((inc/29 (x)
                                     `(setf ,x (ldb (byte 29 0) (1+ ,x)))))
                          (multiple-value-bind (e k side cost) (find-plane n events min max)
                            (if (> cost (* *intersection-cost* n))
                                (make-kd-leaf-node :min min :max max
                                                   :objects (when (plusp (length events))
                                                              (events->subset events set)))
                                (multiple-value-bind (left-events right-events nl nr)
                                    (split-events events e k side info (inc/29 info-tag))
                                  #+nil
                                  (break "split ~S:~S, ~S/~S" k e nl nr)
                                  ;; Split the voxel.
                                  (let ((lmax (copy-vec max))
                                        (rmin (copy-vec min)))
                                    (setf (aref lmax k) e
                                          (aref rmin k) e)
                                    (make-kd-interior-node
                                     :plane-position e
                                     :axis k
                                     :min min
                                     :max max
                                     :left (rec nl left-events min lmax)
                                     :right (rec nr right-events rmin max)))))))))
             (setf tree (rec size (build-events size set) min max)))))
      (unwind-protect
           (progn
             ;; Building large KD trees conses much more than we'd really like,
             ;; but this stops us from tenuring temporary objects too deep, and
             ;; ensures that GC time doesn't overwhelm us.
             (when (< gc-threshold *kd-gc-threshold*)
               (setf (sb-ext:bytes-consed-between-gcs) *kd-gc-threshold*))
             (cond (verbose
                   (format t "~&Building ~A for ~A ~A~%" name size type)
                   (finish-output t)
                   (fresh-line t)
                   (sb-ext:call-with-timing
                    (lambda (&rest timings)
                      (format t "  Tree depth: ~A~%" (kd-depth tree))
                      (format t "  Real: ~,2F, User: ~,2F System: ~,2F seconds~%  ~
                             GC: ~,2F seconds, ~,2F Mb consed~%"
                              (real-time-seconds timings)
                              (user-run-time-seconds timings)
                              (system-run-time-seconds timings)
                              (gc-run-time-seconds timings)
                              (gc-mb-consed timings))
                      (finish-output t))
                    #'build-it))
                  (t
                   (build-it))))
        (setf (sb-ext:bytes-consed-between-gcs) gc-threshold)))))

(defun build-events (size set)
  ;; 3 dimensions, max 2 events per object
  (let ((events (make-events (* 6 size)))
        (p 0))
    (declare (fixnum p))
    (map-kd-set (lambda (id)
                  (let ((min (kd-object-min id set))
                        (max (kd-object-max id set)))
                    (dotimes (k 3)
                      (flet ((make (type)
                               (save-event events p
                                           :id id
                                           :type type
                                           :e (if (= .e- type)
                                                  (aref max k)
                                                  (aref min k))
                                           :k k)
                               (incf p)))
                        (cond ((= (aref min k) (aref max k))
                               (make .e!))
                              (t
                               (make .e+)
                               (make .e-)))))))
                set)
    (sort-events (%shrink-events events p))))

(defconstant +kd-left-only+  #b001)
(defconstant +kd-right-only+ #b010)
(defconstant +kd-counted+    #b100)
(defconstant +kd-class-mask+ #b111)

;;; We use the 29 high bits of each element of the INFO-vector to store
;;; the seminunique TAG. If TAG is zero, it may have rolled over and we
;;; need to zap the whole INFO -- otherwise we can use lazy updates.
;;;
;;; The 3 low bits of each element are the classification: left/right/both,
;;; and counted/uncounted.
(defun split-events (events e k side info tag)
  (declare (type (simple-array (unsigned-byte 32) (*)) events)
           (single-float e)
           (fixnum k)
           (type (simple-array (unsigned-byte 32) (*)) info)
           (type (unsigned-byte 29) tag)
           (optimize speed))
  (when (zerop tag)
    ;; Tag has rolled over, need to clear the whole info.
    (fill info 0))
  (let ((tag3 (ash tag 3))
        (n-events (event-count events))
        ;; COMMON tells us if all events seem to be on the same side.
        (common 0))
    (declare (fixnum common))
    (flet ((classify (event-data class)
             ;; Update COMMON, and store info.
             (setf common (logior common class)
                   (aref info (event-data-id event-data)) (logior tag3 class))
             class)
           (ensure-classification (event-data)
             (let* ((id (event-data-id event-data))
                    (event-info (aref info id)))
               ;; If tag is valid, we're good. Otherwise this is as-of yet
               ;; unclassified: update INFO, return zero.
               (cond ((= tag3 (logandc2 event-info +kd-class-mask+))
                      (logand +kd-class-mask+ event-info))
                     (t
                      (setf (aref info id) tag3)
                      0)))))
      ;; Sweep 1: Classify along K
      (dotimes (i n-events)
        (let ((data (event-data events i)))
          (when (= k (event-data-k data))
            (let ((type (event-data-type data))
                  (ee (event-e events i)))
              (cond ((= .e- type)
                     (when (<= ee e) (classify data +kd-left-only+)))
                    ((= .e+ type)
                     (when (>= ee e) (classify data +kd-right-only+)))
                    ;; The rest are for .e! types.
                    ((or (< ee e) (and (= ee e) (eq :left side)))
                     (classify data +kd-left-only+))
                    ((or (> ee e) (and (= ee e) (eq :right side)))
                     (classify data +kd-right-only+)))))))
      ;; Sweep 2: split into left and right -- including other Ks
      (let ((left-list (when (logtest common +kd-left-only+)
                         (make-events n-events)))
            (right-list (when (logtest common +kd-right-only+)
                          (make-events n-events)))
            (left 0) (right 0) (pl 0) (pr 0))
        (declare (fixnum left right pl pr)
                 (type (or null (simple-array (unsigned-byte 32) (*))) left-list right-list))
        (macrolet ((inc (x)
                     `(1- (setf ,x (logand most-positive-fixnum (+ ,x 1))))))
          (dotimes (i n-events)
            (let* ((data (event-data events i))
                   (class (ensure-classification data)))
              (macrolet ((handle-event (&key left-side right-side)
                           `(progn
                              ,@(when left-side
                                      `((unless left-list
                                          (setf left-list (make-events n-events)))
                                        (set-event left-list (inc pl) data (event-packed-e events i))))
                              ,@(when right-side
                                      `((unless right-list
                                          (setf right-list (make-events n-events)))
                                        (set-event right-list (inc pr) data (event-packed-e events i))))
                              (unless (logtest class +kd-counted+)
                                ,@(when left-side `((inc left)))
                                ,@(when right-side `((inc right)))
                                (classify data (logior +kd-counted+ class))))))
                (cond ((logtest class +kd-left-only+)
                       (handle-event :left-side t))
                      ((logtest class +kd-right-only+)
                       (handle-event :right-side t))
                      (t
                       (handle-event :left-side t :right-side t)))))))
        (if left-list
            (%shrink-events left-list pl)
            (setf left-list (no-events)))
        (if right-list
            (%shrink-events right-list pr)
            (setf right-list (no-events)))
        (values left-list right-list left right)))))

(defun find-plane (n events min max)
  (declare (type (simple-array (unsigned-byte 32) (*)) events)
           (optimize speed))
  (let* ((nl (make-array 3 :element-type 'fixnum))
         (np (make-array 3 :element-type 'fixnum))
         (nr (make-array 3 :element-type 'fixnum :initial-contents (list n n n)))
         (n-events (event-count events))
         (best-side nil)
         (best-cost #.sb-ext:single-float-positive-infinity)
         (best-e #.sb-ext:single-float-positive-infinity)
         (best-k 0))
    (declare (dynamic-extent nl nr np)
             (single-float best-cost best-e)
             (type (integer 0 2) best-k))
    (macrolet ((inc (x &optional (d 1))
                 `(setf ,x (logand most-positive-fixnum (+ ,x ,d))))
               (dec (x &optional (d 1))
                 `(setf ,x (logand most-positive-fixnum (- ,x ,d)))))
      (loop with i fixnum = 0
            while (< i n-events)
            do (let* ((data (event-data events i))
                      (e (event-packed-e events i))
                      (k (event-data-k data))
                      (p+ 0)
                      (p- 0)
                      (p! 0))
                 (declare (fixnum p+ p- p!))
                 (flet ((event-ok (i type)
                          (and (< i n-events)
                               (= k (event-data-k (setf data (event-data events i))))
                               (= type (event-data-type data))
                               (= e (event-packed-e events i)))))
                   (declare (inline event-ok))
                   (loop while (event-ok i .e-)
                         do (inc p-)
                            (inc i))
                   (loop while (event-ok i .e!)
                         do (inc p!)
                            (inc i))
                   (loop while (event-ok i .e+)
                         do (inc p+)
                            (inc i)))
                 (setf (aref np k) p!)
                 (dec (aref nr k) p!)
                 (dec (aref nr k) p-)
                 (let ((e (unpack-single e)))
                   (multiple-value-bind (cost side)
                       (surface-area-heuristic min max e k (aref nl k) (aref nr k) (aref np k))
                     (declare (single-float cost))
                     (when (< cost best-cost)
                       (setf best-cost cost
                             best-e e
                             best-k k
                             best-side side))))
                 (inc (aref nl k) p+)
                 (inc (aref nl k) p!)
                 (setf (aref np k) 0))))
    (values best-e best-k best-side best-cost)))

(defun surface-area-heuristic (min max e k nl nr np)
  (declare (optimize speed)
           (fixnum nl nr np))
  (flet  ((surface-area (min max)
            (declare (type vec min max))
            (let ((x (- (aref max 0) (aref min 0)))
                  (y (- (aref max 1) (aref min 1)))
                  (z (- (aref max 2) (aref min 2))))
              (+ (* x y) (* x z) (* y z))))
          (split-cost (pl pr nl nr)
            (if (or (and (= 1.0 pl) (= 0 nr))
                    (and (= 1.0 pr) (= 0 nl)))
                #.sb-ext:single-float-positive-infinity
                (* (if (or (= 0 nl) (= 0 nr))
                       0.8
                       1.0)
                   (+ *kd-traversal-cost*
                      (* *intersection-cost* (+ (* pl nl) (* pr nr))))))))
    ;; Split the voxel
    (let ((l-max (copy-vec max))
          (r-min (copy-vec min)))
      (declare (dynamic-extent l-max r-min))
      (setf (aref l-max k) e
            (aref r-min k) e)
      ;; The bound are now min, l-max, r-min, max
      (let* ((area (surface-area min max))
             (pl (/ (surface-area min l-max) area))
             (pr (/ (surface-area r-min max) area))
             (c.p->l (split-cost pl pr (logand most-positive-fixnum (+ nl np)) nr))
             (c.p->r (split-cost pl pr nl (logand most-positive-fixnum (+ nr np)))))
        (if (and (< c.p->l c.p->r) (< pl 1.0))
            (values c.p->l :left)
            (values c.p->r :right))))))

;;;; SAVING KD-TREES ON DISK
;;;;
;;;; Binary format:
;;;;
;;;; GENERIC HEADER
;;;;   #x4dee75ee (magic bytes)
;;;;   ub32 (format version, currently zero)
;;;; FORMAT 0 HEADER
;;;;   ub32 (total number of nodes)
;;;; NODE START
;;;;   ub32 (node number, odd = leaf, even = interior)
;;;;   single,single,single (node min)
;;;;   single,single,single (node max)
;;;;   ub8 (axis 0-2 for interior nodes, #xf for child nodes)
;;;;   IF LEAF
;;;;     ub32 (number of objects in node)
;;;;     ub32,ub32,ub32... (specified number of object ids)
;;;;   ELSE
;;;;     ub32 (left child id)
;;;;     ub32 (right child id)
;;;;     single (plane position)
;;;; NODE END
;;;;
;;;;   child nodes always come before their parents -- the last
;;;;   node is the root.

(defconstant +kd-tree-magic-bytes+ #x4dee75ee)
(defconstant +kd-tree-format-version+ #x0)
(defconstant +kd-tree-leaf-mark+ #xf)

(defun read-word (stream)
  (macrolet ((one ()
               `(the (unsigned-byte 8) (read-byte stream))))
    (logior (one)
            (ash (one) 8)
            (ash (one) 16)
            (ash (one) 24))))

(defun write-word (word stream)
  (declare (type (unsigned-byte 32) word))
  (macrolet ((one (byte)
               `(write-byte ,byte stream)))
    (one (ldb (byte 8 0) word))
    (one (ldb (byte 8 8) word))
    (one (ldb (byte 8 16) word))
    (one (ldb (byte 8 24) word)))
  word)

(defun write-single (single stream)
  (declare (single-float single))
  (write-word (pack-single single) stream))

(defun read-single (stream)
  (unpack-single (read-word stream)))

(defun map-kd-tree (function tree)
  (declare (function function))
  (labels ((walk (node)
             (unless (kd-leaf-p node)
               (walk (kd-left node))
               (walk (kd-right node)))
             (funcall function node)))
    (walk tree)))

(defun load-kd-tree (pathname)
  (let ((pathname (merge-pathnames pathname (make-pathname :type "kd"))))
    (with-open-file (f pathname :element-type '(unsigned-byte 8))
      (read-kd-tree f))))

(defun save-kd-tree (tree pathname)
  (let ((pathname (merge-pathnames pathname (make-pathname :type "kd"))))
    (with-open-file (f pathname
                       :element-type '(unsigned-byte 8)
                       :direction :output)
      (write-kd-tree tree f))))

(defun write-kd-tree (tree stream)
  (let ((nodes (make-hash-table))
        (last -1))
    ;; Number the nodes
    (map-kd-tree (lambda (node)
                   (setf (gethash node nodes)
                         (hash-table-count nodes)))
                 tree)
    ;; Write header
    (write-word +kd-tree-magic-bytes+ stream)
    (write-word +kd-tree-format-version+ stream)
    (write-word (hash-table-count nodes) stream)
    (flet ((write-node (node)
             (write-word (setf last (gethash node nodes)) stream)
             (let ((min (kd-min node))
                   (max (kd-max node)))
               (write-single (aref min 0) stream)
               (write-single (aref min 1) stream)
               (write-single (aref min 2) stream)
               (write-single (aref max 0) stream)
               (write-single (aref max 1) stream)
               (write-single (aref max 2) stream)
               (cond ((kd-leaf-p node)
                      (write-byte +kd-tree-leaf-mark+ stream)
                      (let* ((objects (kd-objects node))
                             (n (length objects)))
                        (write-word (length objects) stream)
                        (dotimes (i n)
                          (write-word (aref objects i) stream))))
                     (t
                      (write-byte (kd-axis node) stream)
                      (write-word (gethash (kd-left node) nodes) stream)
                      (write-word (gethash (kd-right node) nodes) stream)
                      (write-single (kd-plane-position node) stream))))))
      (map-kd-tree #'write-node tree)
      tree)))

(defun read-kd-tree (stream)
  (assert (equal '(unsigned-byte 8) (stream-element-type stream)))
  (unless (= +kd-tree-magic-bytes+ (read-word stream))
    (error "Not a serialized Raylisp KD-tree: ~S" (pathname stream)))
  (let ((version (read-word stream)))
    (unless (= +kd-tree-format-version+ version)
      (error "Unknown KD-tree format version: ~S" version)))
  (let* ((n-nodes (read-word stream))
         (nodes (make-array n-nodes))
         (root (1- n-nodes)))
    (loop
      (let ((node-number (read-word stream))
            (min (vec (read-single stream) (read-single stream) (read-single stream)))
            (max (vec (read-single stream) (read-single stream) (read-single stream)))
            (axis-or-leaf-mark (read-byte stream)))
        (setf (aref nodes node-number)
              (if (= +kd-tree-leaf-mark+ axis-or-leaf-mark)
                  (let ((n-objects (read-word stream)))
                    (make-kd-leaf-node
                     :min min
                     :max max
                     :objects
                     (when (plusp n-objects)
                       (let ((objects (make-array n-objects :element-type '(unsigned-byte 32))))
                         (dotimes (i n-objects)
                           (setf (aref objects i) (read-word stream)))
                         objects))))
                  (make-kd-interior-node
                   :min min
                   :max max
                   :left (aref nodes (read-word stream))
                   :right (aref nodes (read-word stream))
                   :axis axis-or-leaf-mark
                   :plane-position (read-single stream))))
        (when (= node-number root)
          (return-from read-kd-tree (aref nodes root)))))))
