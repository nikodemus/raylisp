;;;; On building fast kd-Trees for Ray Tracing, and on doing that in O(N log N)
;;;; by Ingo Wald and Vlastimil Havran, 2006
;;;;
;;;;  papers/ingo06rtKdtree.pdf
;;;;
;;;; Contents of the paper in Lisp, to facilitate understanding.

;;;; 2 Building KD-Trees for Ray Tracing

;;;; Algorithm 1 Recursive KD-tree build
(defun rec-build (triangles voxel)
  (if (terminate triangles voxel)
      (leaf-node triangles)
      (let ((p (find-plane triangles voxel)))
        (multiple-value-bind (left-voxel right-voxel) (split voxel p)
          (let ((left-triangles (triangles-in-voxel triangles left-voxel))
                (right-triangles (triangles-in-voxel triangles right-voxel)))
            (node p
                  (rec-build left-triangles left-voxel)
                  (rec-build right-triangles right-voxel)))))))
(defun build-kd-tree (triangles)
  (rec-build triangles (voxel-for-triangles triangles)))

;;;; 2.1 Naive, "spatial median" KD-Trees

(defun find-plane/naive (triangles voxel)
  (let* ((pk (mod (current-subdivision-depth) 3))
         (pe (* 1/2 (+ (voxel-min voxel pk) (voxel-max voxel pk)))))
    (plane pk pe)))

(defun terminate/naive (triangles voxel)
  (or (<= (length triangles) +k-tri-target+)
      (>= (current-subdivision-depth) +k-max-depth+)))

;;;; 3. The Surface Area Heuristic (SAH)

;;;; Equation (1)

(defun probability-to-hit-subvoxel-after-hitting-voxel (subvoxel voxel)
  (/ (surface-area subvoxel)
     (surface-area vocel)))

;;;; Equation (2)

(defun expected-cost-of-plane (plane voxel)
  (let ((vl (left-subvoxel plane voxel))
        (vr (right-subvoxel plane voxel)))
    (+ +traversal-step-cost+
       (* (probability-to-hit-subvoxel-after-hitting-voxel vl voxel)
          (voxel-cost vl))
       (* (probability-to-hit-subvoxel-after-hitting-voxel vr voxel)
          (voxel-cost vr)))))

;;;;; 3.1. Local greedy heuristic

;;;;; Equation (3)

(defun cost-of-complete-tree (tree)
  (+ (reduce #'+ (lambda (node)
                   (* (/ (surface-area node) (surface-area tree)) +traversal-step-cost+))
             (tree-nodes tree))
     (reduce #'+ (lambda (leaf)
                   (* (/ (surface-area leaf) (surface-area tree)) +intersection-cost+))
             (tree-leaves tree))))

;;;; Equation (4)

(defun expected-cost-of-subdivision-1 (triangles plane voxel)
  (let ((vl (left-subvoxel plane voxel))
        (vr (right-subvoxel plane voxel)))
    (+ +traversal-step-cost+
       (* (probability-to-hit-subvoxel-after-hitting-voxel left-subvoxel)
          (length (triangles-in-voxel triangles vl))
          +intersection-cost+)
       (* (probability-to-hit-subvoxel-after-hitting-voxel right-subvoxel)
          (length (triangles-in-voxel triangles vr))
          +intersection-cost+))))

;;;; Equation (5)

(defun expected-cost-of-subdivision-2 (triangles plane voxel)
  (let ((vl (left-subvoxel plane voxel))
        (vr (right-subvoxel plane voxel)))
    (+ +traversal-step-cost+
       (* +intersection-cost+
          (+ (* (/ (surface-area vl) (surface-area voxel))
                (length (triangles-in-voxel triangles vl)))
             (* (/ (surface-area vr) (surface-area voxel))
                (length (triangles-in-voxel triangles vr))))))))

;;;; 3.2. Automatic Termination Criterion

;;;; Equation (6)

(defun terminate/sah (triangles voxel)
  (> (expected-cost-of-subdivision triangles (find-plane triangles voxel) voxel)
     (* +intersection-cost+ (length triangles))))

;;;; 3.3. Modifications and Extensions

;;;; Equation (7)

(defun expected-cost-multiplier (plane)
  (if (or (no-triangles-on-left plane)
          (no-triangles-on-right plane))
      0.8
      1.0))

;;;; 3.4. Split Candidates and Perfect Splits

;;;; 3.5. Accurate Determination of Nl and Nr

;;;; Equation (8)

(defun triangles-left-of-plane (triangles left-voxel-excluding-plane)
  (loop for triangle in triangles
        when (> (area-of-intersection triangle left-voxel-excluding-plane) 0)
        collect triangle))

;;;; Equation (9)

(defun triangles-right-of-plane (triangles right-voxel-excluding-plane)
  (loop for triangle in triangles
        when (> (area-of-intersection triangle right-voxel-excluding-plane) 0)
        collect triangle))

;;;; Equation (10)

(defun triangles-on-plane (triangles plane)
  (loop for triangle in triangles
        when (> (area-of-intersection triangle plane) 0)
        collect triangle))

;;;; Algorithm 2 Final cost heuristic for a given configuration

(defun cost-of-plane (plane prob-l prob-r nl nr)
  (* (expected-cost-multiplier plane)
     (+ +traversal-step-cost+
        (* +intersection-cost+ (+ (* prob-l nl) (* prob-r nr))))))

(defun sah (plane voxel nl nr np)
  (multiple-value-bind (vl vr) (split-voxel plane voxel)
    (let* ((prob-l (/ (surface-area vl) (surface-area voxel)))
           (prob-r (/ (surface-area vr) (surface-area voxel)))
           (cost-p->left
            (cost-of-plane plane prob-l prob-r
                           (+ (triangles-left-of-plane plane) (triangles-on-plane plane))
                           (triangles-right-of-plane plane)))
           (cost-p->right
            (cost-of-plane plane prob-l prob-r
                           (triangles-left-of-plane plane)
                           (+ (triangles-right-of-plane plane) (triangles-on-plane plane)))))
      (if (< cost-p->l cost-p->r)
          (values cost-p->l :left)
          (values cost-p->r :right)))))

;;;; On Building SAH-based KD-Trees

;;;; 4.1. Naive O(N^2) Plane Selection

;;;; Algorithm 3 Algrorithm for naive O(N^2) plane selection

(defun perfect-splits (triangle voxel)
  (let ((bounds (clip-to triangle voxel)))
    (loop for axis from 1 upto 3
          appending (list (plane axis (min-bound bounds))
                          (plane axis (max-bound bounds))))))

(defun classify (triangles voxel-left voxel-right plane)
  (let ((t-l nil)
        (t-r nil)
        (t-p nil))
    (dolist (triangle triangles)
      (cond ((and (lies-in-plane-p triangle plane)
                  (> (area-of-intersection plane voxel) 0))
             (push triangle t-p))
            (t
             (when (> (area-of-intersection triangle (sans-plane voxel-left plane)) 0)
               (push triangle t-l))
             (when (> (area-of-intersection triangle (sans-plane voxel-right plane)) 0)
               (push triangle t-r)))))
    (values t-l t-r t-p)))

(defun naive-partition (triangles voxel)
  (let ((best-cost +inf+)
        (best-plane nil)
        (p-side nil)
        vl vr)
      (dolist (triangle triangles)
        (dolist (p (perfect-splits triangle voxel))
          (setf (values vl vr) (split-voxel voxel p))
          (multiple-value-bind (tl tr tp) (classify triangles vl vr p)
            (multiple-value-bind (cost side) (sah voxel p (length tl) (length tr) (length tp))
              (when (< cost best-cost)
                (setf best-cost cost
                      best-plane p
                      p-side side))))))
      (multiple-value-bind (tl tr tp) (classify triangles vl vr p)
        (if (eq :left p-side)
            (values p (union tl tp) tr)
            (values p tl (union tr tp))))))

;;;; 4.2. O(N log2 N) Construction

;;;; Equation (11)

(defun next-nl ()
  (+ (prev-nl) (prev-p!) (prev-p+)))

;;;; Equation (12)

(defun next-nr ()
  (- (prev-nr) (next-p!) (next-p-)))

;;;; Equation (13)

(defun next-np ()
  ;; Paper says prev-p!, but I'm pretty sure that's a typo.
  (next-p!))

;;;; Unnumbered

(defun <e (a b)
  (or (< (point a) (point b))
      (and (= (point a) (point b))
           (< (theta a) (theta b)))))

(defun theta (x)
  (cond ((end-event-p x) 0)
        ((planar-event-p x) 1)
        ((start-event-p x) 2)))

;;;; Algorithm 4 Incremental sweep to find p

(defun find-plane/sweep (triangles voxel)
  (let (best-cost best-plane best-side)
    (loop for k from 1 upto 3
          do (let (eventlist)
               (dolist (tri triangles)
                 (let ((bounds (clip-to tri voxel)))
                   (cond ((planar-p bounds k)
                          (push (event tri (min-bound bounds k) '!) eventlist))
                         (t
                          (push (event tri (min-bound bounds k) '+) eventlist)
                          (push (event tri (max-bound bounds k) '-) eventlist)))))
               (setf eventlist (sort eventlist #'<e))
               (let ((nl 0)
                     (np 0)
                     (nr (length triangles)))
                 (loop with i = 0
                       while (< i (length eventlist))
                       do (let ((p (plane (point (elt eventlist i)) k))
                                (p+ 0)
                                (p- 0)
                                (p! -))
                            (loop while (and (< i (length eventlist))
                                             (= (point (elt eventlist i)) (plane-point p))
                                             (type-is '- (elt eventlist i)))
                                  do (incf p-)
                                     (incf i))
                            (loop while (and (< i (length eventlist))
                                             (= (point (elt eventlist i)) (plane-point p))
                                             (type-is '! (elt eventlist i)))
                                  do (incf p!)
                                     (incf i))
                            (loop while (and (< i (length eventlist))
                                             (= (point (elt eventlist i)) (plane-point p))
                                             (type-is '+ (elt eventlist i)))
                                  do (incf p+)
                                     (incf i))
                            (setf np p!)
                            (decf nr p!)
                            (decf nr p-)
                            (multiple-value-bind (cost side) (sah voxel p nl nr np)
                              (when (< cost best-cost)
                                (setf best-cost cost
                                      best-plane p
                                      best-side side)))
                            (incf nl p+)
                            (incf nl p!)
                            (setf np 0))))))
    (values best-plane best-side))

;;;; 4.3. O(N log N) Build using Sort-free sweeping

(defun build-eventlist (triangles)
  ;; This in explained in the text.
  (let (events)
    (loop for k from 1 upto 3
          do (dolist (tri triangles)
               (let ((bounds (bounds-of tri)))
                 (cond ((planar-p bounds k)
                        (push (event tri k (min-bound bounds k) '!) events))
                       (t
                        (push (event tri k (min-bound bounds k) '+) events)
                        (push (event tri l (max-bound bounds k) '-) events))))))
    ;; Given how the following FIND-PLANE works, I *think* this should
    ;; also make sure that all events at a given point are also sorted by
    ;; the axis -- order of axis is not important, just that all events
    ;; at X on axis K are togather.
    (sort events #'<e)))

;;;; Algorithm 5 Finding the best plane in O(N)

(defun find-plane (n v e)
  (let ((nl.k (vector 0 0 0))
        (np.k (vector 0 0 0))
        (nr.k (vector 0 0 0))
        best-plane
        best-cost
        best-side)
    (loop with i = 0
          while (< i (length e))
          do (let ((p (event-plane (elt e i)))
                   (p+ 0)
                   (p- 0)
                   (p! 0))
               (loop while (and (< i (length e))
                                (= (plane-axis p) (event-axis (elt e i)))
                                (= (plane-point p) (event-point (elt e i)))
                                (eq '- (event-type (elt e i))))
                     do (incf p-)
                        (incf i))
               (loop while (and (< i (length e))
                                (= (plane-axis p) (event-axis (elt e i)))
                                (= (plane-point p) (event-point (elt e i)))
                                (eq '! (event-type (elt e i))))
                     do (incf p!)
                        (incf i))
               (loop while (and (< i (length e))
                                (= (plane-axis p) (event-axis (elt e i)))
                                (= (plane-point p) (event-point (elt e i)))
                                (eq '+ (event-type (elt e i))))
                     do (incf p+)
                        (incf i))
               (setf (aref np.k (plane-axis p)) p!)
               (decf (aref nr.k (plane-axis p)) p!)
               (decf (aref nr.k (plane-axis p)) p-)
               ;; nl &co below for the right axis, of course
               (multiple-value-bind (cost side) (sah v p nl nr np)
                 (when (< cost best-cost)
                   (setf best-cost cost
                         best-plane p
                         best-side side)))
               (incf (aref nl.k (plane-axis p)) p+)
               (incf (aref nl.k (plane-axis p)) p!)
               (setf (aref nl.k (plane-axis p)) 0)))
    (values best-plane best-side)))

;;;; 4.3.1 Splicing and Merging to Maintain Sort Order

;;;; Algorithm 6 Given E and p, classify triangles to be either
;;;; left of, right of, or overlapping p in a single sweep over E

(defun classify-left/right/both (triangles events p)
  (dolist (tri triangles)
    (setf (triangle-side tri) :both))
  (dolist (e events)
    (cond ((and (eq '- (event-type e))
                (= (event-k e) (plane-k p))
                (<= (event-e e) (plane-e p)))
           (setf (triangle-side (event-t e)) :left-only))
          ((and (eq '+ (event-type e))
                (= (event-k e) (plane-k p))
                (>= (event-e e) (plane-e p)))
           (setf (triangle-side (event-t e)) :right-only))
          ((and (eq '! (event-type e))
                (= (event-k e) (plane-k p)))
           (when (or (< (event-e e) (plane-e p))
                     (and (= (event-e e) (plane-e p))
                          (eq :left (plane-side p))))
             (setf (triangle-side (event-t e)) :left-only))
           (when (or (> (event-e e) (plane-e p))
                     (and (= (event-e e) (plane-e p))
                          (eq :right (plane-side p))))
             (setf (triangle-side (event-t e)) :right-only))))))

(defun splice-onto-elo-and-ero (events)
  ;; This is explained in the text.
  (loop for e in events
        when (eq :left-only (triangle-side (event-t e)))
        collect into elo
        when (eq :right-only (triangle-side (event-t e)))
        collect into ero
        finally (return (values elo ero))))



(defun split-straddlers (events p)
  ;; This is explained in the text.
  (let (ebl ebr)
    (loop for e in events
          when (eq :both (triangle-side (event-t e)))
         do (multiple-value-bind (el er) (split-triangle-and-generate-events (event-t e) p)
              (appendf ebl el)
              (appendf erl er)))
    (values (sort ebl #'<e) (sort ebr #'<e))))

(defun merge-strains (elo ero ebl ebr)
  ;; This is explained in the text.
  (values (merge 'list elo ebl #'<e)
          (merge 'list ero ebr #'<e)))





