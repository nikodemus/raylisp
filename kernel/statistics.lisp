(in-package :raylisp)

;;;# Statistics

(defconstant +camera-ray-slot+        0)
(defconstant +reflected-ray-slot+     1)
(defconstant +refracted-ray-slot+     2)
(defconstant +intersection-miss-slot+ 3)
(defconstant +shadow-miss-slot+       4)
(defconstant +intersection-hit-slot+  5)
(defconstant +shadow-hit-slot+        6)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +counter-vector-length+  8))

(deftype counter-vector ()
  `(simple-array (unsigned-byte 32) (,+counter-vector-length+)))

(defun make-counters ()
  (make-array +counter-vector-length+ :element-type '(unsigned-byte 32)))

(defmacro incf* (place)
  `(setf ,place (logand #xffffffff (+ ,place 1))))

(defmacro incf-fixnum (place)
  `(setf ,place (logand most-positive-fixnum (+ ,place 1))))

(defun note-intersection (counters shadowp hitp)
  (declare (counter-vector counters))
  (let ((slot (if hitp
                  (if shadowp
                      +shadow-miss-slot+
                      +intersection-miss-slot+)
                  (if shadowp
                      +shadow-hit-slot+
                      +intersection-hit-slot+))))
    (incf* (aref counters slot)))
  counters)

(defun note-camera-ray (counters)
  (declare (counter-vector counters))
  (incf* (aref counters +camera-ray-slot+))
  counters)

(defun note-reflected-ray (counters)
  (declare (counter-vector counters))
  (incf* (aref counters +reflected-ray-slot+))
  counters)

(defun note-refracted-ray (counters)
  (declare (counter-vector counters))
  (incf* (aref counters +refracted-ray-slot+))
  counters)

(defun counter-plist (counters)
  (let ((hits (aref counters +intersection-hit-slot+))
        (misses (aref counters +intersection-miss-slot+))
        (shadows (aref counters +shadow-hit-slot+))
        (shadow-misses (aref counters +shadow-miss-slot+)))
    (list :camera (aref counters +camera-ray-slot+)
          :reflected (aref counters +reflected-ray-slot+)
          :refracted (aref counters +refracted-ray-slot+)
          :intersections (+ hits misses)
          :hits hits
          :shadow-tests (+ shadows shadow-misses)
          :shadows shadows)))

;;;## Reporting
;;;
;;; Raylisp collects statistics of the rendering process, so that
;;; we have an idea what the time was spent in.
;;;
;;; TODO: benchmark with and without statistic collection: we can't
;;; afford to spend too much time here.

(defun print-report (scene counters timings &key (stream t))
  (let ((intersections (getf counters :intersections))
	(hits (getf counters :hits))
	(shadow-tests (getf counters :shadow-tests))
	(shadows (getf counters :shadows))
        (c-scene (scene-compiled-scene scene))
        (meshes (remove-if-not (lambda (obj) (typep obj 'mesh))
                               (scene-objects scene))))
    (format stream "~&Total objects: ~A, lights: ~A~%~
                    KD-tree depth: ~A, Unbounded: ~A, Meshes: ~A~@[ (avg depth: ~A)~]~%~
                    Camera rays: ~A~%~
                    Reflections:  ~A, Refractions:  ~A~%~
                    Intersection tests/hits: ~A / ~A~50T~@[~D%~]~%~
                    Shadow tests/hits:       ~A / ~A~50T~@[~D%~]~%~
                    Real: ~,2F, User: ~,2F System: ~,2F seconds~%~
                    GC: ~,2F seconds, ~,2F Mb consed"
	    (length (scene-objects scene))
	    (length (scene-lights scene))
            (let ((kd (compiled-scene-tree c-scene)))
              (if kd
                  (kd-depth kd)
                  0))
            (length (compiled-scene-objects c-scene))
            (length meshes)
            (when meshes
              (mean (mapcar (lambda (m) (kd-depth (mesh-kd-tree m))) meshes)))
	    (getf counters :camera)
	    (getf counters :reflected)
	    (getf counters :refracted)
	    intersections hits
	    (unless (zerop intersections)
	      (round (* 100 (/ hits intersections))))
	    shadow-tests shadows
	    (unless (zerop shadows)
	      (round (* 100 (/ shadows shadow-tests))))
            (real-time-seconds timings)
            (user-run-time-seconds timings)
            (system-run-time-seconds timings)
            (gc-run-time-seconds timings)
            (gc-mb-consed timings))))

(defun report (scene counters timings stream)
  (print-report scene (counter-plist counters) timings :stream stream))

(defun gc-run-time-seconds (timings)
  (/ (getf timings :gc-run-time-ms) 1000.0))
(defun gc-mb-consed (timings)
  (/ (getf timings :bytes-consed) (* 1024.0 1024.0)))
(defun system-run-time-seconds (timings)
  (/ (getf timings :system-run-time-us) 1000000.0))
(defun user-run-time-seconds (timings)
  (/ (getf timings :user-run-time-us) 1000000.0))
(defun real-time-seconds (timings)
  (/ (getf timings :real-time-ms) 1000.0))