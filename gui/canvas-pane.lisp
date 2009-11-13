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

(defpackage :canvas-pane
  (:use :clim-lisp :clim)
  (:export
   #:canvas-pane
   #:canvas-rgba
   #:canvas-selection
   #:canvas-dirty-region))

(in-package :canvas-pane)

;;;; UTILITIES
;;;;
;;;; Looking up a common ancestor of two sheets, and transforming points
;;;; via a common ancestor.

(defun find-common-ancestor (sheet1 sheet2)
  (if (sheet-ancestor-p sheet1 sheet2)
      sheet2
      (find-common-ancestor sheet1 (sheet-parent sheet2))))

(defun map-sheet-position-to-ancestor (sheet ancestor x y)
  (if (eq sheet ancestor)
      (values x y)
      (multiple-value-bind (xt yt) (map-sheet-position-to-parent sheet x y)
        (map-sheet-position-to-ancestor (sheet-parent sheet) ancestor xt yt))))

(defun map-sheet-position-from-ancestor (sheet ancestor x y)
  (if (eq sheet ancestor)
      (values x y)
      (multiple-value-bind (xt yt)
          (map-sheet-position-from-ancestor (sheet-parent sheet) ancestor x y)
        (map-sheet-position-to-child sheet xt yt))))

(defun map-sheet-position-via-ancestor (source target source-x source-y)
  (if (eq source target)
      (values source-x source-y)
      (let ((ancestor (find-common-ancestor source target)))
        (multiple-value-bind (xa ya)
            (map-sheet-position-to-ancestor source ancestor source-x source-y)
          (map-sheet-position-from-ancestor target ancestor xa ya)))))

;;; RASTER

(deftype raster ()
  `(simple-array (unsigned-byte 32) (* *)))

(defun make-raster (width height)
  (make-array (list height width) ))

;;;; CANVAS-PANE

(defclass canvas-pane (application-pane)
  ((raster
    :initform nil
    :accessor canvas-raster)
   (dirty-region
    :initform nil)
   (pixmap
    :initform nil
    :accessor canvas-pixmap)
   (selection
    :initform nil
    :accessor canvas-selection)))

(defun raster-width (raster)
  (array-dimension raster 1))

(defun raster-height (raster)
  (array-dimension raster 0))

(defmethod canvas-raster :around ((canvas canvas-pane))
  (let ((raster (call-next-method))
        (w (bounding-rectangle-width canvas))
        (h (bounding-rectangle-height canvas)))
    (unless (and raster (= (raster-width raster) w) (= (raster-height raster) h))
      ;; FIXME: Copy the old raster, scaling contents.
      (setf raster (make-array (list h w) :element-type '(unsigned-byte 32))
            (canvas-raster canvas) raster))
    raster))

(defmethod canvas-dirty-region ((canvas canvas-pane))
  (let ((dirty (slot-value canvas 'dirty-region)))
    (if dirty
        (destructuring-bind ((minx . miny) . (maxx . maxy)) dirty
          (make-bounding-rectangle minx miny maxx maxy))
        +nowhere+)))

(defmethod canvas-pixmap :around ((canvas canvas-pane))
  (let ((pixmap (call-next-method))
        (w (bounding-rectangle-width canvas))
        (h (bounding-rectangle-height canvas)))
    (unless (and pixmap (= (pixmap-width pixmap) w) (= (pixmap-height pixmap) h))
      ;; FIXME: Copy the old pixmap, scaling contents.
      (when pixmap
        (deallocate-pixmap pixmap))
      (setf pixmap (allocate-pixmap canvas w h)
            (canvas-pixmap canvas) pixmap))
    pixmap))

(defun canonicalize-selection (selection bounds)
  (when selection
    (let* ((width (bounding-rectangle-width bounds))
           (height (bounding-rectangle-height bounds)))
      (destructuring-bind ((minx . miny) . (maxx . maxy)) selection
        (cons (cons (max 0 (min minx maxx))
                    (max 0 (min miny maxy)))
              (cons (min (- width 1) (max minx maxx))
                    (min (- height 1) (max miny maxy))))))))

(defmethod (setf canvas-selection) :around (selection canvas)
  (call-next-method (canonicalize-selection selection canvas) canvas))

;;;; EVENT HANDLING

(defmethod dispatch-event ((canvas canvas-pane) (event pointer-button-press-event))
  (handle-event canvas event))

(defmethod handle-event ((canvas canvas-pane) (start-event pointer-button-press-event))
  (let ((start (cons (pointer-event-x start-event)
                     (pointer-event-y start-event))))
    (setf (canvas-selection canvas) (cons start start))
    (tracking-pointer (canvas)
      (:pointer-motion (&key x y window)
        (multiple-value-bind (x y) (map-sheet-position-via-ancestor window canvas x y)
          (setf (canvas-selection canvas) (cons start (cons x y))))
        (repaint-sheet canvas +everywhere+))
      (:pointer-button-release (&key x y window)
        (multiple-value-bind (x y) (map-sheet-position-via-ancestor window canvas x y)
          (setf (canvas-selection canvas) (cons start (cons x y))))
        (repaint-sheet canvas +everywhere+)
        (return-from handle-event)))))

;;;; READING AND WRITING COLORS ON CANVAS

(defun (setf canvas-rgba) (rgba canvas x y)
  ;; Update dirty-region.
  (let ((dirty-region (slot-value canvas 'dirty-region)))
    (setf (slot-value canvas 'dirty-region)
          (if dirty-region
              (destructuring-bind ((minx . miny) . (maxx . maxy)) dirty-region
                (cons (cons (min x minx)
                            (min y miny))
                      (cons (max x maxx)
                            (max y maxy))))
              (cons (cons x y) (cons x y)))))
  ;; Set color of the point.
  (setf (aref (canvas-raster canvas) y x) rgba))

(defun canvas-rgba (canvas x y)
  (aref (canvas-raster canvas) y x))

;;;; DRAWING

(defun handle-raster-repaint (canvas region)
  (let ((dirty (slot-value canvas 'dirty-region))
        (pixmap (slot-value canvas 'pixmap)))
    ;; 1. Paint dirty area -- which might be outside the requested region,
    ;; but what the hell.
    (when dirty
      (destructuring-bind ((minx . miny) . (maxx . maxy)) dirty
        (declare (fixnum minx miny maxx maxy))
        (let ((width (1+ (- maxx minx)))
              (height (1+ (- maxy miny)))
              (pixmap (canvas-pixmap canvas))
              (raster (canvas-raster canvas)))
          ;; Send the dirty area to X.
          (with-sheet-medium (medium canvas)
            (medium-draw-pixels* medium raster minx miny
                                 :width width :height height
                                 :src-x minx :src-y miny))
          ;; Update the pixmap from the dirty area.
          (copy-to-pixmap canvas minx miny width height
                          pixmap minx miny)))
      (let ((dirty-region (canvas-dirty-region canvas)))
        ;; No longer dirty.
        (setf (slot-value canvas 'dirty-region) nil)
        (when (region-contains-region-p dirty-region region)
          ;; If we repainted the entire requested area, then there is nothing
          ;; more to do.
          (return-from handle-raster-repaint))))
    ;; 2. Repaint clean areas by copying from pixmap.
    (when pixmap
      (if (bounding-rectangle-p region)
          (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
            (copy-from-pixmap pixmap x1 y1 (- x2 x1) (- y2 y1)
                              canvas x1 y1))
          ;; Everything, then.
          (copy-from-pixmap pixmap 0 0 (pixmap-width pixmap) (pixmap-height pixmap)
                            canvas 0 0)))))

(defmethod handle-repaint :after ((canvas canvas-pane) region)
  (unless (eq region +nowhere+)
    ;; Raster
    (handle-raster-repaint canvas region)
    ;; Selection box.
    (let ((selection (canvas-selection canvas)))
      (when selection
        (destructuring-bind ((minx . miny) . (maxx . maxy)) selection
          (with-output-recording-options (canvas :record nil :draw t)
            (draw-rectangle* canvas
                             ;; KLUDGE/FIXME: The selection area doesn't
                             ;; quite do the right thing right now...
                             (max 0 (1- minx)) (max 0 (1- miny))
                             maxx maxy
                             :clipping-region region
                             :filled nil
                             :ink +red+
                             :line-thickness 1)))))))




