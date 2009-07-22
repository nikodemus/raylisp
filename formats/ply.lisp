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

(defpackage :raylisp-ply-loader
  (:use :cl :alexandria)
  (:import-from :raylisp
                #:build-mesh
                #:define-mesh-loader))

(in-package :raylisp-ply-loader)

(define-mesh-loader :ply load-ply)

(defun load-ply (pathname transform)
  (let (ply)
    (unwind-protect
         (progn
           (setf ply (open-ply pathname))
           (let ((vertices (get-element :vertex '(:x :y :z) ply))
                 (faces (get-element :face '(:vertex-indices) ply)))
             (build-mesh vertices faces :transform transform)))
      (close-ply ply))))

(defclass ply ()
  ((pathname
    :initarg :pathname
    :initform (required-argument :pathname)
    :reader ply-pathname)
   (stream
    :initarg :stream
    :initform (required-argument :stream)
    :accessor ply-stream)
   (format
    :initform nil
    :accessor ply-format)
   (format-version
    :initform nil
    :accessor ply-format-version)
   (elements
    :initform nil
    :accessor ply-elements)
   (comments
    :initform nil
    :accessor ply-comments)
   (object-info
    :initform nil
    :accessor ply-object-info)
   (data-offset
    :initform nil
    :accessor ply-data-offset)))

(defclass element ()
  ((name
    :initarg :name
    :initform (required-argument :name)
    :reader element-name)
   (position
    :initarg :position
    :initform (required-argument :position)
    :reader element-position)
   (count
    :initarg :count
    :initform (required-argument :count)
    :reader element-count)
   (properties
    :initform nil
    :accessor element-properties)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type t)
    (prin1 (element-name element) stream)))

(defclass property ()
  ((name
    :initarg :name
    :initform (required-argument :name)
    :reader property-name)
   (type
    :initarg :type
    :initform (required-argument :type)
    :reader property-type)))

(defmethod print-object ((property property) stream)
  (print-unreadable-object (property stream :type t)
    (format stream "~S (~(~A~))" (property-name property) (property-type property))))

(defclass list-property (property)
  ((external-type
    :initarg :external-type
    :initform (required-argument :external-type)
    :reader property-external-type)))

(defun open-ply (pathname)
  (let ((pathname (merge-pathnames pathname (make-pathname :type "ply")))
        (ply nil)
        (stream nil)
        (abort t))
    (unwind-protect
         (progn
           (setf stream (open pathname
                              :external-format :latin-1
                              :element-type :default)
                 ply (make-instance 'ply
                                    :pathname pathname
                                    :stream stream))
           (sb-ext:finalize ply (lambda () (ignore-errors (close stream))))
           (unless (equal '("ply") (read-words stream))
             (error "Not a PLY file: ~A" pathname))
           (read-header stream ply)
           (setf abort nil))
      (when (and abort stream)
        (close stream :abort t)))
    ply))

(defun close-ply (ply &key abort)
  (let ((stream (ply-stream ply)))
    (setf (ply-stream ply) nil)
    (close stream :abort abort)
    ply))

(defun get-element (name properties ply)
  (let* ((element (or (find name (ply-elements ply) :key #'element-name)
                      (error "No element named ~S in ~S" name ply)))
         (properties (mapcar (lambda (p)
                               (or (find p (element-properties element) :key #'property-name)
                                   (error "No property named ~S in ~S" p element)))
                             properties)))
    (ecase (ply-format ply)
      ;; FIXME: Binary formats not implemented.
      (:ascii
       (ascii-get-element ply element properties)))))

(defun ascii-get-element (ply element properties)
  (let* ((stream (ply-stream ply))
         (position (element-position element))
         (all-properties (element-properties element))
         (pcount (length properties))
         (count (element-count element))
         (element (make-array count))
         (allocator (make-component-allocator properties)))
    (file-position stream (ply-data-offset ply))
    ;; Scan to the start of element
    (loop repeat position
          do (read-line stream))
    (dotimes (i count)
      (let ((words (read-words stream)))
        (let ((comp (funcall allocator pcount)))
          (dolist (p all-properties)
            (assert words)
            (let ((j (position p properties)))
              (multiple-value-bind (values rest) (parse-property p words)
                (setf words rest)
                (when j
                  (setf comp (save-property j values comp))
                  (incf j)))))
          (setf (aref element i) comp))))
    element))

(defun make-component-allocator (properties)
  (let ((n (length properties)))
    (if (= 1 n)
        (constantly nil)
        (let ((first-type (property-type (car properties))))
          (if (every (lambda (p) (eq first-type (property-type p)))
                     (cdr properties))
              (ply-type-allocator first-type)
              (progn
                (break "generic ~S" properties)
                (lambda () (make-array n))))))))

(defun save-property (index values component)
  (cond ((not component)
         values)
        (t
         (setf (aref component index) values)
         component)))

(defmethod parse-property ((property property) words)
  (values (read-from-string (car words)) (cdr words)))

(defmethod parse-property ((property list-property) words)
  (let* ((n (read-from-string (pop words)))
         (store (funcall (ply-type-allocator (property-type property)) n))
         (i -1))
    (loop repeat n
          do (setf (aref store (incf i)) (read-from-string (pop words))))
    (values store words)))

(defun read-header (stream ply)
  (let ((last-elt nil)
        (position 0))
    (loop
      (destructuring-bind (op &rest args) (read-words stream)
        (switch (op :test equal)
          ("format"
           (destructuring-bind (format format-version) args
             (setf (ply-format ply)
                   (switch (format :test equal)
                     ("ascii" :ascii)
                     ("binary_little_endian" :little-endian)
                     ("binary_big_endian" :big-endian)
                     (t (error "Unknown format specified in header: ~A" format))))
             (let* ((*read-default-float-format* 'single-float)
                    (version-number (read-from-string format-version)))
               (check-type version-number single-float)
               (setf (ply-format-version ply) version-number))))
          ("element"
           (when last-elt
             (setf (element-properties last-elt) (nreverse (element-properties last-elt))))
           (destructuring-bind (name number) args
             (let ((count (parse-integer number)))
               (push (setf last-elt (make-instance 'element
                                                   :name (intern-name name)
                                                   :position position
                                                   :count count))
                     (ply-elements ply))
               (incf position count))))
          ("property"
           (switch ((car args) :test equal)
             ("list"
              (destructuring-bind (count-name type-name property-name) (cdr args)
                (push (make-instance 'list-property
                                     :name (intern-name property-name)
                                     :external-type (find-type count-name)
                                     :type (find-type type-name))
                      (element-properties last-elt))))
             (t
              (destructuring-bind (type-name property-name) args
                (push (make-instance 'property
                                     :name (intern-name property-name)
                                     :type (find-type type-name))
                      (element-properties last-elt))))))
          ("comment"
           (push (format nil "~{~A~^ ~}" args) (ply-comments ply)))
          ("obj_info"
           (push (format nil "~{~A~^ ~}" args) (ply-object-info ply)))
          ("end_header"
           (setf (ply-elements ply) (nreverse (ply-elements ply)))
           (setf (ply-data-offset ply) (file-position stream))
           (return)))))))

(defun read-words (stream)
  (let* ((line (read-line stream))
         (size (length line))
         (words nil))
    (flet ((space-start (p)
             (when p
               (loop for pos from p below size
                     for char = (aref line pos)
                     when (or (char= #\space char) (char= #\tab char))
                     return pos)))
           (space-end (p)
             (when p
               (loop for pos from p below size
                     for char = (aref line pos)
                     unless (or (char= #\space char) (char= #\tab char))
                     return pos))))
      (do* ((word-start 0 (space-end word-end))
            (word-end (space-start 0) (space-start word-start)))
           ((not word-start) (nreverse words))
        (push (subseq line word-start word-end) words)))))

(defclass ply-type ()
  ((name
    :initarg :name
    :initform (required-argument :name)
    :reader ply-type-name)
   (size
    :initarg :size
    :initform (required-argument :type)
    :reader ply-type-size)
   (allocator
    :initarg :allocator
    :initform (required-argument :allocator)
    :reader ply-type-allocator)))

(defparameter *ply-types*
  (mapcar (lambda (info)
            (destructuring-bind (name size allocator) info
                (make-instance 'ply-type
                               :name name
                               :size size
                               :allocator allocator)))
          (macrolet ((alloc (type)
                       `(lambda (n)
                          (declare (fixnum n))
                          (make-array n :element-type ',type))))
            (list (list "char"   1 (alloc (signed-byte 8)))
                  (list "short"  2 (alloc (signed-byte 16)))
                  (list "int"    4 (alloc (signed-byte 32)))
                  (list "uchar"  1 (alloc (unsigned-byte 8)))
                  (list "ushort" 2 (alloc (unsigned-byte 16)))
                  (list "uint"   4 (alloc (unsigned-byte 32)))
                  (list "float"  4 (alloc single-float))
                  (list "double" 8 (alloc double-float))))))

(defun find-type (type)
  (or (find type *ply-types* :test #'equal :key #'ply-type-name)
      (error "Unknown PLY type: ~A" type)))

(defun intern-name (type)
  (intern (substitute #\- #\_ (string-upcase type)) :keyword))


