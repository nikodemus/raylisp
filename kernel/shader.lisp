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

;;;; SHADERS
;;;;
;;;; Subclasses of SHADER implement different object shaders: during scene
;;;; compilation COMPUTE-SHADER-FUNCTION is called for shaders attached to
;;;; scene objects. The returned function is then called during rendering to
;;;; compute the visible color for points on object surface.
;;;;
;;;; BACKGROUND-SHADER
;;;;
;;;; Subclasses of BACKGROUND-SHADER implemen different background shaders:
;;;; during scene compilation COMPUTE-BACKGROUND-SHADER-FUNCTION is called for
;;;; the scene background. The returned function is then called during rendering
;;;; to compute apparent color of rays which do not hit any objects.

(defclass shader (name-mixin transformable)
  ())

(defclass background-shader ()
  ())

(defmacro shader-lambda (&whole form name (result point normal n.d ray counters)
                         &body body)
  (multiple-value-bind (forms declarations doc)
      (parse-body body :documentation t :whole form)
    `(sb-int:named-lambda ,name (,result ,point ,normal ,n.d ,ray ,counters)
       ,@(when doc (list doc))
       (declare (type color ,result)
                (type point ,point)
                (type vec ,normal)
                (type single-float ,n.d)
                (type ray ,ray)
                (type counter-vector ,counters)
                (optimize (sb-c::recognize-self-calls 0)
                          (sb-c::type-check 0)
                          (sb-c::verify-arg-count 0)))
       (the color
         (values
          (block ,name
            (locally
                (declare (optimize (sb-c::type-check 1) (sb-c::verify-arg-count 1)))
              (let ((,result ,result) (,point ,point)
                    (,normal ,normal) (,n.d ,n.d)
                    (,ray ,ray) (,counters ,counters))
                ,@declarations
                ,@forms))))))))

(defmacro background-shader-lambda (&whole form name lambda-list &body body)
  (destructuring-bind (color ray) lambda-list
    (multiple-value-bind (forms declarations doc)
        (parse-body body :documentation t :whole form)
      `(sb-int:named-lambda ,name ,lambda-list
         ,@(when doc (list doc))
         (declare (type color ,color)
                  (type ray ,ray)
                  (optimize (sb-c::recognize-self-calls 0)
                            (sb-c::type-check 0)
                            (sb-c::verify-arg-count 0)))
         (the color
           (values
            (block ,name
              (locally
                  (declare (optimize (sb-c::type-check 1) (sb-c::verify-arg-count 1)))
                (let ,(mapcar (lambda (arg) (list arg arg)) lambda-list)
                 ,@declarations
                 ,@forms)))))))))

(defun constant-shader-function (value)
  (check-type value vec)
  (shader-lambda constant-shader (result point normal n.d ray counters)
    (declare (ignore result point normal n.d ray counters) (optimize (safety 0)))
    value))

(defun constant-background-shader-function (value)
  (check-type value vec)
  (background-shader-lambda constant-background-shader (result ray)
    (declare (ignore result ray) (optimize (safety 0)))
    value))

(declaim (ftype (function (t t t matrix) shader-function)
                compute-shader-function))
(defgeneric compute-shader-function (shader object scene transform))

(defmethod compute-shader-function :around (shader object scene transform)
  ;; Since the rest of the system trusts the types of shaders functions,
  ;; make sure the function type looks about right.
  (check-function-type (call-next-method) 'shader-function))

(defmethod compute-shader-function :around ((shader transformable) object scene transform)
  ;; Apply the transform of the shader: otherwise all subclasses need to do
  ;; this.
  (call-next-method shader object scene (matrix* transform (transform-of shader))))

(defmethod compute-shader-function ((shader null) object scene transform)
  (constant-shader-function black))

(declaim (ftype (function (t t) background-shader-function)
                compute-background-shader-function))
(defgeneric compute-background-shader-function (shader scene))

(defmethod compute-background-shader-function :around (shader scene)
  (check-function-type (call-next-method) 'background-shader-function))

(defmethod compute-background-shader-function ((shader null) scene)
  (constant-background-shader-function black))

(defmethod compute-background-shader-function ((shader #.(class-of (alloc-vec))) scene)
  (constant-background-shader-function shader))

(declaim (ftype (function (t t t t) (values shader-function &optional))
                compile-shader))
(defun compile-shader (shader object scene transform)
  (declare (type scene-object object))
  (if shader
      (compute-shader-function shader object scene transform)
      (constantly black)))

(declaim (inline coefficient))
(defun coefficient (value shader)
  (declare (float value))
  (/ value (the float (shader-weight shader))))

(declaim (inline shade))
(defun shade (result object ray counters)
  ;; Make sure the types are right: shader functions do not check
  ;; their argument types! The compiler should be able to eliminate
  ;; actual type checks from there, but let's make sure we don't
  ;; accidentally trust stuff!
  (declare (type shading-object object)
           (type ray ray)
           (type counter-vector counters)
           (optimize safety))
  (let* ((point (adjust-vec (ray-origin ray) (ray-direction ray) (ray-extent ray)))
         (normal (funcall (object-normal object) point))
         (n.d (dot-product normal (ray-direction ray))))
    (declare (dynamic-extent point)
             (type vec point normal)
             (type single-float n.d))
    (flet ((%shade (n)
             (declare (type vec n)
                      (dynamic-extent n))
             (values (funcall (object-shader object)
                              result
                              point
                              n
                              n.d
                              ray
                              counters))))
      (if (plusp n.d)
          (%shade (vec* normal -1.0))
          (%shade normal)))))
