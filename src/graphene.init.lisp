;;; ----------------------------------------------------------------------------
;;; graphene.init.lisp
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library graphene
      ((:and :unix (:not :darwin))
       (:or "libgraphene-1.0.so.0" "libgraphene-1.0.so"))
       (:darwin "libgraphene-1.0.dylib")
       (:windows "libgraphene-1.0-0.dll")
       (t (:default "libgraphene-1.0")))

    (cffi:use-foreign-library graphene)
    ;; push the hostname on *features*
    (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
    (pushnew :graphene *features*))

;;; ----------------------------------------------------------------------------

(declaim (inline mklist))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defmacro with-object ((var type &rest args) &body body)
  (cond ((eq 'point-t type)
         `(with-point (,var ,@args) ,@body))
        ((eq 'point3d-t type)
         `(with-point3d (,var ,@args) ,@body))
        ((eq 'rect-t type)
         `(with-rect (,var ,@args) ,@body))
        ((eq 'size-t type)
         `(with-size (,var ,@args) ,@body))
        ((eq 'vec2-t type)
         `(with-vec2 (,var ,@args) ,@body))
        ((eq 'vec3-t type)
         `(with-vec3 (,var ,@args) ,@body))
        ((eq 'vec4-t type)
         `(with-vec4 (,var ,@args) ,@body))
        ((eq 'quad-t type)
         `(with-quad (,var ,@args) ,@body))
        ((eq 'quaternion-t type)
         `(with-quaternion (,var ,@args) ,@body))
        ((eq 'box-t type)
         `(with-box (,var ,@args) ,@body))
        ((eq 'ray-t type)
         `(with-ray (,var ,@args) ,@body))
        ((eq 'triangle-t type)
         `(with-triangle (,var ,@args) ,@body))
        ((eq 'plane-t type)
         `(with-plane (,var ,@args) ,@body))
        ((eq 'sphere-t type)
         `(with-sphere (,var ,@args) ,@body))
        ((eq 'euler-t type)
         `(with-euler (,var ,@args) ,@body))
        ((eq 'frustum-t type)
         `(with-frustum (,var ,@args) ,@body))
        ((eq 'matrix-t type)
         `(with-matrix (,var ,@args) ,@body))
        (t
         (error "Syntax error in GRAPHENE:WITH-object"))))

(export 'with-object)

(defmacro with-objects (vars &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-objects (obj1 ... objn) body) => result}
  @argument[obj1 ... objn]{the newly created Graphene instances}
  @argument[body]{a body that uses the bindings @arg{obj1 ... objn}}
  @begin{short}
    The @fun{graphene:with-objects} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each object can be initialized with values using the syntax for the
  @fun{graphene:with-object} macro. See also the @fun{graphene:with-object}
  documentation.
  @see-macro{graphene:with-object}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-object ,var
           (with-objects ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-objects)

;;; --- End of file graphene.init.lisp -----------------------------------------
