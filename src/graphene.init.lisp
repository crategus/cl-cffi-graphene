;;; ----------------------------------------------------------------------------
;;; graphene.init.lisp
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load the library
  (cffi:define-foreign-library graphene
      ((:and :unix (:not :darwin))
       (:or "libgraphene-1.0.so.0" "libgraphene-1.0.so"))
       (:darwin "libgraphene-1.0.dylib")
       (:windows "libgraphene-1.0-0.dll")
       (t (:default "libgraphene-1.0")))
  ;; Use the library
  (cffi:use-foreign-library graphene)
  ;; Push hostname on *features*
  (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
  ;; Push library name on *features*
  (pushnew :graphene *features*))

;;; ----------------------------------------------------------------------------

;; Short name for destructuring-bind
(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

;;; ----------------------------------------------------------------------------

(declaim (inline mklist))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;;; ----------------------------------------------------------------------------

(defmacro with-object ((var type &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-7}
  @syntax{(graphene:with-object (obj type &rest args) body) => result}
  @argument[obj]{a Graphene instance to create and initialize}
  @argument[type]{a symbol for a Graphene type}
  @argument[args]{arguments for initializing the Graphene instance}
  @begin{short}
    The @fun{graphene:with-object} macro allocates an new Graphene instance
    for the given Graphene type, initializes the Graphene instance with given
    values and executes the body that uses the Graphene instance.
  @end{short}
  After execution of the body the allocated memory for the Graphene instance
  is released.

  The @arg{type} argument can have one of the following values to
  create and initialize a corresponding Graphene instance:
  @begin[code]{table}
    @entry[graphene:point-t]{See the @fun{graphene:with-point} macro.}
    @entry[graphene:point3d-t]{See the @fun{graphene:with-point3d} macro.}
    @entry[graphene:size-t]{See the @fun{graphene:with-size} macro.}
    @entry[graphene:rect-t]{See the @fun{graphene:with-rect} macro.}
    @entry[graphene:quad-t]{See the @fun{graphene:with-quad} macro.}
    @entry[graphene:triangle-t]{See the @fun{graphene:with-triangle} macro.}
    @entry[graphene:box-t]{See the @fun{graphene:with-box} macro.}
    @entry[graphene:sphere-t]{See the @fun{graphene:with-sphere} macro.}
    @entry[graphene:frustum-t]{See the @fun{graphene:with-frustum} macro.}
    @entry[graphene:vec2-t]{See the @fun{graphene:with-vec2} macro.}
    @entry[graphene:vec3-t]{See the @fun{graphene:with-vec3} macro.}
    @entry[graphene:vec4-t]{See the @fun{graphene:with-vec4} macro.}
    @entry[graphene:matrix-t]{See the @fun{graphene:with-matrix} macro.}
    @entry[graphene:euler-t]{See the @fun{graphene:with-euler} macro.}
    @entry[graphene:quaternion-t]{See the @fun{graphene:with-quaternion} macro.}
    @entry[graphene:plane-t]{See the @fun{graphene:with-plane} macro.}
    @entry[graphene:ray-t]{See the @fun{graphene:with-ray} macro.}
  @end{table}
  @begin[Examples]{dictionary}
    Initalize a @symbol{graphene:point-t} instance with default values:
    @begin{pre}
* (macroexpand '(graphene:with-object (p graphene:point-t) p))
(LET ((P (GRAPHENE:POINT-ALLOC)))
  (GRAPHENE:POINT-INIT P 0.0 0.0)
  (UNWIND-PROTECT (PROGN P) (GRAPHENE:POINT-FREE P)))
T
    @end{pre}
    Initialize a @symbol{graphene:with-point} instance with two values:
    @begin{pre}
* (macroexpand '(graphene:with-object (p graphene:point-t 1 2) p))
(LET ((P (GRAPHENE:POINT-ALLOC)))
  (GRAPHENE:POINT-INIT P 1 2)
  (UNWIND-PROTECT (PROGN P) (GRAPHENE:POINT-FREE P)))
T
    @end{pre}
  @end{dictionary}
  @see-macro{graphene:with-objects}"
  (cond ((eq 'point-t type)
         `(with-point (,var ,@args) ,@body))
        ((eq 'point3d-t type)
         `(with-point3d (,var ,@args) ,@body))
        ((eq 'size-t type)
         `(with-size (,var ,@args) ,@body))
        ((eq 'rect-t type)
         `(with-rect (,var ,@args) ,@body))
        ((eq 'quad-t type)
         `(with-quad (,var ,@args) ,@body))
        ((eq 'triangle-t type)
         `(with-triangle (,var ,@args) ,@body))
        ((eq 'box-t type)
         `(with-box (,var ,@args) ,@body))
        ((eq 'sphere-t type)
         `(with-sphere (,var ,@args) ,@body))
        ((eq 'frustum-t type)
         `(with-frustum (,var ,@args) ,@body))
        ((eq 'vec2-t type)
         `(with-vec2 (,var ,@args) ,@body))
        ((eq 'vec3-t type)
         `(with-vec3 (,var ,@args) ,@body))
        ((eq 'vec4-t type)
         `(with-vec4 (,var ,@args) ,@body))
        ((eq 'matrix-t type)
         `(with-matrix (,var ,@args) ,@body))
        ((eq 'euler-t type)
         `(with-euler (,var ,@args) ,@body))
        ((eq 'quaternion-t type)
         `(with-quaternion (,var ,@args) ,@body))
        ((eq 'plane-t type)
         `(with-plane (,var ,@args) ,@body))
        ((eq 'ray-t type)
         `(with-ray (,var ,@args) ,@body))
        (t
         (error "Syntax error in GRAPHENE:WITH-object"))))

(export 'with-object)

(defmacro with-objects (vars &body body)
 #+liber-documentation
 "@version{2025-4-7}
  @syntax{(graphene:with-objects (obj1 ... objn) body) => result}
  @argument[obj1 ... objn]{newly created Graphene instances}
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
