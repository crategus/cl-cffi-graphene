;;; ----------------------------------------------------------------------------
;;; graphene.size.lisp
;;;
;;; The documentation in this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library, see
;;; <https://ebassi.github.io/graphene/docs/>. The API documentation for the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; Size
;;;
;;;     Size representation
;;;
;;; Types and Values
;;;
;;;     graphene_size_t
;;;
;;; Functions
;;;
;;;     graphene_size_alloc
;;;     graphene_size_free
;;;     graphene_size_zero
;;;     graphene_size_init
;;;     graphene_size_init_from_size
;;;     graphene_size_equal
;;;     graphene_size_interpolate
;;;     graphene_size_scale
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-size ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-3}
  @syntax{(graphene:with-size (s) body) => result}
  @syntax{(graphene:with-size (s s1) body) => result}
  @syntax{(graphene:with-size (s width height) body) => result}
  @argument[s]{a @symbol{graphene:size-t} instance to create and initialize}
  @argument[s1]{a @symbol{graphene:size-t} instance to use for initialization}
  @argument[width]{a number coerced to a single float for the width component}
  @argument[height]{a number coerced to a single float for the height component}
  @begin{short}
    The @fun{graphene:with-size} macro allocates a new
    @symbol{graphene:size-t} instance, initializes the @symbol{graphene:size-t}
    instance with the given values and executes the body that uses the size.
  @end{short}
  After execution of the body the allocated memory for the size is released.

  When no argument is given the components of the @symbol{graphene:size-t}
  instance are initialized to zero. The initialization with two single floats
  uses the @fun{graphene:size-init} function. The initialization from another
  size is done with the @fun{graphene:size-init-from-size} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:size-alloc} function and
    released with the @fun{graphene:size-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a @symbol{graphene:size-t} instance with no value and two float
    values.
    @begin{pre}
(graphene:with-size (s)
  (list (graphene:size-width s) (graphene:size-height s)))
=> (0.0 0.0)
(graphene:with-size (s 1.5 1.7)
  (list (graphene:size-width s) (graphene:size-height s)))
=> (1.5 1.7)
    @end{pre}
    This examples uses the @fun{graphene:with-sizes} macro to
    initialize two @symbol{graphene:size-t} instances. The second instance is
    intialized with the values from the first instance.
    @begin{pre}
(graphene:with-sizes ((s1 0.3 0.5) (s2 s1))
  (list (graphene:size-width s2) (graphene:size-height s2)))
=> (0.3 0.5)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:size-t}
  @see-macro{graphene:with-sizes}
  @see-function{graphene:size-alloc}
  @see-function{graphene:size-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (size-alloc)))
            (size-init ,var 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (size-free ,var))))
        ((null (second args))
         ;; One argument of type size-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'size-t)
                  ;; One argument of type size-t
                  `(let ((,var (size-alloc)))
                     (size-init-from-size ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (size-free ,var))))
                 (t
                  ;; One argument with no type, default is size-t
                  `(let ((,var (size-alloc)))
                     (size-init-from-size ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (size-free ,var)))))))
        ((null (third args))
         ;; Two arguments for width and height values
         `(let ((,var (size-alloc)))
            (size-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (size-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-SIZE"))))

(export 'with-size)

(defmacro with-sizes (vars &body body)
 #+liber-documentation
 "@version{2025-4-3}
  @syntax{(graphene:with-sizes (s1 s2 s3 ... sn) body) => result}
  @argument[s1 ... sn]{newly created @symbol{graphene:size-t} instances}
  @argument[body]{a body that uses the bindings @arg{s1 ... sn}}
  @begin{short}
    The @fun{graphene:with-sizes} macro creates new variable bindings
    and executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each size can be initialized with values using the syntax for the
  @fun{graphene:with-size} macro. See also the
  @fun{graphene:with-size} documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-sizes (s1 (s2 1.2 1.3) (s3 s2))
  (list (list (graphene:size-width s1) (graphene:size-height s1))
        (list (graphene:size-width s2) (graphene:size-height s2))
        (list (graphene:size-width s3) (graphene:size-height s3))))
=> ((0.0 0.0) (1.2 1.3) (1.2 1.3))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:size-t}
  @see-macro{graphene:with-size}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-size ,var
           (with-sizes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-sizes)

;;; ----------------------------------------------------------------------------
;;; graphene_size_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct size-t
  (width :float)
  (height :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'size-t)
      "CStruct"
      (liber:symbol-documentation 'size-t)
 "@version{2025-4-3}
  @begin{declaration}
(cffi:defcstruct size-t
  (width :float)
  (height :float))
  @end{declaration}
  @begin{short}
    The @symbol{graphene:size-t} structure is a data structure capable of
    describing a size with two components width and height of type float.
  @end{short}

  Access the coordinates with the @fun{graphene:size-width} and
  @fun{graphene:size-height} functions. Use the @fun{graphene:with-size}
  and @fun{graphene:with-sizes} macros to allocate a new
  @symbol{graphene:size-t} instance and initialize the size with values.
  @see-slot{graphene:size-width}
  @see-slot{graphene:size-height}
  @see-symbol{graphene:size-t}
  @see-function{graphene:size-width}
  @see-function{graphene:size-height}
  @see-function{graphene:with-size}
  @see-function{graphene:with-sizes}")

(export 'size-t)

;;; --- graphene:size-width ----------------------------------------------------

(defun (setf size-width) (value size)
  (setf (cffi:foreign-slot-value size '(:struct size-t) 'width)
        (coerce value 'single-float)))

(defun size-width (size)
  (cffi:foreign-slot-value size '(:struct size-t) 'width))

#+liber-documentation
(setf (liber:alias-for-function 'size-width)
      "Accessor"
      (documentation 'size-width 'function)
 "@version{2025-4-3}
  @syntax{(graphene:size-width size) => width}
  @syntax{(setf (graphene:size-width size) width)}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @argument[width]{a number coerced to a single float for the width component}
  @begin{short}
    Accessor of the @code{width} slot of the @symbol{graphene:size-t} structure.
  @end{short}
  @see-symbol{graphene:size-t}")

(export 'size-width)

;;; --- graphene:size-heigth ---------------------------------------------------

(defun (setf size-height) (value size)
  (setf (cffi:foreign-slot-value size '(:struct size-t) 'height)
        (coerce value 'single-float)))

(defun size-height (size)
  (cffi:foreign-slot-value size '(:struct size-t) 'height))

#+liber-documentation
(setf (liber:alias-for-function 'size-height)
      "Accessor"
      (documentation 'size-height 'function)
 "@version{2025-4-3}
  @syntax{(graphene:size-height size) => height}
  @syntax{(setf (graphene:size-height size) height)}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @argument[width]{a number coerced to a single float for the height component}
  @begin{short}
    Accessor of the @code{height} slot of the @symbol{graphene:size-t}
    structure.
  @end{short}
  @see-symbol{graphene:size-t}")

(export 'size-height)

;;; ----------------------------------------------------------------------------
;;; graphene_size_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_size_alloc" size-alloc) (:pointer (:struct size-t))
 #+liber-documentation
 "@version{2025-4-3}
  @return{The newly allocated @symbol{graphene:size-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:size-t} instance.
  @end{short}
  The components of the returned @symbol{graphene:size-t} instance are
  initialized to zero. Use the @fun{graphene:size-free} function to free the
  resources allocated by this function.
  @see-symbol{graphene:size-t}
  @see-function{graphene:size-free}")

(export 'size-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_size_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_size_free" size-free) :void
 #+liber-documentation
 "@version{2025-4-3}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:size-alloc} function.
  @end{short}
  @see-symbol{graphene:size-t}
  @see-function{graphene:size-alloc}"
  (size (:pointer (:struct size-t))))

(export 'size-free)

;;; ----------------------------------------------------------------------------
;;; graphene_size_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_size_zero" size-zero) (:pointer (:struct size-t))
 #+liber-documentation
 "@version{2025-4-3}
  @return{The @symbol{graphene:size-t} instance with a zero size.}
  @begin{short}
    Returns a @symbol{graphene:size-t} instance with the two components set to
    zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (graphene:size-width (graphene:size-zero))
        (graphene:size-height (graphene:size-zero)))
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:size-t}")

(export 'size-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_size_init
;;; ----------------------------------------------------------------------------

(defun size-init (size width height)
 "@version{2025-4-3}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @argument[width]{a number coerced to a single float for the width component}
  @argument[height]{a number coerced to a single float for the height
    component}
  @return{The initialized @symbol{graphene:size-t} instance.}
  @begin{short}
    Initializes the @symbol{graphene:size-t} instance to the given @arg{width}
    and @arg{height} values.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{graphene:size-t}"
  (cffi:foreign-funcall "graphene_size_init"
                        (:pointer (:struct size-t)) size
                        :float (coerce width 'single-float)
                        :float (coerce height 'single-float)
                        (:pointer (:struct size-t))))

(export 'size-init)

;;; ----------------------------------------------------------------------------
;;; graphene_size_init_from_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_size_init_from_size" size-init-from-size)
    (:pointer (:struct size-t))
 #+liber-documentation
 "@version{2025-4-3}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @argument[source]{a @symbol{graphene:size-t} instance to use}
  @return{The initialized @symbol{graphene:size-t} instance.}
  @begin{short}
    Initializes the @symbol{graphene:size-t} instance using the values of
    @arg{source}.
  @end{short}
  @see-symbol{graphene:size-t}"
  (size (:pointer (:struct size-t)))
  (source (:pointer (:struct size-t))))

(export 'size-init-from-size)

;;; ----------------------------------------------------------------------------
;;; graphene_size_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_size_equal" size-equal) :bool
 #+liber-documentation
 "@version{2025-4-3}
  @argument[a]{a @symbol{graphene:size-t} instance}
  @argument[b]{a @symbol{graphene:size-t} instance}
  @begin{return}
    @em{True}, if the @symbol{graphene:size-t} instances have the same
    components, otherwise @em{false}.
  @end{return}
  @begin{short}
    Checks whether two given @symbol{graphene:size-t} instances are equal.
  @end{short}
  @see-symbol{graphene:size-t}"
  (a (:pointer (:struct size-t)))
  (b (:pointer (:struct size-t))))

(export 'size-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_size_interpolate
;;; ----------------------------------------------------------------------------

(defun size-interpolate (a b factor result)
 #+liber-documentation
 "@version{2025-4-3}
  @argument[a]{a @symbol{graphene:size-t} instance}
  @argument[b]{a @symbol{graphene:size-t} instance}
  @argument[factor]{a number coerced to a double float for the linear
    interpolation factor}
  @argument[result]{a @symbol{graphene:size-t} instance for the interpolated
    size}
  @return{The @symbol{graphene:size-t} instance with the interpolated size.}
  @begin{short}
    Linearly interpolates the components of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @see-symbol{graphene:size-t}"
  (cffi:foreign-funcall "graphene_size_interpolate"
                        (:pointer (:struct size-t)) a
                        (:pointer (:struct size-t)) b
                        :double (coerce factor 'double-float)
                        (:pointer (:struct size-t)) result
                        :void)
  result)

(export 'size-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_size_scale
;;; ----------------------------------------------------------------------------

(defun size-scale (size factor result)
 #+liber-documentation
 "@version{2025-4-3}
  @argument[size]{a @symbol{graphene:size-t} instance}
  @argument[factor]{a number coerced to a single float for the scaling factor}
  @argument[result]{a @symbol{graphene:size-t} instance for the scaled size}
  @return{The @symbol{graphene:size-t} instance with the scaled size.}
  @begin{short}
    Scales the components of the given @symbol{graphene:size-t} instance by the
    given factor.
  @end{short}
  @see-symbol{graphene:size-t}"
  (cffi:foreign-funcall "graphene_size_scale"
                        (:pointer (:struct size-t)) size
                        :float (coerce factor 'single-float)
                        (:pointer (:struct size-t)) result
                        :void)
  result)

(export 'size-scale)

;;; --- End of file graphene.size.lisp -----------------------------------------
