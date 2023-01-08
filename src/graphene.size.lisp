;;; ----------------------------------------------------------------------------
;;; graphene.size.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library.
;;; See <https://ebassi.github.io/graphene/docs/>.
;;; The API documentation of the Lisp binding is available from
;;; <http://www.crategus.com/books/cl-cffi-graphene/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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

(defmacro with-graphene-size ((var &rest args) &body body)
 #+liber-documentation
 "@version{#2022-10-1}
  @syntax[]{with-graphene-size (s) body => result}
  @syntax[]{with-graphene-size (s width height) body => result}
  @syntax[]{with-graphene-size (s s1) body => result}
  @argument[s]{a @symbol{size-t} instance to create and initialize}
  @argument[width]{a number coerced to a single float for the width component}
  @argument[height]{a number coerced to a single float for the height component}
  @argument[s1]{a @symbol{size-t} instance to use for initialization}
  @begin{short}
    The @sym{with-gaphene-size} macro allocates a new @symbol{size-t} instance,
    initializes the @symbol{size-t} instance with the given values and executes
    the body that uses the size.
  @end{short}
  After execution of the body the allocated memory for the size is released.

  When no argument is given the components of the @sym{sizte-t} instance are
  initialized to zero. The initialization with two single float values uses the
  @fun{size-init} function. The initialization from another size is done with
  the @fun{size-init-from-size} function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{size-alloc} function and released
    with the @fun{size-free} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Initialize a @symbol{size-t} instance with no value and two single float
    values.
    @begin{pre}
(with-graphene-size (s)
  (list (size-width s) (size-height s)))
=> (0.0 0.0)
(with-graphene-size (s 1.5 1.7)
  (list (size-width s) (size-height s)))
=> (1.5 1.7)
    @end{pre}
    This examples uses the @fun{with-graphene-sizes} macro to initialize
    two @symbol{size-t} instances. The second instance is intialized with the
    values from the first instance.
    @begin{pre}
(with-graphene-sizes ((s1 0.3 0.5) (s2 s1))
  (list (size-width s2) (size-height s2)))
=> (0.3 0.5)
    @end{pre}
  @end{dictionary}
  @see-symbol{size-t}
  @see-macro{with-graphene-sizes}
  @see-function{size-alloc}
  @see-function{size-free}"
  (cond ((not args)
         ;; We have no arguments, the default is initialization with zeros.
         `(let ((,var (size-alloc)))
            (size-init ,var 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (size-free ,var))))
        ((not (second args))
         ;; We have one argument. The argument must be of type size-t.
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'size-t))
                  ;; One argument with no type or of type size-t
                  `(let ((,var (size-alloc)))
                     (size-init-from-size ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (size-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-SIZE")))))
        ((not (third args))
         ;; We have a list of two arguments with (width,height) values
         `(let ((,var (size-alloc)))
            (size-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (size-free ,var))))
        (t
         (error "Syntax error in WITH-GRAPHENE-SIZE"))))

(export 'with-graphene-size)

(defmacro with-graphene-sizes (vars &body body)
 #+liber-documentation
 "@version{#2022-10-1}
  @syntax[]{with-graphene-sizes (s1 s2 s3 ... sn) body => result}
  @argument[s1 ... sn]{the newly created @symbol{size-t} instances}
  @argument[body]{a body that uses the bindings @arg{s1} ... @arg{sn}}
  @begin{short}
    The @sym{with-graphene-sizes} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{with-graphene-size} macro. See also the @fun{within-graphene-size}
  documentation.
  @begin[Examples]{dictionary}
    @begin{pre}
(with-graphene-sizes (s1 (s2 1.2 1.3) (s3 s2))
  (list (list (size-width s1) (size-height s1))
        (list (size-width s2) (size-height s2))
        (list (size-width s3) (size-height s3))))
=> ((0.0 0.0) (1.2 1.3) (1.2 1.3))
    @end{pre}
  @end{dictionary}
  @see-symbol{size-t}
  @see-macro{with-graphene-size}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-size ,var
           (with-graphene-sizes ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-sizes)

;;; ----------------------------------------------------------------------------
;;; graphene_size_t
;;; ----------------------------------------------------------------------------

(defcstruct size-t
  (width :float)
  (height :float))

#+liber-documentation
(setf (liber:alias-for-symbol 'size-t)
      "CStruct"
      (liber:symbol-documentation 'size-t)
 "@version{#2022-10-1}
  @begin{short}
    The @sym{size-t} structure is a data structure capable of describing a
    size with two components width and height of type single float.
  @end{short}
  @begin{pre}
(defcstruct size-t
  (width :float)
  (height :float))
  @end{pre}
  Access the coordinates with the @fun{size-width} and @fun{size-height}
  functions.
  @see-symbol{size-t}
  @see-function{size-width}
  @see-function{size-height}")

(export 'size-t)

;;; --- Acessor Implementations ------------------------------------------------

;;;     size-width

(defun (setf size-width) (value size)
  (setf (foreign-slot-value size '(:struct size-t) 'width) value))

(defun size-width (size)
  (foreign-slot-value size '(:struct size-t) 'width))

#+liber-documentation
(setf (liber:alias-for-function 'size-width)
      "Accessor"
      (documentation 'size-width 'function)
 "@version{#2022-10-1}
  @syntax[]{(size-width size) => width}
  @syntax[]{(setf (size-width size) width)}
  @argument[size]{a @symbol{size-t} instance}
  @argument[width]{a single float with the width component}
  @begin{short}
    Accessor of the @code{width} slot of the @symbol{size-t} structure.
  @end{short}
  @see-symbol{size-t}")

(export 'size-width)

;;;     size-heigth

(defun (setf size-height) (value size)
  (setf (foreign-slot-value size '(:struct size-t) 'height) value))

(defun size-height (size)
  (foreign-slot-value size '(:struct size-t) 'height))

#+liber-documentation
(setf (liber:alias-for-function 'size-height)
      "Accessor"
      (documentation 'size-height 'function)
 "@version{#2022-10-1}
  @syntax[]{(size-height size) => height}
  @syntax[]{(setf (size-height size) height)}
  @argument[size]{a @symbol{size-t} instance}
  @argument[width]{a single float with the height component}
  @begin{short}
    Accessor of the @code{height} slot of the @symbol{size-t} structure.
  @end{short}
  @see-symbol{size-t}")

(export 'size-height)

;;; ----------------------------------------------------------------------------
;;; graphene_size_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_size_alloc" size-alloc)
    (:pointer (:struct size-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @return{The newly allocated @symbol{size-t} instance.}
  @begin{short}
    Allocates a new @symbol{size-t} instance.
  @end{short}
  The components of the returned @symbol{size-t} instance are initialized to
  zero. Use the @fun{size-free} function to free the resources allocated by
  this function.
  @see-symbol{size-t}
  @see-function{size-free}")

(export 'size-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_size_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_size_free" size-free) :void
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[size]{a @symbol{size-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{size-alloc} function.
  @end{short}
  @see-symbol{size-t}
  @see-function{size-alloc}"
  (size (:pointer (:struct size-t))))

(export 'size-free)

;;; ----------------------------------------------------------------------------
;;; graphene_size_zero ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_size_zero" size-zero)
    (:pointer (:struct size-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @return{The @symbol{size-t} instance with a zero size.}
  @begin{short}
    Returns a @symbol{size-t} instance with all two components set to zero.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(values (size-width (size-zero)) (size-height (size-zero)))
=> 0.0
=> 0.0
    @end{pre}
  @end{dictionary}
  @see-symbol{point-t}")

(export 'size-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_size_init ()
;;; ----------------------------------------------------------------------------

(defun size-init (size width height)
 "@version{#2022-10-1}
  @argument[size]{a @symbol{size-t} instance}
  @argument[width]{a number coerced to a single float with the width component}
  @argument[height]{a number coerced to a single float with the height
    component}
  @return{The initialized @symbol{size-t} instance.}
  @begin{short}
    Initializes the @symbol{size-t} instance to the given @arg{width} and
    @arg{height} values.
  @end{short}
  It is safe to call this function multiple times.
  @see-symbol{size-t}"
  (foreign-funcall "graphene_size_init"
                   (:pointer (:struct size-t)) size
                   :float (coerce width 'single-float)
                   :float (coerce height 'single-float)
                   (:pointer (:struct size-t))))

(export 'size-init)

;;; ----------------------------------------------------------------------------
;;; graphene_size_init_from_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_size_init_from_size" size-init-from-size)
    (:pointer (:struct size-t))
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[size]{a @symbol{size-t} instance}
  @argument[source]{a @symbol{size-t} instance to use}
  @return{The initialized @symbol{size-t} instance.}
  @short{Initializes the @symbol{sizet} instance using the values of
    @arg{source}.}
  @see-symbol{size-t}"
  (size (:pointer (:struct size-t)))
  (source (:pointer (:struct size-t))))

(export 'size-init-from-size)

;;; ----------------------------------------------------------------------------
;;; graphene_size_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_size_equal" size-equal) :bool
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{size-t} instance}
  @argument[b]{a @symbol{size-t} instance}
  @return{@em{True}, if the @symbol{size-t} instances have the same components,
    otherwise @em{false}.}
  @begin{short}
    Checks whether two given @symbol{size-t} instances are equal.
  @end{short}
  @see-symbol{size-t}"
  (a (:pointer (:struct size-t)))
  (b (:pointer (:struct size-t))))

(export 'size-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_size_interpolate ()
;;; ----------------------------------------------------------------------------

(defun size-interpolate (a b factor result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[a]{a @symbol{size-t} instance}
  @argument[b]{a @symbol{size-t} instance}
  @argument[factor]{a number coerced to a double float with the linear
    interpolation factor}
  @argument[result]{a @symbol{size-t} instance for the interpolated size}
  @return{The @symbol{size-t} instance with the interpolated size.}
  @begin{short}
    Linearly interpolates the components of @arg{a} and @arg{b} using the
    given @arg{factor}.
  @end{short}
  @see-symbol{size-t}"
  (foreign-funcall "graphene_size_interpolate"
                   (:pointer (:struct size-t)) a
                   (:pointer (:struct size-t)) b
                   :double (coerce factor 'double-float)
                   (:pointer (:struct size-t)) result
                   :void)
  result)

(export 'size-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_size_scale ()
;;; ----------------------------------------------------------------------------

(defun size-scale (size factor result)
 #+liber-documentation
 "@version{#2022-10-1}
  @argument[size]{a @symbol{size-t} instance}
  @argument[factor]{a number coerced to a single float with the scaling factor}
  @argument[result]{a @symbol{size-t} instance for the scaled size}
  @return{The @symbol{size-t} instance with the scaled size.}
  @begin{short}
    Scales the components of the given @symbol{size-t} instance by the given
    factor.
  @end{short}
  @see-symbol{size-t}"
  (foreign-funcall "graphene_size_scale"
                   (:pointer (:struct size-t)) size
                   :float (coerce factor 'single-float)
                   (:pointer (:struct size-t)) result
                   :void)
  result)

(export 'size-scale)

;;; --- End of file graphene.size.lisp -----------------------------------------
