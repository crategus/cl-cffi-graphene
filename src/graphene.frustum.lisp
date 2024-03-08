;;; ----------------------------------------------------------------------------
;;; graphene.frustum.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library. See
;;; <https://ebassi.github.io/graphene/docs/>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; Frustum
;;;
;;;     A 3D field of view
;;;
;;; Types and Values
;;;
;;;     graphene_frustum_t
;;;
;;; Functions
;;;
;;;     graphene_frustum_alloc
;;;     graphene_frustum_free
;;;     graphene_frustum_init
;;;     graphene_frustum_init_from_frustum
;;;     graphene_frustum_init_from_matrix
;;;     graphene_frustum_get_planes
;;;     graphene_frustum_contains_point
;;;     graphene_frustum_intersects_sphere
;;;     graphene_frustum_intersects_box
;;;     graphene_frustum_equal
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-frustum ((var &rest args) &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-frustum (frustum) body) => result}
  @syntax{(graphene:with-frustum (frustum frustum1) body) => result}
  @syntax{(graphene:with-frustum (frustum (matrix graphene:matrix-t)) body)
    => result}
  @syntax{(graphene:with-frustum (frustum plane0 ... plane5) body) => result}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to create and
    initialize}
  @argument[frustum1]{a @symbol{graphene:frustum-t} instance to use for
    initialization}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance to use for
    initialization}
  @argument[plane0 ... plane5]{the six @symbol{graphene:plane-t} instances to
    use for initialization}
  @begin{short}
    The @fun{graphene:with-frustum} macro allocates a new
    @symbol{graphene:frustum-t} instance, initializes the frustum with the given
    values and executes the body that uses the frustum.
  @end{short}
  After execution of the body the allocated memory for the frustum is released.

  When no argument is given the components of the frustum are undefined. The
  initialization with another frustum uses the
  @fun{graphene:frustum-init-from-frustum} function. If the first value has the
  @code{graphene:matrix-t} type the @fun{graphene:frustum-init-from-matrix}
  function is used for initialization with the matrix. The initialization from
  six planes is done with the @fun{graphene:frustum-init} function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:box-alloc} function and
    released with the @fun{graphene:box-free} function.
  @end{dictionary}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:plane-t}
  @see-macro{graphene:with-frustums}
  @see-function{graphene:frustum-alloc}
  @see-function{graphene:frustum-free}"
  (cond ((null args)
         ;; We have no arguments, we return an uninitialized frustum
         `(let ((,var (frustum-alloc)))
            (unwind-protect
              (progn ,@body)
              (frustum-free ,var))))
        ((null (second args))
         ;; One argument
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'frustum-t))
                  ;; One argument with no type or of type frustum-t
                  `(let ((,var (frustum-alloc)))
                     (frustum-init-from-frustum ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (frustum-free ,var))))
                 ((eq type1 'matrix-t)
                  ;; One argument with type matrix-t
                  `(let ((,var (frustum-alloc)))
                     (frustum-init-from-matrix ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (frustum-free ,var))))
                 (t
                  (error "Syntac error in GRAPHENE:WITH-FRUSTUM")))))
        ((null (seventh args))
         ;; Six arguments of type plane-t
         `(let ((,var (frustum-alloc)))
            (frustum-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (frustum-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-FRUSTUM"))))

(export 'with-frustum)

(defmacro with-frustums (vars &body body)
 #+liber-documentation
 "@version{2024-1-20}
  @syntax{(graphene:with-frustums (frustum1 ... frustumn) body) => result}
  @argument[frustum1 ... frustumn]{the newly created @symbol{graphene:frustum-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{frustum1 ... frustumn}}
  @begin{short}
    The @fun{graphene:with-frustums} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each point can be initialized with values using the syntax for the
  @fun{graphene:with-frustum} macro. See also the @fun{graphene:with-frustum}
  documentation.
  @see-symbol{graphene:frustum-t}
  @see-macro{graphene:with-frustum}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-frustum ,var
           (with-frustums ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-frustums)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct frustum-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'frustum-t)
      "CStruct"
      (liber:symbol-documentation 'frustum-t)
 "@version{2023-12-10}
  @begin{short}
    A @symbol{graphene:frustum-t} structure represents a volume of space
    delimited by planes.
  @end{short}
  It is usually employed to represent the field of view of a camera, and can be
  used to determine whether an object falls within that view, to efficiently
  remove invisible objects from the render process.")

(export 'frustum-t)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_alloc" frustum-alloc)
    (:pointer (:struct frustum-t))
 #+liber-documentation
 "@version{2023-12-10}
  @return{The newly allocated @symbol{graphene:frustum-t} instance. Use the
    @fun{graphene:frustum-free} function to free the resources allocated by this
    function.}
  @begin{short}
    Allocates a new @symbol{graphene:frustum-t} instance.
  @end{short}
  The contents of the returned structure are undefined.
  @see-symbol{graphene:frustum-t}
  @see-function{graphene:frustum-free}")

(export 'frustum-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_free" frustum-free) :void
 #+liber-documentation
 "@version{2023-12-10}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:frustum-alloc} function.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-function{graphene:frustum-alloc}"
  (frustum (:pointer (:struct frustum-t))))

(export 'frustum-free)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_init ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_init" frustum-init)
    (:pointer (:struct frustum-t))
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance}
  @argument[p0 ... p5]{the six @symbol{graphene:plane-t} instances with a
    clipping plane}
  @return{The initialized @symbol{graphene:frustum-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:frustum-t} instance using the
    provided clipping planes.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:plane-t}"
  (frustum (:pointer (:struct frustum-t)))
  (p0 (:pointer (:struct plane-t)))
  (p1 (:pointer (:struct plane-t)))
  (p2 (:pointer (:struct plane-t)))
  (p3 (:pointer (:struct plane-t)))
  (p4 (:pointer (:struct plane-t)))
  (p5 (:pointer (:struct plane-t))))

(export 'frustum-init)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_init_from_frustum ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_init_from_frustum" frustum-init-from-frustum)
    (:pointer (:struct frustum-t))
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to initialize}
  @argument[source]{a @symbol{graphene:frustum-t} instance}
  @return{The initialized @symbol{graphene:frustum-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:frustum-t} instance using the
    clipping planes of another @symbol{graphene:frustum-t} instance.
  @end{short}
  @see-symbol{graphene:frustum-t}"
  (frustum (:pointer (:struct frustum-t)))
  (source (:pointer (:struct frustum-t))))

(export 'frustum-init-from-frustum)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_init_from_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_init_from_matrix" frustum-init-from-matrix)
    (:pointer (:struct frustum-t))
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to initialize}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The initialized @symbol{graphene:frustum-t} instance.}
  @begin{short}
    Initializes the given @symbol{graphene:frustum-t} instance using given
    matrix.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:matrix-t}"
  (frustum (:pointer (:struct frustum-t)))
  (matrix (:pointer (:struct matrix-t))))

(export 'frustum-init-from-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_get_planes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_get_planes" %frustum-planes) :void
  (frustum (:pointer (:struct frustum-t)))
  (values-ar :pointer))

(defun frustum-planes (frustum planes)
 #+liber-documentation
 "@version{2023-12-10}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance}
  @argument[planes]{a list with six @symbol{graphene:plane-t} instances}
  @return{The list of six @symbol{graphene:plane-t} instances.}
  @begin{short}
    Retrieves the planes that define the given @symbol{graphene:frustum-t}
    instance.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:plane-t}"
  (cffi:with-foreign-object (planes-ar '(:struct plane-t) 6)
    (%frustum-planes frustum planes-ar)
    (iter (for i from 0 below 6)
          (for plane in planes)
          (for ptr = (cffi:mem-aptr planes-ar '(:struct plane-t) i))
          (plane-init-from-plane plane ptr)
          (collect plane))))

(export 'frustum-planes)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_contains_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_contains_point" frustum-contains-point) :bool
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to initialize}
  @argument[point]{a @symbol{graphene:point3d-t} instance}
  @return{@em{True} if the point is inside the frustum.}
  @begin{short}
    Checks whether a point is inside the volume defined by the given frustum.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:point3d-t}"
  (frustum (:pointer (:struct frustum-t)))
  (point (:pointer (:struct point3d-t))))

(export 'frustum-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_intersects_sphere ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_intersects_sphere" frustum-intersects-sphere)
    :bool
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to initialize}
  @argument[sphere]{a @symbol{graphene:sphere-t} instance}
  @return{@em{True} if the shpere intersects the frustum.}
  @begin{short}
    Checks whether the given sphere intersects a plane of the frustum.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:point3d-t}"
  (frustum (:pointer (:struct frustum-t)))
  (sphere (:pointer (:struct sphere-t))))

(export 'frustum-intersects-sphere)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_intersects_box ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_intersects_box" frustum-intersects-box) :bool
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[frustum]{a @symbol{graphene:frustum-t} instance to initialize}
  @argument[box]{a @symbol{graphene:box-t} instance}
  @return{@em{True} if the box intersects the frustum.}
  @begin{short}
    Checks whether the given box intersects a plane of the frustum.
  @end{short}
  @see-symbol{graphene:frustum-t}
  @see-symbol{graphene:box-t}"
  (frustum (:pointer (:struct frustum-t)))
  (box (:pointer (:struct box-t))))

(export 'frustum-intersects-box)

;;; ----------------------------------------------------------------------------
;;; graphene_frustum_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_frustum_equal" frustum-equal) :bool
 #+liber-documentation
 "@version{#2022-9-29}
  @argument[a]{a @symbol{graphene:frustum-t} instance}
  @argument[b]{a @symbol{graphene:frustum-t} instance}
  @return{@em{True} if the given frustums are equal.}
  @begin{short}
    Checks whether the given frustums are equal.
  @end{short}
  @see-symbol{graphene:frustum-t}"
  (a (:pointer (:struct frustum-t)))
  (b (:pointer (:struct frustum-t))))

(export 'frustum-equal)

;;; --- End of file graphene.frustum.lisp --------------------------------------
