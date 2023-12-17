;;; ----------------------------------------------------------------------------
;;; graphene.euler.lisp
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
;;; Euler
;;;
;;;     Euler angles
;;;
;;; Types and Values
;;;
;;;     graphene_euler_t
;;;     graphene_euler_order_t
;;;
;;; Functions
;;;
;;;     graphene_euler_alloc
;;;     graphene_euler_free
;;;     graphene_euler_init
;;;     graphene_euler_init_with_order
;;;     graphene_euler_init_from_matrix
;;;     graphene_euler_init_from_quaternion
;;;     graphene_euler_init_from_vec3
;;;     graphene_euler_init_from_euler
;;;     graphene_euler_init_from_radians
;;;     graphene_euler_equal
;;;     graphene_euler_get_x
;;;     graphene_euler_get_y
;;;     graphene_euler_get_z
;;;     graphene_euler_get_order
;;;     graphene_euler_get_alpha
;;;     graphene_euler_get_beta
;;;     graphene_euler_get_gamma
;;;     graphene_euler_to_vec3
;;;     graphene_euler_to_matrix
;;;     graphene_euler_to_quaternion
;;;     graphene_euler_reorder
;;; ----------------------------------------------------------------------------

(in-package :graphene)

;; TODO: Add the initialization from radians

(defmacro with-euler ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-euler (euler) body) => result}
  @syntax[]{(graphene:with-euler (euler euler1) body) => result}
  @syntax[]{(graphene:with-euler (euler matrix order) body) => result}
  @syntax[]{(graphene:with-euler (euler (quat graphene:quaternion-t) order) body)
    => result}
  @syntax[]{(graphene:with-euler (euler (vec graphene:vec3-t) order) body)
    => result}
  @syntax[]{(graphene:with-euler (euler x y z) body) => result}
  @syntax[]{(graphene:with-euler (euler x y z order) body) => result}
  @argument[euler]{a @symbol{graphene:euler-t} instance to create and
    initialize}
  @argument[euler1]{a @symbol{graphene:euler-t} instance to use for
    initialization}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance to use for
    initialization}
  @argument[quat]{a @symbol{graphene:quaternion-t} instance to use for
    initialization}
  @argument[vec]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[x, y, z]{a float value}
  @argument[order]{a @symbol{graphene:euler-order-t} value}
  @begin{short}
    The @fun{graphene:with-euler} macro allocates a new
    @symbol{graphene:euler-t} instance, initializes the box with the given
    values and executes the body that uses the box.
  @end{short}
  After execution of the body the allocated memory for the instance is released.

  When no argument is given the components of the instance are undefined. The
  initialization from another instance is done with the
  @fun{graphene:euler-init-from-euler} function. The initialization with two
  values uses the @fun{graphene:euler-init-from-function},
  @fun{graphene:euler-init-from-quaternion} and
  @fun{graphene:euler-init-from-vec3} functions. The
  @fun{graphene:euler-init} and @fun{graphene:euler-init-with-order} functions
  initializes an instance with the rotation angles in degrees.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:box-alloc} function and
    released with the @fun{graphene:box-free} function.
  @end{dictionary}
  @see-symbol{graphene:box-t}
  @see-symbol{graphene:point3d-t}
  @see-symbol{grapene:vec3-t}
  @see-macro{graphene:with-boxes}
  @see-function{graphene:box-alloc}
  @see-function{graphene:box-free}"
  (cond ((null args)
         ;; We have no arguments, the content is undefined
         `(let ((,var (euler-alloc)))
            (unwind-protect
              (progn ,@body)
              (euler-free ,var))))
        ((null (second args))
         ;; We have one argument. The argument must be of type euler-t
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'euler-t))
                  ;; One argument with no type or of type euler-t
                  `(let ((,var (euler-alloc)))
                     (euler-init-from-euler ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (euler-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-EULER")))))
        ((null (third args))
         ;; We have two arguments. The first can be of type matrix-t,
         ;; quaternion-t, or vec3-t. The second argument is the order
         (destructuring-bind ((arg1 &optional type1)
                              (arg2 &optional type2))
             (list (mklist (first args)) (mklist (second args)))
           (cond ((and (or (not type1)
                           (eq type1 'matrix-t))
                       (not type2))
                  ;; First argument with no type or of type matrix-t and
                  ;; second argument with no type
                  `(let ((,var (euler-alloc)))
                     (euler-init-from-matrix ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (euler-free ,var))))
                 ((and (eq type1 'quaternion-t)
                       (not type2))
                  ;; First argument of type quaternion-t and second argument
                  ;; with no type
                  `(let ((,var (euler-alloc)))
                     (euler-init-from-quaternion ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (euler-free ,var))))
                 ((and (eq type1 'vec3-t)
                       (not type2))
                  ;; First argument of type vec3-t and second argument
                  ;; with no type
                  `(let ((,var (euler-alloc)))
                     (euler-init-from-vec3 ,var ,arg1 ,arg2)
                     (unwind-protect
                       (progn ,@body)
                       (euler-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-EULER")))))
        ((not (fourth args))
         ;; We have three numbers
         `(let ((,var (euler-alloc)))
            (euler-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (euler-free ,var))))
        ((not (fifth args))
         ;; We have three numbers and the order
         `(let ((,var (euler-alloc)))
            (euler-init-with-order ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (euler-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-EULER"))))

(export 'with-euler)

(defmacro with-eulers (vars &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-euler (euler1 ... eulern) body) => result}
  @argument[euler1 ... eulern]{the newly created @symbol{graphene:euler-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{euler1 ... eulern}}
  @begin{short}
    The @fun{graphene:with-eulers} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each instance can be initialized with values using the syntax for the
  @fun{graphene:with-euler} macro. See also the @fun{graphene:with-euler}
  documentation.
  @see-symbol{graphene:euler-t}
  @see-macro{graphene:with-euler}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-euler ,var
           (with-eulers ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-eulers)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_order_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum euler-order-t
  :default
  :XYZ  :YZX  :ZXY  :XZY  :YXZ  :ZYX

  :SXYZ :SXYX :SXZY :SXZX
  :SYZX :SYZY :SYXZ :SYXY
  :SZXY :SZXZ :SZYX :SZYZ

  :RZYX :RXYX :RYZX :RXZX
  :RXZY :RYZY :RZXY :RYXY
  :RYXZ :RZXZ :RXYZ :RZYZ)

#+liber-documentation
(setf (liber:alias-for-symbol 'euler-order-t)
      "CEnum"
      (liber:symbol-documentation 'euler-order-t)
 "@version{#2022-9-23}
  @begin{short}
    Specify the order of the rotations on each axis.
  @end{short}
  The @code{:default} value is special, and is used as an alias for one of the
  other orders.
  @begin{pre}
(cffi:defcenum euler-order-t
  :default

  :XYZ  :YZX  :ZXY  :XZY  :YXZ  :ZYX

  :SXYZ :SXYX :SXZY :SXZX
  :SYZX :SYZY :SYXZ :SYXY
  :SZXY :SZXZ :SZYX :SZYZ

  :RZYX :RXYX :RYZX :RXZX
  :RXZY :RYZY :RZXY :RYXY
  :RYXZ :RZXZ :RXYZ :RZYZ)
  @end{pre}
  @begin[code]{table}
    @entry[:default]{Rotate in the default order. The default order is one of
      the following enumeration values.}
    @entry[:XYZ]{Rotate in the X, Y, and Z order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SXYZ}.}
    @entry[:YZX]{Rotate in the Y, Z, and X order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SYZX}.}
    @entry[:ZXY]{Rotate in the Z, X, and Y order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SZXY}.}
    @entry[:XZY]{Rotate in the X, Z, and Y order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SXZY}.}
    @entry[:YXZ]{Rotate in the Y, X, and Z order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SYXZ}.}
    @entry[:ZYX]{Rotate in the Z, Y, and X order. Deprecated in Graphene 1.10,
      it is an alias for @code{:SZYX}.}
    @entry[:SXYZ]{Defines a static rotation along the X, Y, and Z axes.}
    @entry[:SXYX]{Defines a static rotation along the X, Y, and X axes.}
    @entry[:SXZY]{Defines a static rotation along the X, Z, and Y axes.}
    @entry[:SXZX]{Defines a static rotation along the X, Z, and X axes.}
    @entry[:SYZX]{Defines a static rotation along the Y, Z, and X axes.}
    @entry[:SYZY]{Defines a static rotation along the Y, Z, and Y axes.}
    @entry[:SYXZ]{Defines a static rotation along the Y, X, and Z axes.}
    @entry[:SYXY]{Defines a static rotation along the Y, X, and Y axes.}
    @entry[:SZXY]{Defines a static rotation along the Z, X, and Y axes.}
    @entry[:SZXZ]{Defines a static rotation along the Z, X, and Z axes.}
    @entry[:SZYX]{Defines a static rotation along the Z, Y, and X axes.}
    @entry[:SZYZ]{Defines a static rotation along the Z, Y, and Z axes.}
    @entry[:RZYX]{Defines a relative rotation along the Z, Y, and X axes.}
    @entry[:RXYX]{Defines a relative rotation along the X, Y, and X axes.}
    @entry[:RYZX]{Defines a relative rotation along the Y, Z, and X axes.}
    @entry[:RXZX]{Defines a relative rotation along the X, Z, and X axes.}
    @entry[:RXZY]{Defines a relative rotation along the X, Z, and Y axes.}
    @entry[:RYZY]{Defines a relative rotation along the Y, Z, and Y axes.}
    @entry[:RZXY]{Defines a relative rotation along the Z, X, and Y axes.}
    @entry[:RYXY]{Defines a relative rotation along the Y, X, and Y axes.}
    @entry[:RYXZ]{Defines a relative rotation along the Y, X, and Z axes.}
    @entry[:RZXZ]{Defines a relative rotation along the Z, X, and Z axes.}
    @entry[:RXYZ]{Defines a relative rotation along the X, Y, and Z axes.}
    @entry[:RZYZ]{Defines a relative rotation along the Z, Y, and Z axes-}
  @end{table}
  @see-symbol{graphene:euler-t}")

(export 'euler-order-t)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct euler-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'euler-t)
      "CStruct"
      (liber:symbol-documentation 'euler-t)
 "@version{#2022-9-23}
  @begin{short}
    The @symbol{graphene:euler-t} structure defines a rotation along three axes
    using three angles.
  @end{short}
  It also optionally can describe the order of the rotations.

  Euler's rotation theorem states that, in three-dimensional space, any
  displacement of a rigid body such that a point on the rigid body remains
  fixed, is equivalent to a single rotation about some axis that runs through
  the fixed point. The angles on each axis can be placed in a vector of three
  components—α, β, and γ—called the *Euler angle vector*. Each rotation
  described by these components results in a rotation matrix:
  @begin{pre}
rot(α) = A
rot(β) = B
rot(γ) = G
  @end{pre}
  The resulting rotation matrix expressed by the Euler angle vector is given by
  the product of each rotation matrix:
  @begin{pre}
G × B × A = R
  @end{pre}
  In order to specify the meaning of an Euler angle vector, we need to assign
  each axis of rotation to the corresponding α, β, and γ components, for
  instance X, Y, and Z.

  Additionally, we need to specify whether the rotations move the axes as they
  are applied, also known as intrinsic, or relative rotations; or if the axes
  stay fixed and the vectors move within the axis frame, also known as
  extrinsic, or static rotations. For instance, a static rotation alongside the
  ZYX axes will be interpreted as relative to extrinsic coordinate axes, and be
  performed, in order, about the Z, Y, and finally X axis. A relative rotation
  alongside the ZXZ axes will be interpreted as relative to intrinsic coordinate
  axes, and be performed, in order, about the Z axis, about the rotated X axis,
  and finally about the rotated Z axis.

  Finally, we need to define the direction of the rotation, or the handedness of
  the coordinate system. In the case of Graphene, the direction is given by the
  right-hand rule, which means all rotations are counterclockwise.

  Rotations described Euler angles are typically immediately understandable,
  compared to rotations expressed using quaternions, but they are susceptible of
  \"Gimbal lock\" - the loss of one degree of freedom caused by two axis on the
  same plane. You typically should use the @symbol{graphene:euler-t} structure
  to expose rotation angles in your API, or to store them, but use the
  @symbol{graphene:quaternion-t} to apply rotations to modelview matrices, or
  interpolate between initial and final rotation transformations.

  For more information, see:
  @begin{itemize}
    @item{@url[http://en.wikipedia.org/wiki/Rotation_matrix]{Wikipedia,
      Rotation Matrix}}
    @item{@url[http://en.wikipedia.org/wiki/Euler_angles]{Wikipedia,
      Euler Angels}}
    @item{@url[http://mathworld.wolfram.com/EulerAngles.html]{Mathworld, Euler
      Angles}}
    @item{\"Representing Attitude with Euler Angles and Quaternions: A
      Reference\" by James Diebel, 2006}
    @item{\"Graphics Gems IV\", edited by Paul Heckbert, Academic Press, 1994.}
  @end{itemize}
  @see-symbol{graphene:quaternion-t}")

(export 'euler-t)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_alloc" euler-alloc)
    (:pointer (:struct euler-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @return{The newly allocated @symbol{graphene:euler-t} instance.}
  @short{Allocates a new @symbol{graphene:euler-t} instance.}
  The contents of the returned structure are undefined.
  @see-symbol{graphene:euler-t}
  @see-function{graphene:euler-free}")

(export 'euler-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_free" euler-free) :void
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:euler-alloc} function.
  @end{short}
  @see-symbol{graphene:euler-t}
  @see-function{graphene:euler-alloc}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-free)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init ()
;;; ----------------------------------------------------------------------------

(defun euler-init (euler x y z)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[x]{a float with the rotation angle on the x axis, in degrees}
  @argument[y]{a float with the rotation angle on the y axis, in degrees}
  @argument[z]{a float with the rotation angle on the z axis, in degrees}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the given angles.
  @end{short}
  The order of the rotations is the @code{:default} value of the
  @symbol{graphene:euler-order-t} enumeration.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:euler-order-t}"
  (cffi:foreign-funcall "graphene_euler_init"
                        (:pointer (:struct euler-t)) euler
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        (:pointer (:struct euler-t))))

(export 'euler-init)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_with_order ()
;;; ----------------------------------------------------------------------------

(defun euler-init-with-order (euler x y z order)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[x]{a float with the rotation angle on the x axis, in degrees}
  @argument[y]{a float with the rotation angle on the y axis, in degrees}
  @argument[z]{a float with the rotation angle on the z axis, in degrees}
  @argument[order]{a @symbol{graphene:euler-order-t} value with the order used
    to apply the rotations}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the given angles and
    order.
  @end{short}
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:euler-order-t}"
  (cffi:foreign-funcall "graphene_euler_init_with_order"
                        (:pointer (:struct euler-t)) euler
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        euler-order-t order
                        (:pointer (:struct euler-t))))

(export 'euler-init-with-order)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_from_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_init_from_matrix" euler-init-from-matrix)
    (:pointer (:struct euler-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the rotation
    matrix}
  @argument[order]{a @symbol{graphene:euler-order-t} value with the order used
    to apply the rotations}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the given rotation
    matrix.
  @end{short}
  If the matrix is @code{null-pointer}, the @symbol{graphene:euler-t} instance
  will be initialized with all angles set to 0.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:euler-order-t}"
  (euler (:pointer (:struct euler-t)))
  (matrix (:pointer (:struct matrix-t)))
  (order euler-order-t))

(export 'euler-init-from-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_from_quaternion ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_init_from_quaternion" euler-init-from-quaternion)
    (:pointer (:struct euler-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[quaternion]{a normalized @symbol{graphene:quaternion-t} instance}
  @argument[order]{a @symbol{graphene:euler-order-t} value with the order used
    to apply the rotations}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the given normalized
    quaternion.
  @end{short}
  If the quaternion is @code{null-pointer}, the @symbol{graphene:euler-t}
  instance will be initialized with all angles set to 0.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:euler-order-t}"
  (euler (:pointer (:struct euler-t)))
  (quaternion (:pointer (:struct quaternion-t)))
  (order euler-order-t))

(export 'euler-init-from-quaternion)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_from_vec3 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_init_from_vec3" euler-init-from-vec3)
    (:pointer (:struct euler-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[vector]{a @symbol{graphene:vec3-t} instance containing the rotation
    angles in degrees}
  @argument[order]{a @symbol{graphene:euler-order-t} value with the order used
    to apply the rotations}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the angles contained
    in the vector.
  @end{short}
  If the vector is @code{null-pointer}, the @symbol{graphene:euler-t} instance
  will be initialized with all angles set to 0.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:euler-order-t}"
  (euler (:pointer (:struct euler-t)))
  (vector (:pointer (:struct vec3-t)))
  (order euler-order-t))

(export 'euler-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_from_euler ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_init_from_euler" euler-init-from-euler)
    (:pointer (:struct euler-t))
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance to initialize}
  @argument[source]{a @symbol{graphene:euler-t} instance}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the angles and order
    of another @symbol{graphene:euler-t} instance.
  @end{short}
  If @arg{source} is @code{null-pointer}, the @symbol{graphene:euler-t} instance
  will be initialized with all angles set to 0.
  @see-symbol{graphene:euler-t}"
  (euler (:pointer (:struct euler-t)))
  (source (:pointer (:struct euler-t))))

(export 'euler-init-from-euler)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_init_from_radians ()
;;; ----------------------------------------------------------------------------

(defun euler-init-from-radians (euler x y z order)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[x]{a float with the rotation angle on the x axis, in radians}
  @argument[y]{a float with the rotation angle on the y axis, in radians}
  @argument[z]{a float with the rotation angle on the z axis, in radians}
  @argument[order]{a @symbol{graphene:euler-order-t} value with the order used
    to apply the rotations}
  @return{The initialized @symbol{graphene:euler-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:euler-t} instance using the given angles and
    order of rotation.
  @end{short}
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:euler-order-t}"
  (cffi:foreign-funcall "graphene_euler_init_from_radians"
                        (:pointer (:struct euler-t)) euler
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        euler-order-t order
                        (:pointer (:struct euler-t))))

(export 'euler-init-from-radians)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_equal" euler-equal) :bool
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[a]{a @symbol{graphene:euler-t} instance}
  @argument[b]{a @symbol{graphene:euler-t} instance}
  @return{@em{true} if the two @symbol{graphene:euler-t} instances are equal.}
  @short{Checks if two @symbol{graphene:euler-t} instances are equal.}
  @see-symbol{graphene:euler-t}"
  (a (:pointer (:struct euler-t)))
  (b (:pointer (:struct euler-t))))

(export 'euler-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_x ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_x" euler-x) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the rotation angle.}
  @short{Retrieves the rotation angle on the x axis, in degrees.}
  @see-symbol{graphene:euler-t}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-x)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_y ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_y" euler-y) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the rotation angle.}
  @short{Retrieves the rotation angle on the y axis, in degrees.}
  @see-symbol{graphene:euler-t}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-y)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_z ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_z" euler-z) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the rotation angle.}
  @short{Retrieves the rotation angle on the z axis, in degrees.}
  @see-symbol{graphene:euler-t}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-z)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_order ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_order" euler-order) euler-order-t
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The @symbol{graphene:euler-order-t} value with the order to apply
    rotations.}
  @begin{short}
    Retrieves the order used to apply the rotations described in the
    @symbol{graphene:euler-t} instance, when converting to and from other
    structures, like @symbol{graphene:quaternion-t} and
    @symbol{graphene:matrix-t} instances.
  @end{short}
  This function does not return the @code{:default} enumeration value. It will
  return the effective order of rotation instead.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:euler-order-t}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:matrix-t}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-order)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_alpha ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_alpha" euler-alpha) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the first component of the Euler angle, in radians.}
  @begin{short}
    Retrieves the first component of the Euler angle vector, depending on the
    order of rotation.
  @end{short}
  See also the @fun{graphene:euler-x} function.
  @see-symbol{graphene:euler-t}
  @see-function{graphene:euler-x}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-alpha)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_beta ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_beta" euler-beta) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the second component of the Euler angle, in radians.}
  @begin{short}
    Retrieves the first component of the Euler angle vector, depending on the
    order of rotation.
  @end{short}
  See also the @fun{graphene:euler-y} function.
  @see-symbol{graphene:euler-t}
  @see-function{graphene:euler-y}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-beta)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_get_gamma ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_euler_get_gamma" euler-gamma) :float
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The float with the third component of the Euler angle, in radians.}
  @begin{short}
    Retrieves the first component of the Euler angle vector, depending on the
    order of rotation.
  @end{short}
  See also the @fun{graphene:euler-z} function.
  @see-symbol{graphene:euler-t}
  @see-function{graphene:euler-z}"
  (euler (:pointer (:struct euler-t))))

(export 'euler-gamma)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_to_vec3 ()
;;; ----------------------------------------------------------------------------

(defun euler-to-vec3 (euler result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Retrieves the angles of a @symbol{graphene:euler-t} instance and initializes
    a @symbol{graphene:vec3-t} instance with them.
  @end{short}
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_euler_to_vec3"
                        (:pointer (:struct euler-t)) euler
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'euler-to-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_to_matrix ()
;;; ----------------------------------------------------------------------------

(defun euler-to-matrix (euler result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[result]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Converts a @symbol{graphene:euler-t} instance into a transformation matrix
    expressing the extrinsic composition of rotations described by the Euler
    angles.
  @end{short}

  The rotations are applied over the reference frame axes in the order
  associated with the @symbol{graphene:euler-t} instance. For instance, if the
  order used to initialize @arg{euler} is the @code{:xyz} value:
  @begin{itemize}
    @item{the first rotation moves the body around the X axis with an angle φ}
    @item{the second rotation moves the body around the Y axis with an angle
      of ϑ}
    @item{the third rotation moves the body around the Z axis with an angle
      of ψ}
  @end{itemize}
  The rotation sign convention is right-handed, to preserve compatibility
  between Euler-based, quaternion-based, and angle-axis-based rotations.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_euler_to_matrix"
                        (:pointer (:struct euler-t)) euler
                        (:pointer (:struct matrix-t)) result
                        :void)
  result)

(export 'euler-to-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_to_quaternion ()
;;; ----------------------------------------------------------------------------

(defun euler-to-quaternion (euler result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[result]{a @symbol{graphene:quaternion-t} instance}
  @return{The @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Converts a @symbol{graphene:euler-t} instance into a
    @symbol{graphene:quaternion-t} instance.
  @end{short}
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:quaternion-t}"
  (cffi:foreign-funcall "graphene_euler_to_quaternion"
                        (:pointer (:struct euler-t)) euler
                        (:pointer (:struct quaternion-t)) result
                        :void)
  result)

(export 'euler-to-quaternion)

;;; ----------------------------------------------------------------------------
;;; graphene_euler_reorder ()
;;; ----------------------------------------------------------------------------

(defun euler-reorder (euler order result)
 #+liber-documentation
 "@version{#2022-9-23}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @argument[order]{a @symbol{graphene:euler-order-t} value}
  @argument[result]{a @symbol{graphene:euler-t} instance}
  @return{The @symbol{graphene:euler-t} instance.}
  @begin{short}
    Reorders a @symbol{graphene:euler-t} instance using @arg{order}.
  @end{short}
  This function is equivalent to creating a @symbol{graphene:quaternion-t}
  instance from the given @symbol{graphene:euler-t} instance, and then
  converting the quaternion into another @symbol{graphene:euler-t} instance.
  @see-symbol{graphene:euler-t}
  @see-symbol{graphene:quaternion-t}"
  (cffi:foreign-funcall "graphene_euler_reorder"
                        (:pointer (:struct euler-t)) euler
                        euler-order-t order
                        (:pointer (:struct euler-t)) result
                        :void)
  result)

(export 'euler-reorder)

;;; --- End of file graphene.euler.lisp ----------------------------------------
