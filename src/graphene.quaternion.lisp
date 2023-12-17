;;; ----------------------------------------------------------------------------
;;; graphene.quaternion.lisp
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
;;; Quaternion
;;;
;;;     Quaternion operations
;;;
;;; Types and Values
;;;
;;;     graphene_quaternion_t
;;;
;;; Functions
;;;
;;;     graphene_quaternion_alloc
;;;     graphene_quaternion_free
;;;     graphene_quaternion_init
;;;     graphene_quaternion_init_identity
;;;     graphene_quaternion_init_from_quaternion
;;;     graphene_quaternion_init_from_vec4
;;;     graphene_quaternion_init_from_matrix
;;;     graphene_quaternion_init_from_angles
;;;     graphene_quaternion_init_from_radians
;;;     graphene_quaternion_init_from_angle_vec3
;;;     graphene_quaternion_init_from_euler
;;;     graphene_quaternion_to_vec4
;;;     graphene_quaternion_to_matrix
;;;     graphene_quaternion_to_angles
;;;     graphene_quaternion_to_radians
;;;     graphene_quaternion_to_angle_vec3
;;;     graphene_quaternion_equal
;;;     graphene_quaternion_dot
;;;     graphene_quaternion_invert
;;;     graphene_quaternion_normalize
;;;     graphene_quaternion_add
;;;     graphene_quaternion_multiply
;;;     graphene_quaternion_scale
;;;     graphene_quaternion_slerp
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-quaternion ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-quaternion (q) body) => result}
  @syntax[]{(graphene:with-quaternion (q q1) body) => result}
  @syntax[]{(graphene:with-quaternion (q (v graphene:vec4-t)) body) => result}
  @syntax[]{(graphene:with-quaternion (q (m graphene:matrix-t)) body) => result}
  @syntax[]{(graphene:with-quaternion (q (e graphene:euler-t)) body) => result}
  @syntax[]{(graphene:with-quaternion (q angle axis) body) => result}
  @syntax[]{(graphene:with-quaternion (q xdeg ydeg zdeg) body) => result}
  @syntax[]{(graphene:with-quaternion (q xrad yrad zrad) body) => result}
  @syntax[]{(graphene:with-quaternion (q x y z w) body) => result}
  @argument[q]{a @symbol{graphene:quaternion-t} instance to create and
    initialize}
  @argument[q1]{a @symbol{graphene:quaternion-t} instance to use for
    initialization}
  @argument[v]{a @symbol{graphene:vec4-t} instance to use for initialization}
  @argument[e]{a @symbol{graphene:euler-t} instance to use for initialization}
  @argument[axis]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[angle]{a float with the rotation on a given axis}
  @argument[xdeg, ydeg, zdeg]{a float with the rotation angle, in degrees}
  @argument[xrad, yrad, zrad]{a float with the rotation angle, in radians}
  @argument[x, y, z, w]{a float with the component of a quaternion}
  @begin{short}
    The @fun{graphene:with-quaternion} macro allocates a new
    @symbol{graphene:quaternion-t} instance, initializes the box with the given
    values and executes the body that uses the box.
  @end{short}
  After execution of the body the allocated memory for the quaternion is
  released.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:quaternion-alloc} function
    and released with the @fun{graphene:quaternion-free} function.
  @end{dictionary}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:euler-t}
  @see-symbol{grapene:vec4-t}
  @see-symbol{grapene:vec3-t}
  @see-macro{graphene:with-quaternions}
  @see-function{graphene:quaternion-alloc}
  @see-function{graphene:quaternion-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (quaternion-alloc)))
            (quaternion-init ,var 0.0 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (quaternion-free ,var))))
        ((null (second args))
         ;; One argument
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 'quaternion-t))
                  ;; One argument with no type or of type quaternion-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-quaternion ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type1 'vec4-t)
                  ;; One argument with type vec4-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-vec4 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type1 'matrix-t)
                  ;; One argument with type matrix-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-matrix ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type1 'euler-t)
                  ;; One argument with type euler-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-euler ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-QUATERNION")))))
        ((null (third args))
         ;; Two arguments, these must be a float and a vec3-t
         `(let ((,var (quaternion-alloc)))
            (quaternion-init-from-angle-vec3 ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (quaternion-free ,var))))
        ((null (fourth args))
         ;; Three arguments, 3 floats in deegres or in radians
         (destructuring-bind (arg &optional type1) (mklist (first args))
           (cond ((or (not type1)
                      (eq type1 :deg))
                  ;; First argument with no type or of type :deg
                  `(let ((,var (quaternion-alloc)))
                       (quaternion-init-from-angles ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (quaternion-free ,var))))
                 ((eq type1 :rad)
                  ;; First argument with type :rad
                  `(let ((,var (quaternion-alloc)))
                       (quaternion-init-from-radians ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (quaternion-free ,var))))
                 (t
                  (error "Syntax error in GRAPHENE:WITH-QUATERNION")))))
        ((null (fifth args))
         ;; Four arguments of type float
         `(let ((,var (quaternion-alloc)))
            (quaternion-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (quaternion-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-QUATERNION"))))

(export 'with-quaternion)

(defmacro with-quaternions (vars &body body)
 #+liber-documentation
 "@version{2023-12-10}
  @syntax[]{(graphene:with-quaternions (q1 ... qn) body) => result}
  @argument[q1 ... qn]{the newly created @symbol{graphene:quaternion-t}
    instances}
  @argument[body]{a body that uses the bindings @arg{q1 ... qn}}
  @begin{short}
    The @fun{graphene:with-quaternions} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each quaternion can be initialized with values using the syntax for the
  @fun{graphene:with-quaternion} macro. See also the
  @fun{graphene:with-quaternion} documentation.
  @see-symbol{graphene:quaternion-t}
  @see-macro{graphene:with-quaternion}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-quaternion ,var
           (with-quaternions ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-quaternions)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct quaternion-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'quaternion-t)
      "CStruct"
      (liber:symbol-documentation 'quaternion-t)
 "@version{#2023-12-8}
  @begin{short}
    Quaternions are a mathematical entity that can be used to represent rotation
    transformations in 3D space.
  @end{short}
  Unlike the usual Euler representation with roll, pitch, and yaw, quaternions
  do not suffer from the so-called \"Gimbal Lock\" problem.
  @see-symbol{graphene:euler-t}")

(export 'quaternion-t)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_alloc" quaternion-alloc)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @return{The newly allocated @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:quaternion-t} instance.
  @end{short}
  The contents of the returned instance are undefined. Use the
  @fun{quaternion-free} function to free the resources allocated by this
  function.
  @see-symbol{graphene:quaternion-t}
  @see-function{graphene:quaternion-free}")

(export 'quaternion-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_free" quaternion-free) :void
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:quaternion-alloc}
    function.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-function{graphene:quaternion-alloc}"
  (quaternion (:pointer (:struct quaternion-t))))

(export 'quaternion-free)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init ()
;;; ----------------------------------------------------------------------------

(defun quaternion-init (quaternion x y z w)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[x]{a number coerced to a float with the first component of the
    quaterion}
  @argument[y]{a number coerced to a float with the second component of the
    quaterion}
  @argument[z]{a number coerced to a float with the third component of the
    quaterion}
  @argument[x]{a number coerced to a float with the fourth component of the
    quaterion}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @short{Initializes a quaternion using the given four values.}
  @see-symbol{graphene:quaternion-t}"
  (cffi:foreign-funcall "graphene_quaternion_init"
                        (:pointer (:struct quaternion-t)) quaternion
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        :float (coerce w 'single-float)
                        (:pointer (:struct quaternion-t))))

(export 'quaternion-init)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_identity ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_init_identity" quaternion-init-identity)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance to initialize}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @short{Initializes a quaternion using the identity transformation.}
  @see-symbol{graphene:quaternion-t}"
  (quaternion (:pointer (:struct quaternion-t))))

(export 'quaternion-init-identity)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_quaternion ().
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_init_from_quaternion"
               quaternion-init-from-quaternion)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[source]{a @symbol{graphene:quaternion-t} instance}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @short{Initializes a quaternion with the values from @arg{source}.}
  @see-symbol{graphene:quaternion-t}"
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-quaternion)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_vec4 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_init_from_vec4" quaternion-init-from-vec4)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[source]{a @symbol{graphene:vec4-t} instance}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @short{Initializes a quaternion with the values from @arg{source}.}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:vec4-t}"
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct vec4-t))))

(export 'quaternion-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_init_from_matrix"
               quaternion-init-from-matrix) (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Initializes a quaternion using the rotation components of a transformation
    matrix.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:matrix-t}"
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct matrix-t))))

(export 'quaternion-init-from-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_angles ()
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-angles (quaternion xdeg ydeg zdeg)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance to initialize}
  @argument[xdeg]{a number coerced to a float with the rotation angle on the
    x axis (yaw), in degrees}
  @argument[ydeg]{a number coerced to a float with the rotation angle on the
    y axis (pitch), in degrees}
  @argument[zdeg]{a number coerced to a float with the rotation angle on the
    z axis (roll), in degrees}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Initializes a quaternion using the values of the Euler angles on each axis.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-function{graphene:quaternion-init-from-euler}"
  (cffi:foreign-funcall "graphene_quaternion_init_from_angles"
                        (:pointer (:struct quaternion-t)) quaternion
                        :float (coerce xdeg 'single-float)
                        :float (coerce ydeg 'single-float)
                        :float (coerce zdeg 'single-float)
                        (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-angles)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_radians ()
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-radians (quaternion xrad yrad zrad)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance to initalize}
  @argument[xrad]{a number coerced to a float with the rotation angle on the
    x axis (yaw), in radians}
  @argument[yrad]{a number coerced to a float with the rotation angle on the
    y axis (pitch), in radians}
  @argument[zrad]{a number coerced to a float with the rotation angle on the
    z axis (roll), in radians}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Initializes a quaternion using the values of the Euler angles on each axis.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-function{graphene:quaternion-init-from-euler}"
  (cffi:foreign-funcall "graphene_quaternion_init_from_radians"
                        (:pointer (:struct quaternion-t)) quaternion
                        :float (coerce xrad 'single-float)
                        :float (coerce yrad 'single-float)
                        :float (coerce zrad 'single-float)
                        (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-radians)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_angle_vec3 ()
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-angle-vec3 (quaternion angle axis)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance to initalize}
  @argument[angle]{a number coerced to a float with the rotation on a given
    axis, in degrees}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with the axis of
    rotation, expressed as a vector}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Initializes a quaternion using an angle on a specific axis.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_quaternion_init_from_radians"
                        (:pointer (:struct quaternion-t)) quaternion
                        :float (coerce angle 'single-float)
                        (:pointer (:struct vec3-t)) axis
                        (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-angle-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_euler ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_init_from_euler" quaternion-init-from-euler)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance to initialize}
  @argument[euler]{a @symbol{graphene:euler-t} instance}
  @return{The initialized @symbol{graphene:quaternion-t} instance.}
  @begin{short}
    Initializes a quaternion using the given @symbol{graphene:euler-t} instance.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:euler-t}"
  (quaternion (:pointer (:struct quaternion-t)))
  (source :pointer)) ; euler-t not known at this point

(export 'quaternion-init-from-euler)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_to_vec4 ()
;;; ----------------------------------------------------------------------------

(defun quaternion-to-vec4 (quaternion result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance}
  @return{The @symbol{graphene:vec4-t} instance with the components.}
  @begin{short}
    Copies the components of a quaternion into a @symbol{graphene:vec4-t}
    instances.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_quaternion_to_vec4"
                        (:pointer (:struct quaternion-t)) quaternion
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'quaternion-to-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_to_matrix ()
;;; ----------------------------------------------------------------------------

(defun quaternion-to-matrix (quaternion result)
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:matrix-t} instance}
  @return{The @symbol{graphene:matrix-t} instance.}
  @begin{short}
    Converts a quaternion into a transformation matrix expressing the rotation
    defined by the quaternion.
  @end{short}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:matrix-t}"
  (cffi:foreign-funcall "graphene_quaternion_to_matrix"
                        (:pointer (:struct quaternion-t)) quaternion
                        (:pointer (:struct matrix-t)) result
                        :void)
  result)

(export 'quaternion-to-matrix)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_to_angles ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_to_angles" %quaternion-to-angles) :void
  (quaternion (:pointer (:struct quaternion-t)))
  (xdeg (:pointer :float))
  (ydeg (:pointer :float))
  (zdeg (:pointer :float)))

(defun quaternion-to-angles (quaternion)
 #+liber-documentation
 "@version{#2023-12-8}
  @syntax[]{(graphene:quaternion-to-angles quaternion) => xdeg, ydeg, zdeg}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @return{The value list of floats with the rotation angles, in degrees.}
  @begin{short}
    Converts a quaternion to its corresponding rotations on the Euler angles on
    each axis.
  @end{short}
  @see-symbol{graphene:quaternion-t}"
  (cffi:with-foreign-objects ((xdeg :float) (ydeg :float) (zdeg :float))
    (%quaternion-to-angles quaternion xdeg ydeg zdeg)
    (values (cffi:mem-ref xdeg :float)
            (cffi:mem-ref ydeg :float)
            (cffi:mem-ref zdeg :float))))

(export 'quaternion-to-angles)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_to_radians ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_to_radians" %quaternion-to-radians) :void
  (quaternion (:pointer (:struct quaternion-t)))
  (xrad (:pointer :float))
  (yrad (:pointer :float))
  (zrad (:pointer :float)))

(defun quaternion-to-radians (quaternion)
 #+liber-documentation
 "@version{#2023-12-8}
  @syntax[]{(graphene:quaternion-to-angles quaternion) => xrad, yrad, zrad}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @return{The value list of floats with the rotation angles, in radians.}
  @begin{short}
    Converts a quaternion to its corresponding rotations on the Euler angles on
    each axis.
  @end{short}
  @see-symbol{graphene:quaternion-t}"
  (cffi:with-foreign-objects ((xrad :float) (yrad :float) (zrad :float))
    (%quaternion-to-angles quaternion xrad yrad zrad)
    (values (cffi:mem-ref xrad :float)
            (cffi:mem-ref yrad :float)
            (cffi:mem-ref zrad :float))))

(export 'quaternion-to-radians)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_to_angle_vec3 ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_to_angle_vec3" %quaternion-to-angle-vec3)
    :void
  (quaternion (:pointer (:struct quaternion-t)))
  (angle (:pointer :float))
  (result (:pointer (:struct vec3-t))))

(defun quaternion-to-angle-vec3 (quaternion axis)
 #+liber-documentation
 "@version{#2023-12-8}
  @syntax[]{(graphene:quaternion-to-angle-vec3 quaterion result) => angle, result}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the rotation axis}
  @return{The value list with the angle in degrees and the
    @symbol{graphene:vec3-t} instance with the rotation axis.}
  @short{Converts a quaternion into an angle and an axis pair.}
  @see-symbol{graphene:quaternion-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:with-foreign-object (angle :float)
    (%quaternion-to-angle-vec3 quaternion angle axis)
    (values (cffi:mem-ref angle :float) axis)))

(export 'quaternion-to-angle-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_equal" quaternion-equal) :bool
 #+liber-documentation
 "@version{#2023-12-8}
  @argument[a]{a @symbol{graphene:quaternion-t} instance}
  @argument[b]{a @symbol{graphene:quaternion-t} instance}
  @return{@em{True} if the quaternions are equal.}
  @short{Checks whether the given quaternions are equal.}
  @see-symbol{graphene:quaternion-t}"
  (a (:pointer (:struct quaternion-t)))
  (b (:pointer (:struct quaternion-t))))

(export 'quaternion-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_dot ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_dot" quaternion-dot) :float
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[a]{a @symbol{graphene:quaternion-t} instance}
  @argument[b]{a @symbol{graphene:quaternion-t} instance}
  @return{The float with the value of the dot product.}
  @short{Computes the dot product of two quaternions.}
  @see-symbol{graphene:quaternion-t}"
  (a (:pointer (:struct quaternion-t)))
  (b (:pointer (:struct quaternion-t))))

(export 'quaternion-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_invert ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_invert" %quaternion-invert) :void
  (quaternion (:pointer (:struct quaternion-t)))
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-invert (quaternion result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the result}
  @return{The @symbol{graphene:quaternion-t} instance with the inverted
    quaternion.}
  @begin{short}
    Inverts a quaternion and returns the conjugate quaternion.
  @end{short}
  @see-symbol{graphene:quaternion-t}"
  (%quaternion-invert quaternion result)
  result)

(export 'quaternion-invert)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_normalize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_normalize" %quaternion-normalize) :void
  (quaternion (:pointer (:struct quaternion-t)))
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-normalize (quaternion result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the
    normalized quaternion}
  @return{The normalized @symbol{graphene:quaternion-t} instance.}
  @short{Normalizes a quaternion.}
  @see-symbol{graphene:quaternion-normalize}"
  (%quaternion-normalize quaternion result)
  result)

(export 'quaternion-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_add ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_add" %quaternion-add) :void
  (a (:pointer (:struct quaternion-t)))
  (b (:pointer (:struct quaternion-t)))
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-add (a b result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[a]{a @symbol{graphene:quaternion-t} instance}
  @argument[b]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the result}
  @return{The @symbol{graphene:quaternion-t} instance with the result.}
  @short{Adds two quaternions @arg{a} and @arg{b}.}
  @see-symbol{graphene:quaternion-t}"
  (%quaternion-add a b result)
  result)

(export 'quaternion-add)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_multiply ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_multiply" %quaternion-multiply) :void
  (a (:pointer (:struct quaternion-t)))
  (b (:pointer (:struct quaternion-t)))
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-multiply (a b result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[a]{a @symbol{graphene:quaternion-t} instance}
  @argument[b]{a @symbol{graphene:quaternion-t} instance}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the result}
  @return{The @symbol{graphene:quaternion-t} instance with the result.}
  @short{Multiplies two quaternions @arg{a} and @arg{b}.}
  @see-symbol{graphene:quaternion-t}"
  (%quaternion-multiply a b result)
  result)

(export 'quaternion-multiply)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_scale" %quaternion-scale) :void
  (quaternion (:pointer (:struct quaternion-t)))
  (factor :float)
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-scale (quaternion factor result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[quaternion]{a @symbol{graphene:quaternion-t} instance}
  @argument[factor]{a number coerced to a float with the scaling factor}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the result}
  @return{The @symbol{graphene:quaternion-t} instance with the result.}
  @short{Scales all the elements of a quaternion using the given scalar factor.}
  @see-symbol{graphene:quaternion-t}"
  (%quaternion-scale quaternion (coerce factor 'single-float) result)
  result)

(export 'quaternion-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_slerp ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quaternion_slerp" %quaternion-slerp) :void
  (a (:pointer (:struct quaternion-t)))
  (b (:pointer (:struct quaternion-t)))
  (factor :float)
  (result (:pointer (:struct quaternion-t))))

(defun quaternion-slerp (a b factor result)
 #+liber-documentation
 "@version{#2023-12-10}
  @argument[a]{a @symbol{graphene:quaternion-t} instance}
  @argument[b]{a @symbol{graphene:quaternion-t} instance}
  @argument[factor]{a number coerced to a float with the interpolation factor}
  @argument[result]{a @symbol{graphene:quaternion-t} instance for the result}
  @begin{short}
    The @symbol{graphene:quaternion-t} instance with the interpolated
    quaternion.
  @end{short}
  @see-symbol{graphene:quaternion-t}"
  (%quaternion-slerp a b (coerce factor 'single-float) result)
  result)

(export 'quaternion-slerp)

;;; --- End of file graphene.quaternion.lisp -----------------------------------
