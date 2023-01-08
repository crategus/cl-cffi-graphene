;;; ----------------------------------------------------------------------------
;;; graphene.quaternion.lisp
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
;;;
;;; Description
;;;
;;;     Quaternions are a mathematical entity that can be used to represent
;;;     rotation transformations in 3D space; unlike the usual Euler
;;;     representation with roll, pitch, and yaw, quaternions do not suffer
;;;     from the so-called "Gimbal Lock" problem.
;;;
;;;     See also: graphene_euler_t
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-graphene-quaternion ((var &rest args) &body body)
  (cond ((not args)
         ;; No arguments, the default is initialization with zeros.
         `(let ((,var (quaternion-alloc)))
            (quaternion-init ,var 0.0 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (quaternion-free ,var))))

        ((not (second args))
         ;; One argument.
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'quaternion-t))
                  ;; One argument with no type or of type quaternion-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-quaternion ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type 'vec4-t)
                  ;; One argument with type vec4-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-vec4 ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type 'matrix-t)
                  ;; One argument with type matrix-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-matrix ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 ((eq type 'euler-t)
                  ;; One argument with type euler-t
                  `(let ((,var (quaternion-alloc)))
                     (quaternion-init-from-euler ,var ,arg)
                     (unwind-protect
                       (progn ,@body)
                       (quaternion-free ,var))))
                 (t
                  (error "Type error in WITH-GRAPHENE-QUATERNION")))))
        ((not (third args))
         ;; Two arguments. These must be a float and a vec3-t.
         `(let ((,var (quaternion-alloc)))
            (quaternion-init-from-angle-vec3 ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (quaternion-free ,var))))

        ((not (fifth args))
         ;; Four arguments. Get a possible type from the first arugment
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type :float))
                  ;; First argument with no type or of type :float
                  `(let ((,var (quaternion-alloc)))
                       (quaternion-init ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (quaternion-free ,var))))

                 ((eq type :deg)
                  ;; First argument with type :deg
                  `(let ((,var (quaternion-alloc)))
                       (quaternion-init-from-angles ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (quaternion-free ,var))))

                 ((eq type :rad)
                  ;; First argument with type :rad
                  `(let ((,var (quaternion-alloc)))
                       (quaternion-init-from-radians ,var ,arg ,@(rest args))
                       (unwind-protect
                         (progn ,@body)
                         (quaternion-free ,var))))

                   (t
                    (error "Type error in WITH-GRAPHENE-QUATERNION")))))
        (t
         (error "Syntax error in WITH-GRAPHENE-QUATERNION"))))

(export 'with-graphene-quaternion)

(defmacro with-graphene-quaternions (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-quaternion ,var
           (with-graphene-quaternions ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-quaternions)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_t
;;;
;;; typedef struct {
;;; } graphene_quaternion_t;
;;;
;;; A quaternion.
;;;
;;; The contents of the graphene_quaternion_t structure are private and should
;;; never be accessed directly.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcstruct quaternion-t)

(export 'quaternion-t)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_alloc" quaternion-alloc)
    (:pointer (:struct quaternion-t))
 #+liber-documentation
 "@version{#2022-9-24}
  @return{The newly allocated @symbol{quaternion-t} instance. Use the
    @fun{quaternion-free} function to free the resources allocated by this
    function.}
  @begin{short}
    Allocates a new @symbol{quaternion-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{quaternion-t}
  @see-function{quaternion-free}")

(export 'quaternion-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_free" quaternion-free) :void
 #+liber-documentation
 "@version{#2022-9-24}
  @argument[quaternion]{a @symbol{quaternion-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{quaternion-alloc} function.
  @end{short}
  @see-symbol{quaternion-t}
  @see-function{quaternion-alloc}"
  (quaternion (:pointer (:struct quaternion-t))))

(export 'quaternion-free)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init (graphene_quaternion_t *q,
;;;                          float x,
;;;                          float y,
;;;                          float z,
;;;                          float w);
;;;Initializes a graphene_quaternion_t using the given four values.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;x

;;;the first component of the quaternion

;;;y

;;;the second component of the quaternion

;;;z

;;;the third component of the quaternion

;;;w

;;;the fourth component of the quaternion

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defun quaternion-init (quaternion x y z w)
  (foreign-funcall "graphene_quaternion_init"
                   (:pointer (:struct quaternion-t)) quaternion
                   :float (coerce x 'single-float)
                   :float (coerce y 'single-float)
                   :float (coerce z 'single-float)
                   :float (coerce w 'single-float)
                   (:pointer (:struct quaternion-t))))

(export 'quaternion-init)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_identity ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_identity (graphene_quaternion_t *q);
;;;Initializes a graphene_quaternion_t using the identity transformation.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_init_identity" quaternion-init-identity)
    (:pointer (:struct quaternion-t))
  (quaternion (:pointer (:struct quaternion-t))))

(export 'quaternion-init-identity)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init_from_quaternion ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_quaternion
;;;                               (graphene_quaternion_t *q,
;;;                                const graphene_quaternion_t *src);
;;;Initializes a graphene_quaternion_t with the values from src .

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;src

;;;a graphene_quaternion_t

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_init_from_quaternion"
           quaternion-init-from-quaternion)
    (:pointer (:struct quaternion-t))
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-quaternion)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_vec4 ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_vec4 (graphene_quaternion_t *q,
;;;                                    const graphene_vec4_t *src);
;;;Initializes a graphene_quaternion_t with the values from src .

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;src

;;;a graphene_vec4_t

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_init_from_vec4" quaternion-init-from-vec4)
    (:pointer (:struct quaternion-t))
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct vec4-t))))

(export 'quaternion-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init_from_matrix ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_matrix (graphene_quaternion_t *q,
;;;                                      const graphene_matrix_t *m);
;;;Initializes a graphene_quaternion_t using the rotation components of a transformation matrix.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;m

;;;a graphene_matrix_t

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_init_from_matrix" quaternion-init-from-matrix)
    (:pointer (:struct quaternion-t))
  (quaternion (:pointer (:struct quaternion-t)))
  (source (:pointer (:struct matrix-t))))

(export 'quaternion-init-from-matrix)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init_from_angles ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_angles (graphene_quaternion_t *q,
;;;                                      float deg_x,
;;;                                      float deg_y,
;;;                                      float deg_z);
;;;Initializes a graphene_quaternion_t using the values of the Euler angles on each axis.

;;;See also: graphene_quaternion_init_from_euler()

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;deg_x

;;;rotation angle on the X axis (yaw), in degrees

;;;deg_y

;;;rotation angle on the Y axis (pitch), in degrees

;;;deg_z

;;;rotation angle on the Z axis (roll), in degrees

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-angles (quaternion xdeg ydeg zdeg wdeg)
  (foreign-funcall "graphene_quaternion_init_from_angles"
                   (:pointer (:struct quaternion-t)) quaternion
                   :float (coerce xdeg 'single-float)
                   :float (coerce ydeg 'single-float)
                   :float (coerce zdeg 'single-float)
                   :float (coerce wdeg 'single-float)
                   (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-angles)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init_from_radians ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_radians (graphene_quaternion_t *q,
;;;                                       float rad_x,
;;;                                       float rad_y,
;;;                                       float rad_z);
;;;Initializes a graphene_quaternion_t using the values of the Euler angles on each axis.

;;;See also: graphene_quaternion_init_from_euler()

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;rad_x

;;;rotation angle on the X axis (yaw), in radians

;;;rad_y

;;;rotation angle on the Y axis (pitch), in radians

;;;rad_z

;;;rotation angle on the Z axis (roll), in radians

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-radians (quaternion xrad yrad zrad wrad)
  (foreign-funcall "graphene_quaternion_init_from_radians"
                   (:pointer (:struct quaternion-t)) quaternion
                   :float (coerce xrad 'single-float)
                   :float (coerce yrad 'single-float)
                   :float (coerce zrad 'single-float)
                   :float (coerce wrad 'single-float)
                   (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-radians)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_init_from_angle_vec3 ()
;;;graphene_quaternion_t *
;;;graphene_quaternion_init_from_angle_vec3
;;;                               (graphene_quaternion_t *q,
;;;                                float angle,
;;;                                const graphene_vec3_t *axis);
;;;Initializes a graphene_quaternion_t using an angle on a specific axis .

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;angle

;;;the rotation on a given axis, in degrees

;;;axis

;;;the axis of rotation, expressed as a vector

;;;Returns
;;;the initialized quaternion.
;;; ----------------------------------------------------------------------------

(defun quaternion-init-from-angle-vec3 (quaternion angle axis)
  (foreign-funcall "graphene_quaternion_init_from_radians"
                   (:pointer (:struct quaternion-t)) quaternion
                   :float (coerce angle 'single-float)
                   (:pointer (:struct vec3-t)) axis
                   (:pointer (:struct quaternion-t))))

(export 'quaternion-init-from-angle-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_quaternion_init_from_euler ()
;;;
;;; graphene_quaternion_t *
;;; graphene_quaternion_init_from_euler (graphene_quaternion_t *q,
;;;                                      const graphene_euler_t *e);
;;;
;;; Initializes a graphene_quaternion_t using the given graphene_euler_t.
;;;
;;; q :
;;;     the graphene_quaternion_t to initialize
;;;
;;; e :
;;;     a graphene_euler_t
;;;
;;; Returns :
;;;     the initialized graphene_quaternion_t.
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quaternion_init_from_euler" quaternion-init-from-euler)
    (:pointer (:struct quaternion-t))
  (quaternion (:pointer (:struct quaternion-t)))
  (source :pointer)) ; euler-t not known at this point

(export 'quaternion-init-from-euler)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_to_vec4 ()
;;;void
;;;graphene_quaternion_to_vec4 (const graphene_quaternion_t *q,
;;;                             graphene_vec4_t *res);
;;;Copies the components of a graphene_quaternion_t into a graphene_vec4_t.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;res

;;;return location for a graphene_vec4_t.
;;; ----------------------------------------------------------------------------

(defun quaternion-to-vec4 (quaternion result)
  (foreign-funcall "graphene_quaternion_to_vec4"
                   (:pointer (:struct quaternion-t)) quaternion
                   (:pointer (:struct vec4-t)) result
                   :void)
  result)

(export 'quaternion-to-vec4)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_to_matrix ()
;;;void
;;;graphene_quaternion_to_matrix (const graphene_quaternion_t *q,
;;;                               graphene_matrix_t *m);
;;;Converts a quaternion into a transformation matrix expressing the rotation defined by the graphene_quaternion_t.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;m

;;;a graphene_matrix_t.

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defun quaternion-to-matrix (quaternion result)
  (foreign-funcall "graphene_quaternion_to_matrix"
                   (:pointer (:struct quaternion-t)) quaternion
                   (:pointer (:struct matrix-t)) result
                   :void)
  result)


(export 'quaternion-to-matrix)

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_to_angles ()
;;;void
;;;graphene_quaternion_to_angles (const graphene_quaternion_t *q,
;;;                               float *deg_x,
;;;                               float *deg_y,
;;;                               float *deg_z);
;;;Converts a graphene_quaternion_t to its corresponding rotations on the Euler angles on each axis.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;deg_x

;;;return location for the rotation angle on the X axis (yaw), in degrees.

;;;deg_y

;;;return location for the rotation angle on the Y axis (pitch), in degrees.

;;;deg_z

;;;return location for the rotation angle on the Z axis (roll), in degrees.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_to_radians ()
;;;void
;;;graphene_quaternion_to_radians (const graphene_quaternion_t *q,
;;;                                float *rad_x,
;;;                                float *rad_y,
;;;                                float *rad_z);
;;;Converts a graphene_quaternion_t to its corresponding rotations on the Euler angles on each axis.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;rad_x

;;;return location for the rotation angle on the X axis (yaw), in radians.

;;;rad_y

;;;return location for the rotation angle on the Y axis (pitch), in radians.

;;;rad_z

;;;return location for the rotation angle on the Z axis (roll), in radians.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_to_angle_vec3 ()
;;;void
;;;graphene_quaternion_to_angle_vec3 (const graphene_quaternion_t *q,
;;;                                   float *angle,
;;;                                   graphene_vec3_t *axis);
;;;Converts a quaternion into an angle , axis pair.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;angle

;;;return location for the angle, in degrees.

;;;axis

;;;return location for the rotation axis.

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_equal ()
;;;bool
;;;graphene_quaternion_equal (const graphene_quaternion_t *a,
;;;                           const graphene_quaternion_t *b);
;;;Checks whether the given quaternions are equal.

;;;Parameters
;;;a

;;;a graphene_quaternion_t

;;;b

;;;a graphene_quaternion_t

;;;Returns
;;;true if the quaternions are equal

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_dot ()
;;;float
;;;graphene_quaternion_dot (const graphene_quaternion_t *a,
;;;                         const graphene_quaternion_t *b);
;;;Computes the dot product of two graphene_quaternion_t.

;;;Parameters
;;;a

;;;a graphene_quaternion_t

;;;b

;;;a graphene_quaternion_t

;;;Returns
;;;the value of the dot products

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_invert ()
;;;void
;;;graphene_quaternion_invert (const graphene_quaternion_t *q,
;;;                            graphene_quaternion_t *res);
;;;Inverts a graphene_quaternion_t, and returns the conjugate quaternion of q .

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;res

;;;return location for the inverted quaternion.

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_normalize ()
;;;void
;;;graphene_quaternion_normalize (const graphene_quaternion_t *q,
;;;                               graphene_quaternion_t *res);
;;;Normalizes a graphene_quaternion_t.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;res

;;;return location for the normalized quaternion.

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_add ()
;;;void
;;;graphene_quaternion_add (const graphene_quaternion_t *a,
;;;                         const graphene_quaternion_t *b,
;;;                         graphene_quaternion_t *res);
;;;Adds two graphene_quaternion_t a and b .

;;;Parameters
;;;a

;;;a graphene_quaternion_t

;;;b

;;;a graphene_quaternion_t

;;;res

;;;the result of the operation.

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_multiply ()
;;;void
;;;graphene_quaternion_multiply (const graphene_quaternion_t *a,
;;;                              const graphene_quaternion_t *b,
;;;                              graphene_quaternion_t *res);
;;;Multiplies two graphene_quaternion_t a and b .

;;;Parameters
;;;a

;;;a graphene_quaternion_t

;;;b

;;;a graphene_quaternion_t

;;;res

;;;the result of the operation.

;;;Since: 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_scale ()
;;;void
;;;graphene_quaternion_scale (const graphene_quaternion_t *q,
;;;                           float factor,
;;;                           graphene_quaternion_t *res);
;;;Scales all the elements of a graphene_quaternion_t q using the given scalar factor.

;;;Parameters
;;;q

;;;a graphene_quaternion_t

;;;factor

;;;a scaling factor

;;;res

;;;the result of the operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_quaternion_slerp ()
;;;void
;;;graphene_quaternion_slerp (const graphene_quaternion_t *a,
;;;                           const graphene_quaternion_t *b,
;;;                           float factor,
;;;                           graphene_quaternion_t *res);
;;;Interpolates between the two given quaternions using a spherical linear interpolation, or SLERP, using the given interpolation factor .

;;;Parameters
;;;a

;;;a graphene_quaternion_t

;;;b

;;;a graphene_quaternion_t

;;;factor

;;;the linear interpolation factor

;;;res

;;;return location for the interpolated quaternion.
;;; ----------------------------------------------------------------------------

;;; --- End of file graphene.quaternion.lisp -----------------------------------
