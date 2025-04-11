;;; ----------------------------------------------------------------------------
;;; graphene.vector.lisp
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
;;; Vectors
;;;
;;;     Vectors in 2, 3, and 4 dimensions
;;;
;;; Types and Values
;;;
;;;     GRAPHENE_VEC2_LEN
;;;     graphene_vec2_t
;;;     GRAPHENE_VEC3_LEN
;;;     graphene_vec3_t
;;;     GRAPHENE_VEC4_LEN
;;;     graphene_vec4_t
;;;
;;; Functions
;;;
;;;     graphene_vec2_alloc
;;;     graphene_vec2_free
;;;     graphene_vec2_init
;;;     graphene_vec2_init_from_vec2
;;;     graphene_vec2_init_from_float
;;;     graphene_vec2_to_float
;;;     graphene_vec2_add
;;;     graphene_vec2_subtract
;;;     graphene_vec2_multiply
;;;     graphene_vec2_divide
;;;     graphene_vec2_dot
;;;     graphene_vec2_scale
;;;     graphene_vec2_length
;;;     graphene_vec2_normalize
;;;     graphene_vec2_negate
;;;     graphene_vec2_equal
;;;     graphene_vec2_near
;;;     graphene_vec2_min
;;;     graphene_vec2_max
;;;     graphene_vec2_interpolate
;;;     graphene_vec2_get_x
;;;     graphene_vec2_get_y
;;;     graphene_vec2_zero
;;;     graphene_vec2_one
;;;     graphene_vec2_x_axis
;;;     graphene_vec2_y_axis
;;;
;;;     graphene_vec3_alloc
;;;     graphene_vec3_free
;;;     graphene_vec3_init
;;;     graphene_vec3_init_from_vec3
;;;     graphene_vec3_init_from_float
;;;     graphene_vec3_to_float
;;;     graphene_vec3_add
;;;     graphene_vec3_subtract
;;;     graphene_vec3_multiply
;;;     graphene_vec3_divide
;;;     graphene_vec3_cross
;;;     graphene_vec3_dot
;;;     graphene_vec3_scale
;;;     graphene_vec3_length
;;;     graphene_vec3_normalize
;;;     graphene_vec3_negate
;;;     graphene_vec3_equal
;;;     graphene_vec3_near
;;;     graphene_vec3_min
;;;     graphene_vec3_max
;;;     graphene_vec3_interpolate
;;;     graphene_vec3_get_x
;;;     graphene_vec3_get_y
;;;     graphene_vec3_get_z
;;;     graphene_vec3_get_xy
;;;     graphene_vec3_get_xy0
;;;     graphene_vec3_get_xyz0
;;;     graphene_vec3_get_xyz1
;;;     graphene_vec3_get_xyzw
;;;     graphene_vec3_zero
;;;     graphene_vec3_one
;;;     graphene_vec3_x_axis
;;;     graphene_vec3_y_axis
;;;     graphene_vec3_z_axis
;;;
;;;     graphene_vec4_alloc
;;;     graphene_vec4_free
;;;     graphene_vec4_init
;;;     graphene_vec4_init_from_vec4
;;;     graphene_vec4_init_from_vec3
;;;     graphene_vec4_init_from_vec2
;;;     graphene_vec4_init_from_float
;;;     graphene_vec4_to_float
;;;     graphene_vec4_add
;;;     graphene_vec4_subtract
;;;     graphene_vec4_multiply
;;;     graphene_vec4_divide
;;;     graphene_vec4_dot
;;;     graphene_vec4_scale
;;;     graphene_vec4_length
;;;     graphene_vec4_normalize
;;;     graphene_vec4_negate
;;;     graphene_vec4_equal
;;;     graphene_vec4_near
;;;     graphene_vec4_min
;;;     graphene_vec4_max
;;;     graphene_vec4_interpolate
;;;     graphene_vec4_get_x
;;;     graphene_vec4_get_y
;;;     graphene_vec4_get_z
;;;     graphene_vec4_get_w
;;;     graphene_vec4_get_xy
;;;     graphene_vec4_get_xyz
;;;     graphene_vec4_zero
;;;     graphene_vec4_one
;;;     graphene_vec4_x_axis
;;;     graphene_vec4_y_axis
;;;     graphene_vec4_z_axis
;;;     graphene_vec4_w_axis
;;; ----------------------------------------------------------------------------

(in-package :graphene)

;;; ----------------------------------------------------------------------------

(defmacro with-vec2 ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec2 (v) body) => result}
  @syntax{(graphene:with-vec2 (v v1) body) => result}
  @syntax{(graphene:with-vec2 (v x y) body) => result}
  @argument[v]{a @symbol{graphene:vec2-t} instance to create and initialize}
  @argument[v1]{a @symbol{graphene:vec2-t} instance to use for initialization}
  @argument[x]{a number coerced to a single float for the x component}
  @argument[y]{a number coerced to a single float for the y component}
  @begin{short}
    The @fun{graphene:with-vec2} macro allocates a new @symbol{graphene:vec2-t}
    instance, initializes the vector with the given values and executes the body
    that uses the vector.
  @end{short}
  After execution of the body the allocated memory for the vector is released.

  When no argument is given the components of the vector are initialized to
  zero. The initialization from another vector is done with the
  @fun{graphene:vec2-init-from-vec2} function. The initialization with two
  values @arg{x} and @arg{y} uses the @fun{graphene:vec2-init} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:vec2-alloc} function and
    released with the @fun{graphene:vec2-free} function.
  @end{dictionary}
  @see-symbol{graphene:vec2-t}
  @see-macro{graphene:with-vec2s}
  @see-function{graphene:vec2-alloc}
  @see-function{graphene:vec2-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (vec2-alloc)))
            (vec2-init ,var 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (vec2-free ,var))))
        ((null (second args))
         ;; One argument of type vec2-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'vec2-t)
                  ;; One argument of type vec2-t
                  `(let ((,var (vec2-alloc)))
                     (vec2-init-from-vec2 ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (vec2-free ,var))))
                 (t
                  ;; One argument with no type, default is vec2-t
                  `(let ((,var (vec2-alloc)))
                     (vec2-init-from-vec2 ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (vec2-free ,var)))))))
        ((null (third args))
         ;; Two arguments for the (x,y) values
         `(let ((,var (vec2-alloc)))
            (vec2-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (vec2-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-VEC2"))))

(export 'with-vec2)

(defmacro with-vec2s (vars &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec2s (v1 v2 v3 ... vn) body) => result}
  @argument[v1 ... vn]{newly created @symbol{graphene:vec2-t} instances}
  @argument[body]{a body that uses the bindings @arg{v1 ... vn}}
  @begin{short}
    The @fun{graphene:with-vec2s} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each vector can be initialized with values using the syntax for the
  @fun{graphene:with-vec2} macro. See also the @fun{graphene:with-vec2}
  documentation.
  @see-symbol{graphene:vec2-t}
  @see-macro{graphene:with-vec2}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-vec2 ,var
           (with-vec2s ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-vec2s)

;;; ----------------------------------------------------------------------------

(defmacro with-vec3 ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec3 (v) body) => result}
  @syntax{(graphene:with-vec3 (v v1) body) => result}
  @syntax{(graphene:with-vec3 (v x y z) body) => result}
  @argument[v]{a @symbol{graphene:vec3-t} instance to create and initialize}
  @argument[v1]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[x]{a number coerced to a single float for the x component}
  @argument[y]{a number coerced to a single float for the y component}
  @argument[z]{a number coerced to a single float for the z component}
  @begin{short}
    The @fun{graphene:with-vec3} macro allocates a new @symbol{graphene:vec3-t}
    instance, initializes the vector with the given values and executes the body
    that uses the vector.
  @end{short}
  After execution of the body the allocated memory for the vector is released.

  When no argument is given the components of the vector are initialized to
  zero. The initialization from another vector is done with the
  @fun{graphene:vec3-init-from-vec3} function. The initialization with three
  values @arg{x}, @arg{y} and @arg{z} uses the @fun{graphene:vec3-init}
  function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:vec3-alloc} function and
    released with the @fun{graphene:vec3-free} function.
  @end{dictionary}
  @see-symbol{graphene:vec3-t}
  @see-macro{graphene:with-vec3s}
  @see-function{graphene:vec3-alloc}
  @see-function{graphene:vec3-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (vec3-alloc)))
            (vec3-init ,var 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (vec3-free ,var))))
        ((null (second args))
         ;; One argument of type vec3-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'vec3-t)
                  ;; One argument of type vec3-t
                  `(let ((,var (vec3-alloc)))
                     (vec3-init-from-vec3 ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (vec3-free ,var))))
                 (t
                  ;; One argument with no type, default is vec3-t
                  `(let ((,var (vec3-alloc)))
                     (vec3-init-from-vec3 ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (vec3-free ,var)))))))
        ((null (fourth args))
         ;; List of three arguments for the (x,y,z) values
         `(let ((,var (vec3-alloc)))
            (vec3-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (vec3-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-VEC3"))))

(export 'with-vec3)

(defmacro with-vec3s (vars &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec3s (v1 v2 v3 ... vn) body) => result}
  @argument[v1 ... vn]{newly created @symbol{graphene:vec3-t} instances}
  @argument[body]{a body that uses the bindings @arg{v1 ... vn}}
  @begin{short}
    The @fun{graphene:with-vec3s} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each vector can be initialized with values using the syntax for the
  @fun{graphene:with-vec3} macro. See also the @fun{graphene:with-vec3}
  documentation.
  @see-symbol{graphene:vec3-t}
  @see-macro{graphene:with-vec3}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-vec3 ,var
           (with-vec3s ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-vec3s)

;;; ----------------------------------------------------------------------------

(defmacro with-vec4 ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec4 (v) body) => result}
  @syntax{(graphene:with-vec4 (v v1) body) => result}
  @syntax{(graphene:with-vec4 (v v2 w) body) => result}
  @syntax{(graphene:with-vec4 (v v3 z w) body) => result}
  @syntax{(graphene:with-vec4 (v x y z w) body) => result}
  @argument[v]{a @symbol{graphene:vec4-t} instance to create and initialize}
  @argument[v1]{a @symbol{graphene:vec4-t} instance to use for initialization}
  @argument[v2]{a @symbol{graphene:vec3-t} instance to use for initialization}
  @argument[v3]{a @symbol{graphene:vec2-t} instance to use for initialization}
  @argument[x]{a number coerced to a single float for the x component}
  @argument[y]{a number coerced to a single float for the y component}
  @argument[z]{a number coerced to a single float for the z component}
  @argument[w]{a number coerced to a single float for the w component}
  @begin{short}
    The @fun{graphene:with-vec4} macro allocates a new @symbol{graphene:vec4-t}
    instance, initializes the vector with the given values and executes the body
    that uses the vector.
  @end{short}
  After execution of the body the allocated memory for the vector is released.

  When no argument is given the components of the vector are initialized to
  zero. The initialization from another vector is done with the
  @fun{graphene:vec4-init-from-vec4} function. The initialization with two
  values uses the @fun{graphene:vec4-init-from-vec3} function and the
  initialization with three values values uses the
  @fun{graphene:vec4-init-from-vec2} function. At last, the
  @fun{graphene:vec4-init} function initializes the vector from four values.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:vec4-alloc} function and
    released with the @fun{graphene:vec4-free} function.
  @end{dictionary}
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec2-t}
  @see-macro{graphene:with-vec4s}
  @see-function{graphene:vec4-alloc}
  @see-function{graphene:vec4-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (vec4-alloc)))
            (vec4-init ,var 0.0 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (vec4-free ,var))))
        ((null (second args))
         ;; One argument of type vec4-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'vec4-t)
                  ;; One argument of type vec4-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec4 ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var))))
                 (t
                  ;; One argument with no type, default is type vec4-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec4 ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var)))))))
        ((null (third args))
         ;; Two argument,the first argument must be of type vec3-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'vec3-t)
                  ;; First argument of type vec3-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec3 ,var ,arg1 ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var))))
                 (t
                  ;; First argument with no type, default is vec3-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec3 ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var)))))))
        ((null (fourth args))
         ;; Three arguments, the first argument must be of type vec2-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'vec2-t)
                  ;; First argument of type vec2-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec2 ,var ,arg1 ,@(rest args))
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var))))
                 (t
                  ;; First argument of no type, default is vec2-t
                  `(let ((,var (vec4-alloc)))
                     (vec4-init-from-vec2 ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (vec4-free ,var)))))))
        ((null (fifth args))
         ;; List of four arguments for the (x,y,z,w) values
         `(let ((,var (vec4-alloc)))
            (vec4-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (vec4-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-VEC4"))))

(export 'with-vec4)

(defmacro with-vec4s (vars &body body)
 #+liber-documentation
 "@version{2025-4-5}
  @syntax{(graphene:with-vec4s (v1 v2 v3 ... vn) body) => result}
  @argument[v1 ... vn]{newly created @symbol{graphene:vec4-t} instances}
  @argument[body]{a body that uses the bindings @arg{v1 ... vn}}
  @begin{short}
    The @fun{graphene:with-vec4s} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each vector can be initialized with values using the syntax for the
  @fun{graphene:with-vec4} macro. See also the @fun{graphene:with-vec4}
  documentation.
  @see-symbol{graphene:vec4-t}
  @see-macro{graphene:with-vec4}"
  (if vars
      (let ((var (mklist (first vars))))
        `(with-vec4 ,var
           (with-vec4s ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-vec4s)

;;; ----------------------------------------------------------------------------
;;; GRAPHENE_VEC2_LEN
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+vec2-len+) "Constant")

(defconstant +vec2-len+ 2
 #+liber-documentation
 "@version{2025-4-5}
  @variable-value{2}
  @begin{short}
    Evaluates to the number of components of a @symbol{graphene:vec2-t}
    structure.
  @end{short}
  @see-symbol{graphene:vec2-t}")

(export '+vec2-len+)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct vec2-t
  (value :float :count 4)) ; place for 4 float values

#+liber-documentation
(setf (liber:alias-for-symbol 'vec2-t)
      "CStruct"
      (liber:symbol-documentation 'vec2-t)
 "@version{2025-4-5}
  @begin{declaration}
(cffi:defcstruct vec2-t
  (value :float :count 4)) ; place for 4 single floats
  @end{declaration}
  @begin{short}
    The @symbol{graphene:vec2-t} structure is capable of holding a vector with
    two dimensions x and y.
  @end{short}
  Use the @fun{graphene:vec2-x} and @fun{graphene:vec2-y} functions to get the
  values of the components. There are no functions to set the values directly.
  @see-function{graphene:vec2-x}
  @see-function{graphene:vec2-y}")

(export 'vec2-t)

;;; ----------------------------------------------------------------------------
;;; GRAPHENE_VEC3_LEN
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+vec3-len+) "Constant")

(defconstant +vec3-len+ 3
 #+liber-documentation
 "@version{2025-4-5}
  @variable-value{3}
  @begin{short}
    Evaluates to the number of components of a @symbol{graphene:vec3-t}
    structure.
  @end{short}
  @see-symbol{graphene:vec3-t}")

(export '+vec3-len+)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct vec3-t
  (value :float :count 4)) ; place for 4 float values

#+liber-documentation
(setf (liber:alias-for-symbol 'vec3-t)
      "CStruct"
      (liber:symbol-documentation 'vec3-t)
 "@version{2025-4-5}
  @begin{declaration}
(cffi:defcstruct vec3-t
  (value :float :count 4)) ; place for 4 single floats
  @end{declaration}
  @begin{short}
    The @symbol{graphene:vec3-t} structure is capable of holding a vector with
    three dimensions x, y, and z.
  @end{short}
  Use the @fun{graphene:vec3-x}, @fun{graphene:vec3-y}, and
  @fun{graphene:vec3-z} functions to get the values of the components. There
  are no functions to set the values directly.
  @see-function{graphene:vec3-x}
  @see-function{graphene:vec3-y}
  @see-function{graphene:vec3-z}")

(export 'vec3-t)

;;; ----------------------------------------------------------------------------
;;; GRAPHENE_VEC4_LEN
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+vec4-len+) "Constant")

(defconstant +vec4-len+ 4
 #+liber-documentation
 "@version{2025-4-5}
  @variable-value{4}
  @begin{short}
    Evaluates to the number of components of a @symbol{graphene:vec4-t}
    structure.
  @end{short}
  @see-symbol{graphene:vec4-t}")

(export '+vec4-len+)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct vec4-t
  (value :float :count 4)) ; place for 4 float values

#+liber-documentation
(setf (liber:alias-for-symbol 'vec4-t)
      "CStruct"
      (liber:symbol-documentation 'vec4-t)
 "@version{2025-4-5}
  @begin{declaration}
(cffi:defcstruct vec4-t
  (value :float :count 4)) ; place for 4 single floats
  @end{declaration}
  @begin{short}
    The @symbol{graphene:vec4-t} structure is capable of holding a vector with
    four dimensions x, y, z and w.
  @end{short}
  Use the @fun{graphene:vec4-x}, @fun{graphene:vec4-y}, @fun{graphene:vec4-z},
  and @fun{graphene:vec4-w} functions to get the values of the components.
  There are no functions to set the values directly.
  @see-function{graphene:vec4-x}
  @see-function{graphene:vec4-y}
  @see-function{graphene:vec4-z}
  @see-function{graphene:vec4-w}")

(export 'vec4-t)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_alloc" vec2-alloc) (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The newly allocated @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:vec2-t} instance.
  @end{short}
  Use the @fun{graphene:vec2-free} function to free the resources allocated by
  this function.
  @see-symbol{graphene:vec2-t}
  @see-function{graphene:vec2-free}")

(export 'vec2-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_free" vec2-free) :void
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @short{Frees the resources allocated by @arg{v}.}
  @see-symbol{graphene:vec2-t}
  @see-function{graphene:vec2-alloc}"
  (v (:pointer (:struct vec2-t))))

(export 'vec2-free)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_init
;;; ----------------------------------------------------------------------------

(defun vec2-init (v x y)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[x]{a number coerced to a single float for the x field of the vector}
  @argument[y]{a number coerced to a single float for the y field of the vector}
  @return{The initialized @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec2-t} instance using the given values.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_init"
                        (:pointer (:struct vec2-t)) v
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        (:pointer (:struct vec2-t))))

(export 'vec2-init)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_init_from_vec2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_init_from_vec2" vec2-init-from-vec2)
    (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[source]{a @symbol{graphene:vec2-t} instance}
  @return{The initialized @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec2-t} instance using another
    @symbol{graphene:vec2-t} instance.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec2-t}"
  (v (:pointer (:struct vec2-t)))
  (source (:pointer (:struct vec2-t))))

(export 'vec2-init-from-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_init_from_float
;;; ----------------------------------------------------------------------------

(defun vec2-init-from-float (v arg)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[arg]{a list with two numbers coerced to single floats}
  @return{The initialized @symbol{graphene:vec2-t} instance.}
  @short{Initializes @arg{v} with the contents of the given list.}
  @begin[Notes]{dictionary}
    The Lisp implementation does not use the C function, but calls the
    @fun{graphene:vec2-init} function to inialize the vector.
    @begin{pre}
(defun vec2-init-from-float (v arg)
  (apply #'graphene:vec2-init v arg))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:vec2-t}
  @see-function{graphene:vec2-init}"
  (apply #'vec2-init v arg))

(export 'vec2-init-from-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_to_float
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_to_float" %vec2-to-float) :void
  (v (:pointer (:struct vec2-t)))
  (v-ar (:pointer :float)))

(defun vec2-to-float (v)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @return{The list with the single floats for the components of @arg{v}.}
  @begin{short}
    Stores the components of @arg{v} into a list.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-vec2 (v 1/2 3/2) (graphene:vec2-to-float v))
=> (0.5 1.5)
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:vec2-t}"
  (cffi:with-foreign-object (v-ar :float +vec2-len+)
    (%vec2-to-float v v-ar)
    (iter (for i from 0 below +vec2-len+)
          (collect (cffi:mem-aref v-ar :float i)))))

(export 'vec2-to-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_add
;;; ----------------------------------------------------------------------------

(defun vec2-add (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Adds each component of the two passed vectors and places each result into
    the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_add"
                        (:pointer (:struct vec2-t)) a
                        (:pointer (:struct vec2-t)) b
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-add)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_subtract
;;; ----------------------------------------------------------------------------

(defun vec2-subtract (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Subtracts from each component of the first operand @arg{a} the corresponding
    component of the second operand @arg{b} and places each result into the
    components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_subtract"
                        (:pointer (:struct vec2-t)) a
                        (:pointer (:struct vec2-t)) b
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-subtract)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_multiply
;;; ----------------------------------------------------------------------------

(defun vec2-multiply (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Multiplies each component of the two passed vectors and places each result
    into the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_multiply"
                        (:pointer (:struct vec2-t)) a
                        (:pointer (:struct vec2-t)) b
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-multiply)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_divide
;;; ----------------------------------------------------------------------------

;; Workaround uses the VEC4-DIVIDE function and return the first components

(defun vec2-divide (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Divides each component of the first operand @arg{a} by the corresponding
    component of the second operand @arg{b}, and places the results into the
    vector @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (with-vec4s ((v1 a 1.0 1.0) (v2 b 1.0 1.0) res)
    (vec4-divide v1 v2 res)
    (vec4-xy res result)))

(export 'vec2-divide)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_dot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_dot" vec2-dot) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @return{The single float with the dot product of the vectors.}
  @begin{short}
    Computes the dot product of the two given vectors.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (a (:pointer (:struct vec2-t)))
  (b (:pointer (:struct vec2-t))))

(export 'vec2-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_scale
;;; ----------------------------------------------------------------------------

(defun vec2-scale (v factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[factor]{a number coerced to a single float for the scale factor}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result vector.}
  @begin{short}
    Multiplies all components of the given vector with the given scalar factor.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_scale"
                        (:pointer (:struct vec2-t)) v
                        :float (coerce factor 'single-float)
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_length" vec2-length) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @return{The single float with the length of the vector.}
  @begin{short}
    Computes the length of the given vector.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (v (:pointer (:struct vec2-t))))

(export 'vec2-length)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_normalize
;;; ----------------------------------------------------------------------------

(defun vec2-normalize (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the normalized vector.}
  @begin{short}
    Computes the normalized vector for the given vector.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_normalize"
                        (:pointer (:struct vec2-t)) v
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_negate
;;; ----------------------------------------------------------------------------

(defun vec2-negate (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the negated vector.}
  @begin{short}
    Negates the given vector.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_negate"
                        (:pointer (:struct vec2-t)) v
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-negate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_equal" vec2-equal) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec2-t} instance}
  @argument[v2]{a @symbol{graphene:vec2-t} instance}
  @return{@em{True} if the two vectors are equal, and @em{false} otherwise}
  @begin{short}
    Checks whether the two given vectors are equal.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (v1 (:pointer (:struct vec2-t)))
  (v2 (:pointer (:struct vec2-t))))

(export 'vec2-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_near
;;; ----------------------------------------------------------------------------

;; TODO: Consider to use a default value for epsilon

(defun vec2-near (v1 v2 epsilon)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec2-t} instance}
  @argument[v2]{a @symbol{graphene:vec2-t} instance}
  @argument[epsilon]{a number coerced to a single float for the treshold
    between the two vectors}
  @begin{return}
    @em{True} if the two vectors are near each other, and @em{false} otherwise.
  @end{return}
  @begin{short}
    Compares the two given vectors and checks whether their values are within
    the given @arg{epsilon}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_near"
                        (:pointer (:struct vec2-t)) v1
                        (:pointer (:struct vec2-t)) v2
                        :float (coerce epsilon 'single-float)
                        :bool))

(export 'vec2-near)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_min
;;; ----------------------------------------------------------------------------

(defun vec2-min (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the minimum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_min"
                        (:pointer (:struct vec2-t)) a
                        (:pointer (:struct vec2-t)) b
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-min)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_max
;;; ----------------------------------------------------------------------------

(defun vec2-max (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec2-t} instance}
  @argument[b]{a @symbol{graphene:vec2-t} instance}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the maximum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_max"
                        (:pointer (:struct vec2-t)) a
                        (:pointer (:struct vec2-t)) b
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-max)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_interpolate
;;; ----------------------------------------------------------------------------

(defun vec2-interpolate (v1 v2 factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec2-t} instance}
  @argument[v2]{a @symbol{graphene:vec2-t} instance}
  @argument[factor]{a number coerced to a double float for the interpolation
    factor}
  @argument[result]{a @symbol{graphene:vec2-t} instance for the result}
  @return{The @symbol{graphene:vec2-t} instance with the interpolated vector.}
  @begin{short}
    Linearly interpolates @arg{v1} and @arg{v2} using the given factor.
  @end{short}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec2_interpolate"
                        (:pointer (:struct vec2-t)) v1
                        (:pointer (:struct vec2-t)) v2
                        :double (coerce factor 'double-float)
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec2-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_get_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_get_x" vec2-x) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @return{The single float with the value of the x component.}
  @short{Retrieves the x component of the vector.}
  @see-symbol{graphene:vec2-t}"
  (v (:pointer (:struct vec2-t))))

(export 'vec2-x)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_get_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_get_y" vec2-y) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec2-t} instance}
  @return{The single float with the value of the y component.}
  @short{Retrieves the y component of the vector.}
  @see-symbol{graphene:vec2-t}"
  (v (:pointer (:struct vec2-t))))

(export 'vec2-y)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_zero" vec2-zero) (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec2-t} instance with (0.0, 0.0) components.}
  @short{Retrieves a constant zero vector.}
  @see-symbol{graphene:vec2-t}")

(export 'vec2-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_one" vec2-one) (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec2-t} instance with (1.0, 1.0) components.}
  @short{Retrieves a constant one vector.}
  @see-symbol{graphene:vec2-t}")

(export 'vec2-one)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_x_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_x_axis" vec2-x-axis) (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec2-t} instance with the X axis vector.}
  @short{Retrieves a constant vector with (1.0, 0.0) components.}
  @see-symbol{graphene:vec2-t}")

(export 'vec2-x-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec2_y_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec2_y_axis" vec2-y-axis) (:pointer (:struct vec2-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec2-t} instance with the Y axis vector.}
  @short{Retrieves a constant vector with (0.0, 1.0) components.}
  @see-symbol{graphene:vec2-t}")

(export 'vec2-y-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_alloc" vec3-alloc) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The newly allocated @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:vec3-t} instance.
  @end{short}
  Use the @fun{graphene:vec3-free} function to free the resources allocated by
  this function.
  @see-symbol{graphene:vec3-t}
  @see-function{graphene:vec3-free}")

(export 'vec3-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_free" vec3-free) :void
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @short{Frees the resources allocated by @arg{v}.}
  @see-symbol{graphene:vec3-t}
  @see-function{graphene:vec3-alloc}"
  (vector (:pointer (:struct vec3-t))))

(export 'vec3-free)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_init
;;; ----------------------------------------------------------------------------

(defun vec3-init (v x y z)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[x]{a number coerced to a single float for the x field of the vector}
  @argument[y]{a number coerced to a single float for the y field of the vector}
  @argument[z]{a number coerced to a single float for the z field of the vector}
  @return{The initialized @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec3-t} instance using the given values.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_init"
                        (:pointer (:struct vec3-t)) v
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        (:pointer (:struct vec3-t))))

(export 'vec3-init)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_init_from_vec3
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_init_from_vec3" vec3-init-from-vec3)
    (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[source]{a @symbol{graphene:vec3-t} instance}
  @return{The initialized @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec3-t} instance using another
    @symbol{graphene:vec3-t} instance.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec3-t}"
  (v (:pointer (:struct vec3-t)))
  (source (:pointer (:struct vec3-t))))

(export 'vec3-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_init_from_float
;;; ----------------------------------------------------------------------------

(defun vec3-init-from-float (v arg)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[arg]{a list with three numbers coerced to single floats}
  @return{The initialized @symbol{graphene:vec3-t} instance.}
  @short{Initializes @arg{v} with the contents of the given list.}
  @begin[Notes]{dictionary}
    The Lisp implementation does not use the C function, but calls the
    @fun{graphene:vec3-init} function to inialize the vector.
    @begin{pre}
(defun vec3-init-from-float (v arg)
  (apply #'vec3-init v arg))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:vec3-t}
  @see-function{graphene:vec3-init}"
  (apply #'vec3-init v arg))

(export 'vec3-init-from-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_to_float
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_to_float" %vec3-to-float) :void
  (v (:pointer (:struct vec3-t)))
  (v-ar (:pointer :float)))

(defun vec3-to-float (v)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The list with the single floats of the components of @arg{v}.}
  @begin{short}
    Stores the components of @arg{v} into a list.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:with-foreign-object (v-ar :float +vec3-len+)
    (%vec3-to-float v v-ar)
    (iter (for i from 0 below +vec3-len+)
          (collect (cffi:mem-aref v-ar :float i)))))

(export 'vec3-to-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_add
;;; ----------------------------------------------------------------------------

(defun vec3-add (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Adds each component of the two passed vectors and places each result into
    the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_add"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-add)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_subtract
;;; ----------------------------------------------------------------------------

(defun vec3-subtract (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Subtracts from each component of the first operand @arg{a} the corresponding
    component of the second operand @arg{b} and places each result into the
    components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_subtract"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-subtract)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_multiply
;;; ----------------------------------------------------------------------------

(defun vec3-multiply (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Multiplies each component of the two passed vectors and places each result
    into the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_multiply"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-multiply)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_divide
;;; ----------------------------------------------------------------------------

;; Workaround uses the VEC4-DIVIDE function

(defun vec3-divide (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Divides each component of the first operand @arg{a} by the corresponding
    component of the second operand @arg{b}, and places the results into the
    vector @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (with-vec4s ((v1 a 1.0) (v2 b 1.0) res)
    (vec4-divide v1 v2 res)
    (vec4-xyz res result)))

(export 'vec3-divide)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_cross
;;; ----------------------------------------------------------------------------

(defun vec3-cross (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Computes the cross product of the two given vectors.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_cross"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-cross)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_dot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_dot" vec3-dot) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @return{The single float with the dot product of the vectors.}
  @begin{short}
    Computes the dot product of the two given vectors.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (a (:pointer (:struct vec3-t)))
  (b (:pointer (:struct vec3-t))))

(export 'vec3-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_scale
;;; ----------------------------------------------------------------------------

(defun vec3-scale (v factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[factor]{a number coerced to a single float for the scale factor}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result vector.}
  @begin{short}
    Multiplies all components of the given vector with the given scalar factor.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_scale"
                        (:pointer (:struct vec3-t)) v
                        :float (coerce factor 'single-float)
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_length" vec3-length) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The single float with the length of the vector.}
  @begin{short}
    Computes the length of the given vector.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (v (:pointer (:struct vec3-t))))

(export 'vec3-length)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_normalize
;;; ----------------------------------------------------------------------------

(defun vec3-normalize (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the normalized vector.}
  @begin{short}
    Computes the normalized vector for the given vector.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_normalize"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_negate
;;; ----------------------------------------------------------------------------

(defun vec3-negate (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the negated vector.}
  @begin{short}
    Negates the given vector.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_negate"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-negate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_equal" vec3-equal) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec3-t} instance}
  @argument[v2]{a @symbol{graphene:vec3-t} instance}
  @return{@em{True} if the two vectors are equal, and @em{false} otherwise}
  @begin{short}
    Checks whether the two given vectors are equal.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (v1 (:pointer (:struct vec3-t)))
  (v2 (:pointer (:struct vec3-t))))

(export 'vec3-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_near
;;; ----------------------------------------------------------------------------

(defun vec3-near (v1 v2 epsilon)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec3-t} instance}
  @argument[v2]{a @symbol{graphene:vec3-t} instance}
  @argument[epsilon]{a number coerced to a single float for the treshold
    between the two vectors}
  @begin{return}
    @em{True} if the two vectors are near each other, and @em{false} otherwise.
  @end{return}
  @begin{short}
    Compares the two given vectors and checks whether their values are within
    the given @arg{epsilon}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_near"
                        (:pointer (:struct vec3-t)) v1
                        (:pointer (:struct vec3-t)) v2
                        :float (coerce epsilon 'single-float)
                        :bool))

(export 'vec3-near)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_min
;;; ----------------------------------------------------------------------------

(defun vec3-min (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the minimum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_min"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-min)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_max
;;; ----------------------------------------------------------------------------

(defun vec3-max (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec3-t} instance}
  @argument[b]{a @symbol{graphene:vec3-t} instance}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the maximum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_max"
                        (:pointer (:struct vec3-t)) a
                        (:pointer (:struct vec3-t)) b
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-max)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_interpolate
;;; ----------------------------------------------------------------------------

(defun vec3-interpolate (v1 v2 factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec3-t} instance}
  @argument[v2]{a @symbol{graphene:vec3-t} instance}
  @argument[factor]{a number coerced to a double float for the interpolation
    factor}
  @argument[result]{a @symbol{graphene:vec3-t} instance for the result}
  @return{The @symbol{graphene:vec3-t} instance with the interpolated vector.}
  @begin{short}
    Linearly interpolates @arg{v1} and @arg{v2} using the given factor.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_interpolate"
                        (:pointer (:struct vec3-t)) v1
                        (:pointer (:struct vec3-t)) v2
                        :double (coerce factor 'double-float)
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_get_x" vec3-x) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The single float with the value of the x component.}
  @short{Retrieves the x component of the vector.}
  @see-symbol{graphene:vec3-t}"
  (v (:pointer (:struct vec3-t))))

(export 'vec3-x)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_get_y" vec3-y) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The single float with the value of the y component.}
  @short{Retrieves the y component of the vector.}
  @see-symbol{graphene:vec3-t}"
  (v (:pointer (:struct vec3-t))))

(export 'vec3-y)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_z
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_get_z" vec3-z) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The single float with the value of the z component.}
  @short{Retrieves the z component of the vector.}
  @see-symbol{graphene:vec3-t}"
  (v (:pointer (:struct vec3-t))))

(export 'vec3-z)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_xy
;;; ----------------------------------------------------------------------------

(defun vec3-xy (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec2-t} instance that contains the first two
    components of the given vector.
  @end{short}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec3_get_xy"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec3-xy)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_xy0
;;; ----------------------------------------------------------------------------

(defun vec3-xy0 (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec3-t} instance that contains the first two
    components of the given vector, and the third component set to 0.0.
  @end{short}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec3_get_xy0"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec3-xy0)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_xyz0
;;; ----------------------------------------------------------------------------

(defun vec3-xyz0 (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec4-t} instance that contains the first three
    components of the given vector, and the fourth component set to 0.0.
  @end{short}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec3_get_xyz0"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec3-xyz0)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_xyz1
;;; ----------------------------------------------------------------------------

(defun vec3-xyz1 (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @return{The @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec4-t} instance that contains the first three
    components of the given vector, and the fourth component set to 1.0.
  @end{short}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec3_get_xyz1"
                        (:pointer (:struct vec3-t)) v
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec3-xyz1)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_get_xyzw
;;; ----------------------------------------------------------------------------

(defun vec3-xyzw (v w result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-t} instance}
  @argument[w]{a number coerced to a single float for the value of the fourth
    component}
  @return{The @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec4-t} instance that contains the first three
    components of the given vector, and the fourth component set to @arg{w}.
  @end{short}
  @see-symbol{graphene:vec3-t}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec3_get_xyzw"
                        (:pointer (:struct vec3-t)) v
                        :float (coerce w 'single-float)
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec3-xyzw)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_zero" vec3-zero) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @begin{return}
    The @symbol{graphene:vec3-t} instance with three components, all sets
    to 0.0.
  @end{return}
  @short{Retrieves a constant zero vector.}
  @see-symbol{graphene:vec3-t}")

(export 'vec3-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_one" vec3-one) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @begin{return}
    The @symbol{graphene:vec3-t} instance with three components, all sets
    to 1.0.
  @end{return}
  @short{Retrieves a constant one vector.}
  @see-symbol{graphene:vec3-t}")

(export 'vec3-one)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_x_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_x_axis" vec3-x-axis) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec3-t} instance with the X axis vector.}
  @short{Retrieves a constant vector with (1.0, 0.0, 0.0) components.}
  @see-symbol{graphene:vec3-t}")

(export 'vec3-x-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_y_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_y_axis" vec3-y-axis) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec3-t} instance with the Y axis vector.}
  @short{Retrieves a constant vector with (0.0, 1.0, 0.0) components.}
  @see-symbol{graphene:vec3-t}")

(export 'vec3-y-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec3_z_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec3_z_axis" vec3-z-axis) (:pointer (:struct vec3-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec3-t} instance with the Z axis vector.}
  @short{Retrieves a constant vector with (0.0, 0.0, 1.0) components.}
  @see-symbol{graphene:vec3-t}")

(export 'vec3-z-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_alloc" vec4-alloc) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The newly allocated @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:vec4-t} instance.
  @end{short}
  Use the @fun{graphene:vec4-free} function to free the resources allocated by
  this function.
  @see-symbol{graphene:vec4-t}
  @see-function{graphene:vec4-free}")

(export 'vec4-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_free" vec4-free) :void
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec3-4} instance}
  @short{Frees the resources allocated by @arg{v}.}
  @see-symbol{graphene:vec4-t}
  @see-function{graphene:vec4-alloc}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-free)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_init
;;; ----------------------------------------------------------------------------

(defun vec4-init (v x y z w)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[x]{a number coerced to a single float for the x field of the vector}
  @argument[y]{a number coerced to a single float for the y field of the vector}
  @argument[z]{a number coerced to a single float for the z field of the vector}
  @argument[w]{a number coerced to a sindle float for the w field of the vector}
  @return{The initialized @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec4-t} instance using the given values.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_init"
                        (:pointer (:struct vec4-t)) v
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce z 'single-float)
                        :float (coerce w 'single-float)
                        (:pointer (:struct vec4-t))))

(export 'vec4-init)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_init_from_vec4
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_init_from_vec4" vec4-init-from-vec4)
    (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[source]{a @symbol{graphene:vec4-t} instance}
  @return{The initialized @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec4-t} instance using another
    @symbol{graphene:vec4-t} instance.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t)))
  (source (:pointer (:struct vec4-t))))

(export 'vec4-init-from-vec4)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_init_from_vec3
;;; ----------------------------------------------------------------------------

(defun vec4-init-from-vec3 (v src w)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[source]{a @symbol{graphene:vec4-t} instance}
  @argument[w]{a number coerced to a single float for the value of the fourth
    component of @arg{v}}
  @return{The initialized @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec4-t} instance using another
    @symbol{graphene:vec3-t} instance and the value of @arg{w}.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec4_init_from_vec3"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec3-t)) src
                        :float (coerce w 'single-float)
                        (:pointer (:struct vec4-t))))

(export 'vec4-init-from-vec3)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_init_from_vec2
;;; ----------------------------------------------------------------------------

(defun vec4-init-from-vec2 (v src z w)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[source]{a @symbol{graphene:vec2-t} instance}
  @argument[z]{a number coerced to a single float for the value of the third
    component of @arg{v}}
  @argument[w]{a number coerced to a single float for the value of the fourth
    component of @arg{v}}
  @return{The initialized @symbol{graphene:vec4-t} instance.}
  @begin{short}
    Initializes a @symbol{graphene:vec4-t} instance using another
    @symbol{graphene:vec2-t} instance and the values of @arg{z} and @arg{w}.
  @end{short}
  This function can be called multiple times.
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec4_init_from_vec2"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec2-t)) src
                        :float (coerce z 'single-float)
                        :float (coerce w 'single-float)
                        (:pointer (:struct vec4-t))))

(export 'vec4-init-from-vec2)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_init_from_float
;;; ----------------------------------------------------------------------------

(defun vec4-init-from-float (v arg)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[arg]{a list with four numbers coerced to single floats}
  @return{The initialized @symbol{graphene:vec4-t} instance.}
  @short{Initializes @arg{v} with the contents of the given list.}
  @begin[Notes]{dictionary}
    The Lisp implementation does not use the C function, but calls the
    @fun{graphene:vec4-init} function to inialize the vector.
    @begin{pre}
(defun vec4-init-from-float (v arg)
  (apply #'vec4-init v arg))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:vec4-t}
  @see-function{graphene:vec4-init}"
  (apply #'vec4-init v arg))

(export 'vec4-init-from-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_to_float
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_to_float" %vec4-to-float) :void
  (v (:pointer (:struct vec4-t)))
  (v-ar (:pointer :float)))

(defun vec4-to-float (v)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The list with the single floats of the components of @arg{v}.}
  @begin{short}
    Stores the components of @arg{v} into a list.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:with-foreign-object (v-ar :float +vec4-len+)
    (%vec4-to-float v v-ar)
    (iter (for i from 0 below +vec4-len+)
          (collect (cffi:mem-aref v-ar :float i)))))

(export 'vec4-to-float)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_add
;;; ----------------------------------------------------------------------------

(defun vec4-add (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Adds each component of the two passed vectors and places each result into
    the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_add"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-add)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_subtract
;;; ----------------------------------------------------------------------------

(defun vec4-subtract (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Subtracts from each component of the first operand @arg{a} the corresponding
    component of the second operand @arg{b} and places each result into the
    components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_subtract"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-subtract)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_multiply
;;; ----------------------------------------------------------------------------

(defun vec4-multiply (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Multiplies each component of the two passed vectors and places each result
    into the components of @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_multiply"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-multiply)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_divide
;;; ----------------------------------------------------------------------------

(defun vec4-divide (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Divides each component of the first operand @arg{a} by the corresponding
    component of the second operand @arg{b}, and places the results into the
    vector @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_divide"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-divide)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_dot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_dot" vec4-dot) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the dot product of the vectors.}
  @begin{short}
    Computes the dot product of the two given vectors.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (a (:pointer (:struct vec4-t)))
  (b (:pointer (:struct vec4-t))))

(export 'vec4-dot)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_scale
;;; ----------------------------------------------------------------------------

(defun vec4-scale (v factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[factor]{a number coerced to a single float for the scale factor}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result vector.}
  @begin{short}
    Multiplies all components of the given vector with the given scalar factor.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_scale"
                        (:pointer (:struct vec4-t)) v
                        :float (coerce factor 'single-float)
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-scale)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_length" vec4-length) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the length of the vector.}
  @begin{short}
    Computes the length of the given vector.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-length)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_normalize
;;; ----------------------------------------------------------------------------

(defun vec4-normalize (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the normalized vector.}
  @begin{short}
    Computes the normalized vector for the given vector.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_normalize"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_negate
;;; ----------------------------------------------------------------------------

(defun vec4-negate (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the negated vector.}
  @begin{short}
    Negates the given vector.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_negate"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-negate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_equal" vec4-equal) :bool
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec4-t} instance}
  @argument[v2]{a @symbol{graphene:vec4-t} instance}
  @return{@em{True} if the two vectors are equal, and @em{false} otherwise}
  @begin{short}
    Checks whether the two given vectors are equal.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (v1 (:pointer (:struct vec4-t)))
  (v2 (:pointer (:struct vec4-t))))

(export 'vec4-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_near
;;; ----------------------------------------------------------------------------

(defun vec4-near (v1 v2 epsilon)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec4-t} instance}
  @argument[v2]{a @symbol{graphene:vec4-t} instance}
  @argument[epsilon]{a number coerced to a single float for the treshold
    between the two vectors}
  @return{@em{True} if the two vectors are near each other, and @em{false}
    otherwise.}
  @begin{short}
    Compares the two given vectors and checks whether their values are within
    the given @arg{epsilon}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec2_near"
                        (:pointer (:struct vec4-t)) v1
                        (:pointer (:struct vec4-t)) v2
                        :float (coerce epsilon 'single-float)
                        :bool))

(export 'vec4-near)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_min
;;; ----------------------------------------------------------------------------

(defun vec4-min (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the minimum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_min"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-min)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_max
;;; ----------------------------------------------------------------------------

(defun vec4-max (a b result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[a]{a @symbol{graphene:vec4-t} instance}
  @argument[b]{a @symbol{graphene:vec4-t} instance}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the result.}
  @begin{short}
    Compares the two given vectors and places the maximum values of each
    component into @arg{result}.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_max"
                        (:pointer (:struct vec4-t)) a
                        (:pointer (:struct vec4-t)) b
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-max)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_interpolate
;;; ----------------------------------------------------------------------------

(defun vec4-interpolate (v1 v2 factor result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v1]{a @symbol{graphene:vec4-t} instance}
  @argument[v2]{a @symbol{graphene:vec4-t} instance}
  @argument[factor]{a number coerced to a double float for the interpolation
    factor}
  @argument[result]{a @symbol{graphene:vec4-t} instance for the result}
  @return{The @symbol{graphene:vec4-t} instance with the interpolated vector.}
  @begin{short}
    Linearly interpolates @arg{v1} and @arg{v2} using the given factor.
  @end{short}
  @see-symbol{graphene:vec4-t}"
  (cffi:foreign-funcall "graphene_vec4_interpolate"
                        (:pointer (:struct vec4-t)) v1
                        (:pointer (:struct vec4-t)) v2
                        :double (coerce factor 'double-float)
                        (:pointer (:struct vec4-t)) result
                        :void)
  result)

(export 'vec4-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_get_x" vec4-x) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the value of the x component.}
  @short{Retrieves the x component of the vector.}
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-x)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_get_y" vec4-y) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the value of the y component.}
  @short{Retrieves the y component of the vector.}
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-y)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_z
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_get_z" vec4-z) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the value of the z component.}
  @short{Retrieves the z component of the vector.}
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-z)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_w
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_get_w" vec4-w) :float
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The single float with the value of the fourth component.}
  @short{Retrieves the fourth component of the vector.}
  @see-symbol{graphene:vec4-t}"
  (v (:pointer (:struct vec4-t))))

(export 'vec4-w)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_xy
;;; ----------------------------------------------------------------------------

(defun vec4-xy (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The @symbol{graphene:vec2-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec2-t} instance that contains the first two
    components of the given vector.
  @end{short}
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec2-t}"
  (cffi:foreign-funcall "graphene_vec4_get_xy"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec2-t)) result
                        :void)
  result)

(export 'vec4-xy)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_get_xyz
;;; ----------------------------------------------------------------------------

(defun vec4-xyz (v result)
 #+liber-documentation
 "@version{2025-4-5}
  @argument[v]{a @symbol{graphene:vec4-t} instance}
  @return{The @symbol{graphene:vec3-t} instance.}
  @begin{short}
    Returns a @symbol{graphene:vec3-t} instance that contains the first three
    components of the given vector.
  @end{short}
  @see-symbol{graphene:vec4-t}
  @see-symbol{graphene:vec3-t}"
  (cffi:foreign-funcall "graphene_vec4_get_xyz"
                        (:pointer (:struct vec4-t)) v
                        (:pointer (:struct vec3-t)) result
                        :void)
  result)

(export 'vec4-xyz)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_zero" vec4-zero) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @begin{return}
    The @symbol{graphene:vec4-t} instance with three components, all
    sets to 0.0.
  @end{return}
  @short{Retrieves a constant zero vector.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_one
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_one" vec4-one) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @begin{return}
    The @symbol{graphene:vec4-t} instance with three components, all
    sets to 1.0.
  @end{return}
  @short{Retrieves a constant one vector.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-one)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_x_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_x_axis" vec4-x-axis) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec4-t} instance with the X axis vector.}
  @short{Retrieves a constant vector with (1.0, 0.0, 0.0, 0.0) components.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-x-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_y_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_y_axis" vec4-y-axis) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec4-t} instance with the Y axis vector.}
  @short{Retrieves a constant vector with (0.0, 1.0, 0.0, 0.0) components.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-y-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_z_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_z_axis" vec4-z-axis) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec4-t} instance with the Z axis vector.}
  @short{Retrieves a constant vector with (0.0, 0.0, 1.0, 0.0) components.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-z-axis)

;;; ----------------------------------------------------------------------------
;;; graphene_vec4_w_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_vec4_w_axis" vec4-w-axis) (:pointer (:struct vec4-t))
 #+liber-documentation
 "@version{2025-4-5}
  @return{The @symbol{graphene:vec4-t} instance with the w axis vector.}
  @short{Retrieves a constant vector with (0.0, 0.0, 0.0, 1.0) components.}
  @see-symbol{graphene:vec4-t}")

(export 'vec4-w-axis)

;;; --- End of file graphene.vector.lisp ---------------------------------------
