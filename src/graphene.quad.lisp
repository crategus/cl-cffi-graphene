;;; ----------------------------------------------------------------------------
;;; graphene.quad.lisp
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
;;; Quad
;;;
;;;     Four-vertex quadrilateral
;;;
;;; Types and Values
;;;
;;;     graphene_quad_t
;;;
;;; Functions
;;;
;;;     graphene_quad_alloc
;;;     graphene_quad_free
;;;     graphene_quad_init
;;;     graphene_quad_init_from_rect
;;;     graphene_quad_init_from_points
;;;     graphene_quad_contains
;;;     graphene_quad_bounds
;;;     graphene_quad_get_point
;;;
;;; Description
;;;
;;;     A graphene_quad_t represents a coplanar, four vertex quadrilateral
;;;     shape.
;;; ----------------------------------------------------------------------------

(in-package :graphene)

;; TODO: The initialization from a list of points is not implemented.

(defmacro with-quad ((var &rest args) &body body)
 #+liber-documentation
 "@version{2023-11-20}
  @syntax[]{(graphene:with-quad (q) body) => result}
  @syntax[]{(graphene:with-quad (q r) body) => result}
  @syntax[]{(graphene:with-quad (q p1 p2 p3 p4) body) => result}
  @argument[q]{a @symbol{graphene:quad-t} instance to create and initialize}
  @argument[r]{a @symbol{graphene:rect-t} instance}
  @argument[p1]{a @symbol{graphene:point-t} instance with the first point}
  @argument[p2]{a @symbol{graphene:point-t} instance with the second point}
  @argument[p3]{a @symbol{graphene:point-t} instance with the third point}
  @argument[p4]{a @symbol{graphene:point-t} instance with the fourth point}
  @begin{short}
    The @fun{graphene:with-quad} macro allocates a new @symbol{graphene:quad-t}
    instance, initializes the quadrilateral with the given values and executes
    the body that uses the quadrilateral.
  @end{short}
  After execution of the body the allocated memory for the quadrilateral is
  released.

  When no argument is given the components of the quadrilateral are not
  initialized and undefined. The initialization with four points uses the
  @fun{graphene:quad-init} function. The initialization from a rectangle
  is done with the @fun{graphene:quad-init-from-rect} function.
  @begin[Note]{dictionary}
    The memory is allocated with the @fun{graphene:quad-alloc} function and
    released with the @fun{graphene:quad-free} function.
  @end{dictionary}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:point-t}
  @see-macro{graphene:with-quads}
  @see-function{graphene:quad-alloc}
  @see-function{graphene:quad-free}"
  (cond ((not args)
        ;; The default is no initialization
        `(let ((,var (quad-alloc)))
           (unwind-protect
             (progn ,@body)
             (quad-free ,var))))
        ((not (second args))
         ;; We have one argument. The argument must be of type rect-t
         (destructuring-bind (arg &optional type)
             (if (listp (first args)) (first args) (list (first args)))
           (cond ((or (not type)
                      (eq type 'rect-t))
                  ;; One argument with no type or of type rect-t
                  `(let ((,var (quad-alloc)))
                     (setf ,var (quad-init-from-rect ,var ,arg))
                     (unwind-protect
                       (progn ,@body)
                       (quad-free ,var)))))))
         ((not (fifth args))
          ;; We have a list with four arguments, use quad-init
          `(let ((,var (quad-alloc)))
             (setf ,var (quad-init ,var ,@args))
             (unwind-protect
               (progn ,@body)
               (quad-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-QUAD"))))

(export 'with-quad)

(defmacro with-quads (vars &body body)
 #+liber-documentation
 "@version{2023-11-20}
  @syntax[]{(graphene:with-quads (q1 q2 q3 ... qn) body) => result}
  @argument[q1 ... qn]{the newly created @symbol{graphene:quad-t} instances}
  @argument[body]{a body that uses the bindings @arg{q1 ... qn}}
  @begin{short}
    The @fun{graphene:with-quads} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each quadrilateral can be initialized with values using the syntax for the
  @fun{graphene:with-quad} macro. See also the
  @fun{graphene:with-quad} documentation.
  @see-symbol{graphene:quad-t}
  @see-macro{graphene:with-quad}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-quad ,var
           (with-quads ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-quads)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct quad-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'quad-t)
      "CStruct"
      (liber:symbol-documentation 'quad-t)
 "@version{2023-11-20}
  @begin{short}
    A 4 vertex quadrilateral, as represented by four @symbol{graphene:point-t}
    instances.
  @end{short}
  The contents of a @symbol{graphene:quad-t} instance are private and should
  never be accessed directly.
  @see-symbol{graphene:point-t}")

(export 'quad-t)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quad_alloc" quad-alloc)
    (:pointer (:struct quad-t))
 #+liber-documentation
 "@version{2023-11-20}
  @return{The newly allocated @symbol{graphene:quad-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:quad-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{graphene:quad-t}
  @see-function{graphene:quad-free}")

(export 'quad-alloc)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quad_free" quad-free) :void
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:quad-alloc} function.
  @end{short}
  @see-symbol{graphene:quad-t}
  @see-function{graphene:quad-alloc}"
  (q (:pointer (:struct quad-t))))

(export 'quad-free)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_init ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quad_init" quad-init) (:pointer (:struct quad-t))
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance}
  @argument[p1]{a @symbol{graphene:point-t} instance with the first point of
    the quadrilateral}
  @argument[p2]{a @symbol{graphene:point-t} instance with the second point of
    the quadrilateral}
  @argument[p3]{a @symbol{graphene:point-t} instance with the third point of
    the quadrilateral}
  @argument[p4]{a @symbol{graphene:point-t} instance with the fourth point of
    the quadrilateral}
  @return{The initialized @symbol{graphene:quad-t} instance.}
  @begin{short}
    Initializes a quadrilateral with the given points.
  @end{short}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:point-t}"
  (q (:pointer (:struct quad-t)))
  (p1 (:pointer (:struct point-t)))
  (p2 (:pointer (:struct point-t)))
  (p3 (:pointer (:struct point-t)))
  (p4 (:pointer (:struct point-t))))

(export 'quad-init)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_init_from_rect ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quad_init_from_rect" quad-init-from-rect)
    (:pointer (:struct quad-t))
 #+liber-documentation
 "@version{2020-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance to initialize}
  @argument[r]{a @symbol{graphene:rect-t} instance}
  @return{The initialized @symbol{graphene:quad-t} instance.}
  @begin{short}
    Initializes a quadrilateral using the four corners of the given rectangle.
  @end{short}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:rect-t}"
  (q (:pointer (:struct quad-t)))
  (r (:pointer (:struct rect-t))))

(export 'quad-init-from-rect)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_init_from_points ()
;;; ----------------------------------------------------------------------------

(defun quad-init-from-points (q values)
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance to initialize}
  @argument[values]{a list of 4 @symbol{graphene:point-t} instances}
  @return{The initialized @symbol{graphene:quad-t} instance.}
  @short{Initializes a quadrilateral using a list of points.}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:point-t}"
  (apply #'quad-init q values))

(export 'quad-init-from-points)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_contains ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_quad_contains" quad-contains) :bool
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance}
  @argument[p]{a @symbol{graphene:point-t} instance}
  @return{@em{True} if the point is inside the @symbol{graphene:quad-t}
    instance.}
  @short{Checks if the given quadrilateral contains the given point.}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:point-t}"
  (q (:pointer (:struct quad-t)))
  (p (:pointer (:struct quad-t))))

(export 'quad-contains)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_bounds ()
;;; ----------------------------------------------------------------------------

(defun quad-bounds (q result)
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the bounding rectangle.}
  @short{Computes the bounding rectangle of the quadrilateral.}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_quad_bounds"
                        (:pointer (:struct quad-t)) q
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'quad-bounds)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_get_point ()
;;; ----------------------------------------------------------------------------

;; TODO: Is this implementation correct?!

(cffi:defcfun ("graphene_quad_get_point" %quad-point)
    (:pointer (:struct point-t))
  (q (:pointer (:struct quad-t)))
  (i :int))

(defun quad-point (q index result)
 #+liber-documentation
 "@version{2023-11-20}
  @argument[q]{a @symbol{graphene:quad-t} instance}
  @argument[index]{an integer with the index of the point to retrieve}
  @argument[result]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance for the given @arg{index}.}
  @begin{short}
    Retrieves the point of a quadrilateral at the given index.
  @end{short}
  @begin{pre}
p0            p1
 + ---------- +
 |            |
 |            |
 |            |
 + ---------- +
p3            p2
  @end{pre}
  @see-symbol{graphene:quad-t}
  @see-symbol{graphene:point-t}"
  (point-init-from-point result (%quad-point q index)))

(export 'quad-point)

;;; --- End of file graphene.quad.lisp -----------------------------------------
