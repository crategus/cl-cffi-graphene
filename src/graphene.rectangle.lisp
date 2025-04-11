;;; ----------------------------------------------------------------------------
;;; graphene.rectangle.lisp
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
;;; Rectangle
;;;
;;;     Rectangular shape type
;;;
;;; Types and Values
;;;
;;;     graphene_rect_t
;;;
;;; Functions
;;;
;;;     graphene_rect_alloc
;;;     graphene_rect_free
;;;     graphene_rect_init
;;;     graphene_rect_init_from_rect
;;;     graphene_rect_equal
;;;     graphene_rect_normalize
;;;     graphene_rect_normalize_r
;;;     graphene_rect_get_center
;;;     graphene_rect_get_top_left
;;;     graphene_rect_get_top_right
;;;     graphene_rect_get_bottom_right
;;;     graphene_rect_get_bottom_left
;;;     graphene_rect_get_x
;;;     graphene_rect_get_y
;;;     graphene_rect_get_width
;;;     graphene_rect_get_height
;;;     graphene_rect_get_area
;;;     graphene_rect_get_vertices
;;;     graphene_rect_union
;;;     graphene_rect_intersection
;;;     graphene_rect_contains_point
;;;     graphene_rect_contains_rect
;;;     graphene_rect_offset
;;;     graphene_rect_offset_r
;;;     graphene_rect_inset
;;;     graphene_rect_inset_r
;;;     graphene_rect_round_to_pixel                       Deprecated 1.4
;;;     graphene_rect_round                                Deprecated 1.10
;;;     graphene_rect_round_extents
;;;     graphene_rect_expand
;;;     graphene_rect_interpolate
;;;     graphene_rect_zero
;;;     graphene_rect_scale
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-rect ((var &rest args) &body body)
 #+liber-documentation
 "@version{2025-4-3}
  @syntax{(graphene:with-rect (rect) body) => result}
  @syntax{(graphene:with-rect (rect src) body) => result}
  @syntax{(graphene:with-rect (rect x y width height) body) => result}
  @argument[rect]{a @symbol{graphene:rect-t} instance to create and initialize}
  @argument[src]{a @symbol{graphene:rect-t} instance to use for initialization}
  @argument[x]{a number coerced to a single float for the x coordinate}
  @argument[y]{a number coerced to a single float for the y coordinate}
  @argument[width]{a number coerced to a single float for the width}
  @argument[height]{a number coerced to a single float for the height}
  @begin{short}
    The @fun{graphene:with-rect} macro allocates a new @symbol{graphene:rect-t}
    instance, initializes the rectangle with the given values and executes the
    body that uses the rectangle.
  @end{short}
  After execution of the body the allocated memory for the rectangle is
  released.

  When no argument is given the components of the rectangle are initialized to
  zero. The initialization with four single floats uses the
  @fun{graphene:rect-init} function. The initialization from another rectangle
  is done with the @fun{graphene:rect-init-from-rect} function.
  @begin[Notes]{dictionary}
    The memory is allocated with the @fun{graphene:rect-alloc} function and
    released with the @fun{graphene:rect-free} function.
  @end{dictionary}
  @see-symbol{graphene:rect-t}
  @see-macro{graphene:with-rects}
  @see-function{graphene:rect-alloc}
  @see-function{graphene:rect-free}"
  (cond ((null args)
         ;; No arguments, the default is initialization with zeros
         `(let ((,var (rect-alloc)))
            (rect-init ,var 0.0 0.0 0.0 0.0)
            (unwind-protect
              (progn ,@body)
              (rect-free ,var))))
        ((null (second args))
         ;; One argument of type rect-t
         (dbind (arg1 &optional type1 &rest rest1) (mklist (first args))
           (declare (ignore rest1))
           (cond ((eq type1 'rect-t)
                  ;; One argument of type rect-t
                  `(let ((,var (rect-alloc)))
                     (rect-init-from-rect ,var ,arg1)
                     (unwind-protect
                       (progn ,@body)
                       (rect-free ,var))))
                 (t
                  ;; One argument with no type, default is rect-t
                  `(let ((,var (rect-alloc)))
                     (rect-init-from-rect ,var ,@args)
                     (unwind-protect
                       (progn ,@body)
                       (rect-free ,var)))))))
        ((null (fifth args))
         ;; Four arguments for the x, y, width, height values
         `(let ((,var (rect-alloc)))
            (rect-init ,var ,@args)
            (unwind-protect
              (progn ,@body)
              (rect-free ,var))))
        (t
         (error "Syntax error in GRAPHENE:WITH-RECT"))))

(export 'with-rect)

(defmacro with-rects (vars &body body)
 #+liber-documentation
 "@version{2025-3-8}
  @syntax{(graphene:with-rects (rect1 rect2 ... rectn) body) => result}
  @argument[rect1 ... rectn]{newly created @symbol{graphene:rect-t} instances}
  @argument[body]{a body that uses the bindings @arg{rect1 ... rectn}}
  @begin{short}
    The @fun{graphene:with-rects} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each rectangle can be initialized with values using the syntax for the
  @fun{graphene:with-rect} macro. See also the @fun{graphene:with-rect}
  documentation.
  @see-symbol{graphene:rect-t}
  @see-macro{graphene:with-rect}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-rect ,var
           (with-rects ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-rects)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct rect-t
  (origin (:struct point-t))
  (size (:struct size-t)))

#+liber-documentation
(setf (liber:alias-for-symbol 'rect-t)
      "CStruct"
      (liber:symbol-documentation 'rect-t)
 "@version{2025-3-8}
  @begin{declaration}
(cffi:defcstruct rect-t
  (origin (:struct point-t))
  (size (:struct size-t)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[origin]{The @symbol{graphene:point-t} instance for the coordinates
        for the origin of the rectangle.}
      @entry[size]{The @symbol{graphene:size-t} instance for the size of the
        rectangle.}
    @end{table}
  @end{values}
  @begin{short}
    The location and size of a rectangle region.
  @end{short}
  The width and height of a @symbol{graphene:rect-t} instance can be negative.
  For instance, a @symbol{graphene:rect-t} instance with an origin of
  @code{(0,0)} and a size of @code{(10,10)} is equivalent to a
  @symbol{graphene:rect-t} instance with an origin of @code{(10,10)} and a size
  of @code{(-10,-10)}.

  Application code can normalize rectangles using the
  @fun{graphene:rect-normalize} function. This function will ensure that the
  width and height of a rectangle are positive values. All functions taking a
  @symbol{graphene:rect-t} instance as an argument will internally operate on a
  normalized copy. All functions returning a @symbol{graphene:rect-t} instance
  will always return a normalized rectangle.
  @begin[Examples]{dictionary}
    A rectangle with a negative size is normalized internally:
    @begin{pre}
(graphene:with-rect (rect 10 10 -10 -10)
  (list (graphene:rect-x rect)
        (graphene:rect-y rect)
        (graphene:rect-width rect)
        (graphene:rect-height rect)))
=> (0.0 0.0 10.0 10.0)
    @end{pre}
  @end{dictionary}
  @see-slot{graphene:rect-origin}
  @see-slot{graphene:rect-size}
  @see-symbol{graphene:point-t}
  @see-symbol{graphene:size-t}
  @see-function{graphene:rect-normalize}")

(export 'rect-t)

;;; --- graphene:rect-origin ---------------------------------------------------

(defun rect-origin (rect)
  (cffi:foreign-slot-pointer rect '(:struct rect-t) 'origin))

#+liber-documentation
(setf (liber:alias-for-function 'rect-origin)
      "Accessor"
      (documentation 'rect-origin 'function)
 "@version{2025-3-8}
  @syntax{(graphene:rect-origin rect) => origin}
  @syntax{(setf (graphene:rect-origin rect) origin)}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[origin]{a @symbol{graphene:point-t} instance for the origin of
    the rectangle}
  @begin{short}
    Accessor of the @code{origin} slot of the @symbol{graphene:rect-t}
    structure.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}")

(export 'rect-origin)

;;; --- graphene:rect-size -----------------------------------------------------

(defun rect-size (rect)
  (cffi:foreign-slot-pointer rect '(:struct rect-t) 'size))

#+liber-documentation
(setf (liber:alias-for-function 'rect-size)
      "Accessor"
      (documentation 'rect-size 'function)
 "@version{2025-3-8}
  @syntax{(graphene:rect-size rect) => size}
  @syntax{(setf (graphene:rect-size rect) size)}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[size]{a @symbol{graphene:size-t} instance for the size of
    the rectangle}
  @begin{short}
    Accessor of the @code{size} slot of the @symbol{graphene:rect-t}
    structure.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:size-t}")

(export 'rect-size)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_alloc" rect-alloc) (:pointer (:struct rect-t))
 #+liber-documentation
 "@version{2025-3-8}
  @return{The new @symbol{graphene:rect-t} instance.}
  @begin{short}
    Allocates a new @symbol{graphene:rect-t} instance.
  @end{short}
  The contents of the returned rectangle are undefined.
 @see-symbol{graphene:rect-t}")

(export 'rect-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_free
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_free" rect-free) :void
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{graphene:rect-alloc} function.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-function{graphene:rect-alloc}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-free)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_init
;;; ----------------------------------------------------------------------------

(defun rect-init (rect x y width height)
 #+liber-documentation
 "@version{2025-4-3}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[x]{a number coerced to a single float for the x coordinate}
  @argument[y]{a number coerced to a single float for the y coordinate}
  @argument[width]{a number coerced to a single float for the width}
  @argument[height]{a number coerced to a single float for the height}
  @return{The initialized @symbol{graphene:rect-t} instance.}
  @begin{short}
    Initializes the given rectangle with the given values.
  @end{short}
  This function will implicitly normalize the rectangle before returning.
  @see-class{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_rect_init"
                        (:pointer (:struct rect-t)) rect
                        :float (coerce x 'single-float)
                        :float (coerce y 'single-float)
                        :float (coerce width 'single-float)
                        :float (coerce height 'single-float)
                        (:pointer (:struct rect-t))))

(export 'rect-init)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_init_from_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_init_from_rect" rect-init-from-rect)
    (:pointer (:struct rect-t))
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[src]{a @symbol{graphene:rect-t} instance}
  @return{The initialized @symbol{graphene:rect-t} instance.}
  @begin{short}
    Initializes @arg{rect} using the given @arg{src} rectangle.
  @end{short}
  This function will implicitly normalize the rectangle before returning.
  @see-class{graphene:rect-t}
  @see-function{graphene:rect-init}"
  (rect (:pointer (:struct rect-t)))
  (src (:pointer (:struct rect-t))))

(export 'rect-init-from-rect)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_equal" rect-equal) :bool
 #+liber-documentation
 "@version{2025-3-8}
  @argument[a]{a @symbol{graphene:rect-t} instance}
  @argument[b]{a @symbol{graphene:rect-t} instance}
  @return{@em{True} if the rectangles are equal.}
  @short{Checks whether the two given rectangle are equal.}
  @see-symbol{graphene:rect-t}"
  (a (:pointer (:struct rect-t)))
  (b (:pointer (:struct rect-t))))

(export 'rect-equal)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_normalize
;;; graphene_rect_normalize_r
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_normalize_r" %rect-normalize-r) :void
  (rect (:pointer (:struct rect-t)))
  (result (:pointer (:struct rect-t))))

(defun rect-normalize (rect result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance for the result}
  @return{The normalized @symbol{graphene:rect-t} instance.}
  @begin{short}
    Normalizes the passed rectangle.
  @end{short}
  This function ensures that the size of the rectangle is made of positive
  values, and that the origin is the top-left corner of the rectangle.
  @see-symbol{graphene:rect-t}"
  (%rect-normalize-r rect result)
  result)

(export 'rect-normalize)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_center
;;; ----------------------------------------------------------------------------

(defun rect-center (rect point)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance.}
  @begin{short}
    Retrieves the coordinates of the center of the given rectangle.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-rect (rect 0 0 10 20)
  (graphene:with-point (point)
    (graphene:rect-center rect point)
    (values (graphene:point-x point)
            (graphene:point-y point))))
=> 5.0
=> 10.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_get_center"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        :void)
  point)

(export 'rect-center)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_top_left
;;; ----------------------------------------------------------------------------

(defun rect-top-left (rect point)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance}
  @begin{short}
    Retrieves the coordinates of the top-left corner of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_get_top_left"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        :void)
  point)

(export 'rect-top-left)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_top_right
;;; ----------------------------------------------------------------------------

(defun rect-top-right (rect point)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance}
  @begin{short}
    Retrieves the coordinates of the top-right corner of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_get_top_right"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        :void)
  point)

(export 'rect-top-right)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_bottom_right
;;; ----------------------------------------------------------------------------

(defun rect-bottom-right (rect point)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance}
  @begin{short}
    Retrieves the coordinates of the bottom-right corner of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_get_bottom_right"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        :void)
  point)

(export 'rect-bottom-right)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_bottom_left
;;; ----------------------------------------------------------------------------

(defun rect-bottom-left (rect point)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{The @symbol{graphene:point-t} instance}
  @begin{short}
    Retrieves the coordinates of the bottom-left corner of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_get_bottom_left"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        :void)
  point)

(export 'rect-bottom-left)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_x" rect-x) :float
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @return{The single float with the normalized x coordinate of the rectangle.}
  @begin{short}
    Retrieves the normalized x coordinate of the origin of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-x)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_y" rect-y) :float
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @return{The single float with the normalized y coordinate of the rectangle.}
  @begin{short}
    Retrieves the normalized y coordinate of the origin of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-y)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_width" rect-width) :float
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @return{The single float with the normalized width of the rectangle.}
  @begin{short}
    Retrieves the normalized width of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-width)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_height" rect-height) :float
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @return{The single float with the normalized height of the rectangle.}
  @begin{short}
    Retrieves the normalized height of the given rectangle.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-height)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_area" rect-area) :float
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @return{The single float with the area of the given normalized rectangle.}
  @begin{short}
    Compute the area of the given normalized rectangle.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-rect (rect 0 0 2 3)
  (graphene:rect-area rect))
=> 6.0
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:rect-t}"
  (rect (:pointer (:struct rect-t))))

(export 'rect-area)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_get_vertices
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_get_vertices" %rect-vertices) :void
  (rect (:pointer (:struct rect-t)))
  (vertices :pointer))

(defun rect-vertices (rect vertices)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[vertices]{a list with 4 @symbol{graphene:vec2-t} instances}
  @return{The list with 4 @symbol{graphene:vec2-t} instances.}
  @short{Computes the four vertices of a rectangle.}
  @begin[Examples]{dictionary}
    @begin{pre}
(graphene:with-rect (rect 0 0 2 4)
  (graphene:with-vec2s (v1 v2 v3 v4)
    (mapcar #'graphene:vec2-to-float
            (graphene:rect-vertices rect (list v1 v2 v3 v4)))))
=> ((0.0 0.0) (2.0 0.0) (2.0 4.0) (0.0 4.0))
    @end{pre}
  @end{dictionary}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:vec2-t}"
  (cffi:with-foreign-object (vertices-ar '(:struct vec2-t) 4)
    (iter (for i from 0 below 4)
          (for vertex in vertices)
          (for ptr = (cffi:mem-aptr vertices-ar '(:struct vec2-t) i))
          (vec2-init-from-vec2 ptr vertex))
    (%rect-vertices rect vertices-ar)
    (iter (for i from 0 below 4)
          (for vertex in vertices)
          (for ptr = (cffi:mem-aptr vertices-ar '(:struct vec2-t) i))
          (vec2-init-from-vec2 vertex ptr)
          (collect vertex))))

(export 'rect-vertices)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_union
;;; ----------------------------------------------------------------------------

(defun rect-union (a b result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[a]{a @symbol{graphene:rect-t} instance}
  @argument[b]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the result.}
  @begin{short}
    Computes the union of the two given rectangles.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_rect_union"
                        (:pointer (:struct rect-t)) a
                        (:pointer (:struct rect-t)) b
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'rect-union)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_intersection
;;; ----------------------------------------------------------------------------

(defun rect-intersection (a b result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[a]{a @symbol{graphene:rect-t} instance}
  @argument[b]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @begin{return}
    The @symbol{graphene:rect-t} instance with the result, or @code{nil} if
    the rectangles do not intersect.
  @end{return}
  @begin{short}
    Computes the intersection of the two given rectangles.
  @end{short}
  If the two rectangles do not intersect, @arg{result} will contain a degenerate
  rectangle with origin in (0, 0) and a size of 0.
  @see-symbol{graphene:rect-t}"
  (when (cffi:foreign-funcall "graphene_rect_intersection"
                              (:pointer (:struct rect-t)) a
                              (:pointer (:struct rect-t)) b
                              (:pointer (:struct rect-t)) result
                              :bool)
    result))

(export 'rect-intersection)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_contains_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_contains_point" rect-contains-point) :bool
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @return{@em{True} if the rectangle contains the point.}
  @short{Checks whether a rectangle contains the given coordinates.}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (rect (:pointer (:struct rect-t)))
  (point (:pointer (:struct point-t))))

(export 'rect-contains-point)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_contains_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_contains_rect" rect-contains-rect) :bool
 #+liber-documentation
 "@version{2025-3-8}
  @argument[a]{a @symbol{graphene:rect-t} instance}
  @argument[b]{a @symbol{graphene:rect-t} instance}
  @return{@em{True} if the rectangle @arg{a} fully contains @arg{b}.}
  @short{Checks whether a rectangle contains the given rectangle.}
  @see-symbol{graphene:rect-t}"
  (a (:pointer (:struct rect-t)))
  (b (:pointer (:struct rect-t))))

(export 'rect-contains-rect)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_offset
;;; graphene_rect_offset_r
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_offset_r" %rect-offset-r) :void
  (rect (:pointer (:struct rect-t)))
  (dx :float)
  (dy :float)
  (result (:pointer (:struct rect-t))))

(defun rect-offset (rect dx dy result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[dx]{a number coerced to a single float for the horizontal offset}
  @argument[dy]{a number coerced to a single float for the vertical offset}
  @argument[result]{a @symbol{graphene:rect-t} instance for the result}
  @return{The @symbol{graphene:rect-t} instance with the offset rectangle.}
  @begin{short}
    Offsets the origin of the rectangle by @arg{dx} and @arg{dy}.
  @end{short}
  The size of the rectangle is unchanged.
  @see-symbol{graphene:rect-t}"
  (%rect-offset-r rect
                  (coerce dx 'single-float)
                  (coerce dy 'single-float)
                  result)
  result)

(export 'rect-offset)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_inset
;;; graphene_rect_inset_r
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_inset_r" %rect-inset-r) :void
  (rect (:pointer (:struct rect-t)))
  (dx :float)
  (dy :float)
  (result (:pointer (:struct rect-t))))

(defun rect-inset (rect dx dy result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[dx]{a number coerced to a single float for the horizontal inset}
  @argument[dy]{a number coerced to a single float for the vertical inset}
  @argument[result]{a @symbol{graphene:rect-t} instance for the result}
  @return{The @symbol{graphene:rect-t} instance with the inset rectangle.}
  @begin{short}
    Changes the given rectangle to be smaller, or larger depending on the given
    inset parameters.
  @end{short}
  To create an inset rectangle, use positive @arg{dx} or @arg{dy} values. To
  create a larger, encompassing rectangle, use negative @arg{dx} or @arg{dy}
  values.

  The origin of the rectangle is offset by @arg{dx} and @arg{dy}, while the size
  is adjusted by @arg{(2 * dx, 2 * dy)}. If @arg{dx} and @arg{dy} are positive
  values, the size of the rectangle is decreased. If @arg{dx} and @arg{dy} are
  negative values, the size of the rectangle is increased.

  If the size of the resulting inset rectangle has a negative width or height
  then the size will be set to zero.
  @see-symbol{graphene:rect-t}"
  (%rect-inset-r rect
                 (coerce dx 'single-float)
                 (coerce dy 'single-float)
                 result)
  result)

(export 'rect-inset)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_round_to_pixel                            Deprecated 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_rect_round                                     Deprecated 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_rect_round_extents
;;; ----------------------------------------------------------------------------

(defun rect-round-extents (rect result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @begin{return}
    The @symbol{graphene:rect-t} instance for the rectangle with rounded
    extents.
  @end{return}
  @begin{short}
    Rounds the origin of the given rectangle to its nearest integers recompute
    the size so that the rectangle is large enough to contain all the corners
    of the original rectangle.
  @end{short}

  This function is the equivalent of calling floor on the coordinates of the
  origin, and recomputing the size calling ceil on the bottom-right coordinates.

  If you want to be sure that the rounded rectangle completely covers the area
  that was covered by the original rectangle - that is, you want to cover the
  area including all its corners - this function will make sure that the size
  is recomputed taking into account the ceiling of the coordinates of the
  bottom-right corner. If the difference between the original coordinates and
  the coordinates of the rounded rectangle is greater than the difference
  between the original size and and the rounded size, then the move of the
  origin would not be compensated by a move in the anti-origin, leaving the
  corners of the original rectangle outside the rounded one.
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_rect_round_extents"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'rect-round-extents)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_expand
;;; ----------------------------------------------------------------------------

(defun rect-expand (rect point result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the expanded rectangle.}
  @short{Expands a rectangle to contain the given point.}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (cffi:foreign-funcall "graphene_rect_expand"
                        (:pointer (:struct rect-t)) rect
                        (:pointer (:struct point-t)) point
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'rect-expand)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_interpolate
;;; ----------------------------------------------------------------------------

(defun rect-interpolate (a b factor result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[a]{a @symbol{graphene:rect-t} instance}
  @argument[b]{a @symbol{graphene:rect-t} instance}
  @argument[factor]{a number coerced to a double float for the linear
    interpolation factor}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @begin{return}
    The @symbol{graphene:rect-t} instance with the interpolated rectangle.
  @end{return}
  @begin{short}
    Linearly interpolates the origin and size of the two given rectangles.
  @end{short}
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_rect_interpolate"
                        (:pointer (:struct rect-t)) a
                        (:pointer (:struct rect-t)) b
                        :double (coerce factor 'double-float)
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'rect-interpolate)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_zero
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_rect_zero" rect-zero) (:pointer (:struct rect-t))
 #+liber-documentation
 "@version{2025-3-8}
  @return{The fixed @symbol{graphene:rect-t} instance.}
  @begin{short}
    Returns a degenerate rectangle with origin fixed at @code{(0,0)} and a
    size of @code{(0,0)}.
  @end{short}
  @see-symbol{graphene:rect-t}")

(export 'rect-zero)

;;; ----------------------------------------------------------------------------
;;; graphene_rect_scale
;;; ----------------------------------------------------------------------------

(defun rect-scale (rect sh sv result)
 #+liber-documentation
 "@version{2025-3-8}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[sh]{a number coerced to a single float for the horizontal scale
    factor}
  @argument[sv]{a number coerced to a single float for the vertical scale
    factor}
  @argument[result]{a @symbol{graphene:rect-t} instance}
  @return{The @symbol{graphene:rect-t} instance with the scaled rectangle.}
  @begin{short}
    Scales the size and origin of a rectangle horizontaly by @arg{sh}, and
    vertically by @arg{sv}.
  @end{short}
  The returned rectangle is normalized.
  @see-symbol{graphene:rect-t}"
  (cffi:foreign-funcall "graphene_rect_scale"
                        (:pointer (:struct rect-t)) rect
                        :float (coerce sh 'single-float)
                        :float (coerce sv 'single-float)
                        (:pointer (:struct rect-t)) result
                        :void)
  result)

(export 'rect-scale)

;;; --- End of file graphene.rectangle.lisp ------------------------------------
