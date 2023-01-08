;;; ----------------------------------------------------------------------------
;;; graphene.quad.lisp
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

(defmacro with-graphene-quad ((var &rest args) &body body)
  (if args
      (if (second args)
          ;; We have a list with more than one argument, use quad-init
          (progn
            `(let ((,var (quad-alloc)))
               (setf ,var (quad-init ,var ,@args))
               (unwind-protect
                 (progn ,@body)
                 (quad-free ,var))))
            (destructuring-bind (arg &optional type)
                (if (listp (first args)) (first args) (list (first args)))
              (declare (ignorable type))
              (cond (t
                     ;; The default is an argument of type rect-t
                     `(let ((,var (quad-alloc)))
                        (setf ,var (quad-init-from-rect ,var ,arg))
                        (unwind-protect
                          (progn ,@body)
                          (quad-free ,var)))))))
      (progn
        ;; The default is no initialization
        `(let ((,var (quad-alloc)))
           (unwind-protect
             (progn ,@body)
             (quad-free ,var))))))

(export 'with-graphene-quad)

(defmacro with-graphene-quads (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-quad ,var
           (with-graphene-quads ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-quads)

;;; ----------------------------------------------------------------------------
;;; graphene_quad_t
;;;
;;; typedef struct {
;;; } graphene_quad_t;
;;;
;;; A 4 vertex quadrilateral, as represented by four graphene_point_t.
;;;
;;; The contents of a graphene_quad_t are private and should never be accessed
;;; directly.
;;;
;;; Since 1.0
;;; ----------------------------------------------------------------------------

(defcstruct quad-t)

(export 'quad-t)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_alloc" quad-alloc)
    (:pointer (:struct quad-t))
 #+liber-documentation
 "@version{#2022-9-20}
  @return{The newly allocated @symbol{quad-t} instance.}
  @begin{short}
    Allocates a new @symbol{quad-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{quad-t}
  @see-function{quad-free}")

(export 'quad-alloc)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_free" quad-free) :void
 #+liber-documentation
 "@version{#2022-9-20}
  @argument[q]{a @symbol{quad-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{quad-alloc} function.
  @end{short}
  @see-symbol{quad-t}
  @see-function{quad-alloc}"
  (q (:pointer (:struct quad-t))))

(export 'quad-free)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_init ()
;;;graphene_quad_t *
;;;graphene_quad_init (graphene_quad_t *q,
;;;                    const graphene_point_t *p1,
;;;                    const graphene_point_t *p2,
;;;                    const graphene_point_t *p3,
;;;                    const graphene_point_t *p4);
;;;
;;; Initializes a graphene_quad_t with the given points.

;;;Parameters
;;;q

;;;the graphene_quad_t to initialize

;;;p1

;;;the first point of the quadrilateral

;;;p2

;;;the second point of the quadrilateral

;;;p3

;;;the third point of the quadrilateral

;;;p4

;;;the fourth point of the quadrilateral

;;;Returns
;;;the initialized graphene_quad_t.

;;;[transfer none]

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_init" quad-init) (:pointer (:struct quad-t))
  (q (:pointer (:struct quad-t)))
  (p1 (:pointer (:struct point-t)))
  (p2 (:pointer (:struct point-t)))
  (p3 (:pointer (:struct point-t)))
  (p4 (:pointer (:struct point-t))))

(export 'quad-init)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_init_from_rect ()
;;;graphene_quad_t *
;;;graphene_quad_init_from_rect (graphene_quad_t *q,
;;;                              const graphene_rect_t *r);
;;;
;;;Initializes a graphene_quad_t using the four corners of the given graphene_rect_t.

;;;Parameters
;;;q

;;;the graphene_quad_t to initialize

;;;r

;;;a graphene_rect_t

;;;Returns
;;;the initialized graphene_quad_t.

;;;[transfer none]

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_init_from_rect" quad-init-from-rect)
    (:pointer (:struct quad-t))
  (q (:pointer (:struct quad-t)))
  (r (:pointer (:struct rect-t))))

(export 'quad-init-from-rect)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_init_from_points ()
;;;graphene_quad_t *
;;;graphene_quad_init_from_points (graphene_quad_t *q,
;;;                                const graphene_point_t points[]);
;;;Initializes a graphene_quad_t using an array of points.

;;;Parameters
;;;q

;;;the graphene_quad_t to initialize

;;;points

;;;an array of 4 graphene_point_t.

;;;Returns
;;;the initialized graphene_quad_t.

;;;[transfer none]

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

(defun quad-init-from-points (q values)
  (apply #'quad-init q values))

(export 'quad-init-from-points)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_contains ()
;;;bool
;;;graphene_quad_contains (const graphene_quad_t *q,
;;;                        const graphene_point_t *p);
;;;Checks if the given graphene_quad_t contains the given graphene_point_t.

;;;Parameters
;;;q

;;;a graphene_quad_t

;;;p

;;;a graphene_point_t

;;;Returns
;;;true if the point is inside the graphene_quad_t

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_contains" quad-contains) :bool
  (q (:pointer (:struct quad-t)))
  (p (:pointer (:struct quad-t))))

(export 'quad-contains)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_bounds ()
;;;void
;;;graphene_quad_bounds (const graphene_quad_t *q,
;;;                      graphene_rect_t *r);
;;;Computes the bounding rectangle of q and places it into r .

;;;Parameters
;;;q

;;;a graphene_quad_t

;;;r

;;;return location for a graphene_rect_t.

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defun quad-bounds (q result)
  (foreign-funcall "graphene_quad_bounds"
                   (:pointer (:struct quad-t)) q
                   (:pointer (:struct rect-t)) result
                   :void)
  result)

(export 'quad-bounds)

;;; ----------------------------------------------------------------------------
;;;graphene_quad_get_point ()
;;;const graphene_point_t *
;;;graphene_quad_get_point (const graphene_quad_t *q,
;;;                         unsigned int index_);
;;;Retrieves the point of a graphene_quad_t at the given index.

;;;    p0            p1
;;;     + ---------- +
;;;     |            |
;;;     |            |
;;;     |            |
;;;     + ---------- +
;;;    p3            p2


;;;Parameters
;;;q

;;;a graphene_quad_t

;;;index_

;;;the index of the point to retrieve

;;;Returns
;;;a graphene_point_t.

;;;[transfer none]

;;;Since: 1.0
;;; ----------------------------------------------------------------------------

(defcfun ("graphene_quad_get_point" %quad-point)
    (:pointer (:struct point-t))
  (q (:pointer (:struct quad-t)))
  (i :int))

(defun quad-point (q index result)
  (point-init-from-point result (%quad-point q index)))

(export 'quad-point)

;;; --- End of file graphene.quad.lisp -----------------------------------------
