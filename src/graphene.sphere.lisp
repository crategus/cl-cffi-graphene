;;; ----------------------------------------------------------------------------
;;; graphene.sphere.lisp
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
;;; Sphere
;;;
;;;     A sphere
;;;
;;; Types and Values
;;;
;;;     graphene_sphere_t
;;;
;;; Functions
;;;
;;;     graphene_sphere_alloc
;;;     graphene_sphere_free
;;;     graphene_sphere_init
;;;     graphene_sphere_init_from_points
;;;     graphene_sphere_init_from_vectors
;;;     graphene_sphere_get_center
;;;     graphene_sphere_get_radius
;;;     graphene_sphere_get_bounding_box
;;;     graphene_sphere_is_empty
;;;     graphene_sphere_distance
;;;     graphene_sphere_contains_point
;;;     graphene_sphere_translate
;;;     graphene_sphere_equal
;;;
;;; Description
;;;
;;;     graphene_sphere_t provides a representation of a sphere using its
;;;     center and radius.
;;; ----------------------------------------------------------------------------

(in-package :graphene)

(defmacro with-graphene-sphere ((var &rest args) &body body)
  (cond ((not args)
         ;; We have no arguments, the default is initialization with zeros.
         `(let ((,var (sphere-alloc)))
            (sphere-init ,var (point3d-zero) 0.0)
            (unwind-protect
              (progn ,@body)
              (sphere-free ,var))))
        ((not (second args))
         ;; We have one argument. We have no method for this case.
         (error "Type error in WITH-GRAPHENE-SPHERE"))
        ((not (third args))
         ;; We have two arguments. The first must be of type point3d-t and
         ;; the second a single float.
         ;; TODO: Combine the two destructuring-bind calls to onw call.
         (destructuring-bind (arg1 &optional type1)
             (if (listp (first args)) (first args) (list (first args)))
           (destructuring-bind (arg2 &optional type2)
               (if (listp (second args)) (second args) (list (second args)))
             (cond ((and (or (not type1)
                             (eq type1 'point3d-t))
                         (not type2))
                    ;; First argument with no type or of type point3d-t and
                    ;; second argument with no type.
                    `(let ((,var (sphere-alloc)))
                       (sphere-init ,var ,arg1 ,arg2)
                       (unwind-protect
                         (progn ,@body)
                         (sphere-free ,var))))
                   (t
                    (error "Type error in WITH-GRAPHENE-SPHERE"))))))
        (t
         (error "Syntax error in WITH-GRAPHENE-SPHERE"))))

(export 'with-graphene-sphere)

(defmacro with-graphene-spheres (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-sphere ,var
           (with-graphene-spheres ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-spheres)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_t
;;;
;;; typedef struct {
;;; } graphene_sphere_t;
;;;
;;; A sphere, represented by its center and radius.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

(cffi:defcstruct sphere-t)

(export 'sphere-t)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_alloc ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_alloc" sphere-alloc) 
    (:pointer (:struct sphere-t))
 #+liber-documentation
 "@version{#2022-9-24}
  @return{The newly allocated @symbol{sphere-t} instance. Use the
    @fun{sphere-free} function to free the resources allocated by this
    function.}
  @begin{short}
    Allocates a new @symbol{sphere-t} instance.
  @end{short}
  The contents of the returned instance are undefined.
  @see-symbol{sphere-t}
  @see-function{sphere-free}")

(export 'sphere-alloc)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_free" sphere-free) :void
 #+liber-documentation
 "@version{#2022-9-24}
  @argument[sphere]{a @symbol{sphere-t} instance}
  @begin{short}
    Frees the resources allocated by the @fun{sphere-alloc} function.
  @end{short}
  @see-symbol{sphere-t}
  @see-function{sphere-alloc}"
  (sphere (:pointer (:struct sphere-t))))

(export 'sphere-free)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init ()
;;; ----------------------------------------------------------------------------

(defun sphere-init (sphere center radius)
 #+liber-documentation
 "@version{#2022-9-24}
  @argument[sphere]{a @symbol{sphere-t} instance to initialize}
  @argument[center]{a @symbol{point3d-t} instance with the coordinates of the
    center of the sphere}
  @argument[radius]{a single float the radius of the sphere}
  @return{The initialized @symbol{sphere-t} instance.}
  @begin{short}
    Initializes the given @symbol{sphere-t} instance with the given center and
    radius.
  @end{short}
  @see-symbol{sphere-t}
  @see-symbol{point3d-t}"
  (cffi:foreign-funcall "graphene_sphere_init"
                        (:pointer (:struct sphere-t)) sphere
                        (:pointer (:struct point3d-t)) center
                        :float (coerce radius 'single-float)
                        (:pointer (:struct sphere-t))))

(export 'sphere-init)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init_from_points ()
;;;
;;; graphene_sphere_t *
;;; graphene_sphere_init_from_points (graphene_sphere_t *s,
;;;                                   unsigned int n_points,
;;;                                   const graphene_point3d_t *points,
;;;                                   const graphene_point3d_t *center);
;;;
;;; Initializes the given graphene_sphere_t using the given array of 3D
;;; coordinates so that the sphere includes them.
;;;
;;; The center of the sphere can either be specified, or will be center of the
;;; 3D volume that encompasses all points .
;;;
;;; s :
;;;     the graphene_sphere_t to initialize
;;;
;;; n_points :
;;;     the number of graphene_point3d_t in the points array
;;;
;;; points :
;;;     an array of graphene_point3d_t.
;;;
;;; center :
;;;     the center of the sphere.
;;;
;;; Returns :
;;;     the initialized graphene_sphere_t.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_init_from_vectors ()
;;;
;;; graphene_sphere_t *
;;; graphene_sphere_init_from_vectors (graphene_sphere_t *s,
;;;                                    unsigned int n_vectors,
;;;                                    const graphene_vec3_t *vectors,
;;;                                    const graphene_point3d_t *center);
;;;
;;; Initializes the given graphene_sphere_t using the given array of 3D
;;; coordinates so that the sphere includes them.
;;;
;;; The center of the sphere can either be specified, or will be center of the
;;; 3D volume that encompasses all vectors .
;;;
;;; s :
;;;     the graphene_sphere_t to initialize
;;;
;;; n_vectors :
;;;     the number of graphene_vec3_t in the vectors array
;;;
;;; vectors :
;;;     an array of graphene_vec3_t.
;;;
;;; center :
;;;     the center of the sphere.
;;;
;;; Returns :
;;;     the initialized graphene_sphere_t.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_get_center ()
;;;
;;;void
;;;graphene_sphere_get_center (const graphene_sphere_t *s,
;;;                            graphene_point3d_t *center);
;;;Retrieves the coordinates of the center of a graphene_sphere_t.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;center

;;;return location for the coordinates of the center.
;;; ----------------------------------------------------------------------------

(defun sphere-center (sphere center)
  (cffi:foreign-funcall "graphene_sphere_get_center"
                        (:pointer (:struct sphere-t)) sphere
                        (:pointer (:struct point3d-t)) center
                        :void)
  center)

(export 'sphere-center)

;;; ----------------------------------------------------------------------------
;;; graphene_sphere_get_radius ()
;;;
;;;float
;;;graphene_sphere_get_radius (const graphene_sphere_t *s);
;;;Retrieves the radius of a graphene_sphere_t.

;;;Parameters
;;;s

;;;a graphene_sphere_t
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("graphene_sphere_get_radius" sphere-radius) :float
  (sphere (:pointer (:struct sphere-t))))

(export 'sphere-radius)

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_get_bounding_box ()
;;;void
;;;graphene_sphere_get_bounding_box (const graphene_sphere_t *s,
;;;                                  graphene_box_t *box);
;;;Computes the bounding box capable of containing the given graphene_sphere_t.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;box

;;;return location for the bounding box.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_is_empty ()
;;;bool
;;;graphene_sphere_is_empty (const graphene_sphere_t *s);
;;;Checks whether the sphere has a zero radius.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;Returns
;;;true if the sphere is empty

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_distance ()
;;;float
;;;graphene_sphere_distance (const graphene_sphere_t *s,
;;;                          const graphene_point3d_t *point);
;;;Computes the distance of the given point from the surface of a graphene_sphere_t.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;point

;;;a graphene_point3d_t

;;;Returns
;;;the distance of the point

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_contains_point ()
;;;bool
;;;graphene_sphere_contains_point (const graphene_sphere_t *s,
;;;                                const graphene_point3d_t *point);
;;;Checks whether the given point is contained in the volume of a graphene_sphere_t.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;point

;;;a graphene_point3d_t

;;;Returns
;;;true if the sphere contains the point

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_translate ()
;;;void
;;;graphene_sphere_translate (const graphene_sphere_t *s,
;;;                           const graphene_point3d_t *point,
;;;                           graphene_sphere_t *res);
;;;Translates the center of the given graphene_sphere_t using the point coordinates as the delta of the translation.

;;;Parameters
;;;s

;;;a graphene_sphere_t

;;;point

;;;the coordinates of the translation

;;;res

;;;return location for the translated sphere.

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;graphene_sphere_equal ()
;;;bool
;;;graphene_sphere_equal (const graphene_sphere_t *a,
;;;                       const graphene_sphere_t *b);
;;;Checks whether two graphene_sphere_t are equal.

;;;Parameters
;;;a

;;;a graphene_sphere_t

;;;b

;;;a graphene_sphere_t

;;;Returns
;;;true if the spheres are equal

;;;Since: 1.2
;;; ----------------------------------------------------------------------------

;;; --- End of file graphene.sphere.lisp ---------------------------------------
