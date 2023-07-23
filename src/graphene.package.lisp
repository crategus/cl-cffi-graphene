;;; ----------------------------------------------------------------------------
;;; graphene.package.lisp
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

(defpackage :graphene
  (:use :common-lisp)
  (:import-from :cffi))

(in-package :graphene)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :graphene) t)
 "This is the API documentation of a Lisp binding to the Graphene library.
  @begin[Point]{section}
    A point with 2 coordinates.
    @about-symbol{point-t}
    @about-function{point-x}
    @about-function{point-y}
    @about-macro{with-graphene-point}
    @about-macro{with-graphene-points}
    @about-function{point-alloc}
    @about-function{point-free}
    @about-function{point-zero}
    @about-function{point-init}
    @about-function{point-init-from-point}
    @about-function{point-init-from-vec2}
    @about-function{point-to-vec2}
    @about-function{point-equal}
    @about-function{point-near}
    @about-function{point-distance}
    @about-function{point-interpolate}
  @end{section}
  @begin[Point3D]{section}
    A point with 3 coordinates.
    @about-symbol{point3d-t}
    @about-function{point3d-x}
    @about-function{point3d-y}
    @about-function{point3d-z}
    @about-macro{with-graphene-point3d}
    @about-macro{with-graphene-point3ds}
    @about-function{point3d-alloc}
    @about-function{point3d-free}
    @about-function{point3d-zero}
    @about-function{point3d-init}
    @about-function{point3d-init-from-point}
    @about-function{point3d-init-from-vec3}
    @about-function{point3d-to-vec3}
    @about-function{point3d-equal}
    @about-function{point3d-near}
    @about-function{point3d-distance}
    @about-function{point3d-interpolate}
    @about-function{point3d-scale}
    @about-function{point3d-cross}
    @about-function{point3d-dot}
    @about-function{point3d-length}
    @about-function{point3d-normalize}
    @about-function{point3d-normalize-viewport}
  @end{section}
  @begin[Size]{section}
    Size representation.
    @about-symbol{size-t}
    @about-function{size-width}
    @about-function{size-height}
    @about-macro{with-graphene-size}
    @about-macro{with-graphene-sizes}
    @about-function{size-alloc}
    @about-function{size-free}
    @about-function{size-zero}
    @about-function{size-init}
    @about-function{size-init-from-size}
    @about-function{size-equal}
    @about-function{size-interpolate}
    @about-function{size-scale}
  @end{section}
  @begin[Rectangle]{section}
    Rectangular shape type.
    @about-symbol{rect-t}
    @about-macro{with-graphene-rect}
    @about-macro{with-graphene-rects}
    @about-function{rect-origin}
    @about-function{rect-size}
    @about-function{rect-alloc}
    @about-function{rect-free}
    @about-function{rect-init}
    @about-function{rect-init-from-rect}
    @about-function{rect-equal}
    @about-function{rect-normalize}
    @about-function{rect-rect-normalize-r}
    @about-function{rect-center}
    @about-function{rect-top-left}
    @about-function{rect-top-right}
    @about-function{rect-bottom-right}
    @about-function{rect-bottom-left}
    @about-function{rect-x}
    @about-function{rect-y}
    @about-function{rect-width}
    @about-function{rect-height}
    @about-function{rect-area}
    @about-function{rect-vertices}
    @about-function{rect-union}
    @about-function{rect-intersection}
    @about-function{rect-contains-point}
    @about-function{rect-contains-rect}
    @about-function{rect-offset}
    @about-function{rect-offset-r}
    @about-function{rect-inset}
    @about-function{rect-inset-r}
    @about-function{rect-round-to-pixel}
    @about-function{rect-round}
    @about-function{rect-round-extents}
    @about-function{rect-expand}
    @about-function{rect-interpolate}
    @about-function{rect-zero}
    @about-function{rect-scale}
  @end{section}
  @begin[Quad]{section}
    Four-vertex quadrilateral.
    @about-symbol{quad-t}
    @about-macro{with-graphene-quad}
    @about-macro{with-graphene-quads}
    @about-function{quad-alloc}
    @about-function{quad-free}
    @about-function{quad-init}
    @about-function{quad-init-from-rect}
    @about-function{quad-init-from-points}
    @about-function{quad-contains}
    @about-function{quad-bounds}
    @about-function{quad-point}
  @end{section}
  @begin[Triangle]{section}
    A triangle described by 3D points.
    @about-symbol{triangle-t}
    @about-macro{with-graphene-triangle}
    @about-macro{with-graphene-triangles}
    @about-function{triangle-alloc}
    @about-function{triangle-free}
    @about-function{triangle-init-from-point3d}
    @about-function{triangle-init-from-vec3}
    @about-function{triangle-init-from-float}
    @about-function{triangle-points}
    @about-function{triangle-vertices}
    @about-function{triangle-area}
    @about-function{triangle-midpoint}
    @about-function{triangle-normal}
    @about-function{triangle-plane}
    @about-function{triangle-bounding-box}
    @about-function{triangle-barycoords}
    @about-function{triangle-uv}
    @about-function{triangle-contains-point}
    @about-function{triangle-equal}
  @end{section}
  @begin[Box]{section}
    Axis-aligned bounding box.
    @about-symbol{box-t}
    @about-macro{with-graphene-box}
    @about-macro{with-graphene-boxes}
    @about-function{box-alloc}
    @about-function{box-free}
    @about-function{box-init}
    @about-function{box-init-from-box}
    @about-function{box-init-from-points}
    @about-function{box-init-from-vec3}
    @about-function{box-init-from-vectors}
    @about-function{box-equal}
    @about-function{box-expand}
    @about-function{box-expand-scalar}
    @about-function{box-expand-vec3}
    @about-function{box-min}
    @about-function{box-max}
    @about-function{box-center}
    @about-function{box-depth}
    @about-function{box-height}
    @about-function{box-width}
    @about-function{box-size}
    @about-function{box-bounding-sphere}
    @about-function{box-vertices}
    @about-function{box-union}
    @about-function{box-intersection}
    @about-function{box-contains-box}
    @about-function{box-contains-point}
    @about-function{box-zero}
    @about-function{box-one}
    @about-function{box-minus-one}
    @about-function{box-one-minus-one}
    @about-function{box-empty}
    @about-function{box-infinite}
  @end{section}
  @begin[Sphere]{section}
    A sphere.
    @about-symbol{sphere-t}
    @about-macro{with-graphene-sphere}
    @about-macro{with-graphene-spheres}
    @about-function{sphere-alloc}
    @about-function{sphere-free}
    @about-function{sphere-init}
    @about-function{sphere-init-from-points}
    @about-function{sphere-init-from-vectors}
    @about-function{sphere-center}
    @about-function{sphere-radius}
    @about-function{sphere-bounding-box}
    @about-function{sphere-is-empty}
    @about-function{sphere-distance}
    @about-function{sphere-contains-point}
    @about-function{sphere-translate}
    @about-function{sphere-equal}
  @end{section}
  @begin[Frustum]{section}
    A 3D field of view.
    @about-symbol{frustum-t}
    @about-macro{with-graphene-frustum}
    @about-macro{with-graphene-frustums}
    @about-function{frustum-alloc}
    @about-function{frustum-free}
    @about-function{frustum-init}
    @about-function{frustum-init-from-frustum}
    @about-function{frustum-init-from-matrix}
    @about-function{frustum-planes}
    @about-function{frustum-contains-point}
    @about-function{frustum-intersects-sphere}
    @about-function{frustum-intersects-box}
    @about-function{frustum-equal}
  @end{section}
  @begin[Vector]{section}
    Vectors in 2, 3, and 4 dimensions.
    @about-variable{+vec2-len+}
    @about-variable{+vec3-len+}
    @about-variable{+vec4-len+}
    @about-symbol{vec2-t}
    @about-macro{with-graphene-vec2}
    @about-macro{with-graphene-vec2s}
    @about-function{vec2-alloc}
    @about-function{vec2-free}
    @about-function{vec2-init}
    @about-function{vec2-init-from-vec2}
    @about-function{vec2-init-from-float}
    @about-function{vec2-to-float}
    @about-function{vec2-add}
    @about-function{vec2-subtract}
    @about-function{vec2-multiply}
    @about-function{vec2-divide}
    @about-function{vec2-dot}
    @about-function{vec2-scale}
    @about-function{vec2-length}
    @about-function{vec2-normalize}
    @about-function{vec2-negate}
    @about-function{vec2-equal}
    @about-function{vec2-near}
    @about-function{vec2-min}
    @about-function{vec2-max}
    @about-function{vec2-interpolate}
    @about-function{vec2-x}
    @about-function{vec2-y}
    @about-function{vec2-zero}
    @about-function{vec2-one}
    @about-function{vec2-x-axis}
    @about-function{vec2-y-axis}
    @about-symbol{vec3-t}
    @about-macro{with-graphene-vec3}
    @about-macro{with-graphene-vec3s}
    @about-function{vec3-alloc}
    @about-function{vec3-free}
    @about-function{vec3-init}
    @about-function{vec3-init-from-vec3}
    @about-function{vec3-init-from-float}
    @about-function{vec3-to-float}
    @about-function{vec3-add}
    @about-function{vec3-subtract}
    @about-function{vec3-multiply}
    @about-function{vec3-divide}
    @about-function{vec3-cross}
    @about-function{vec3-dot}
    @about-function{vec3-scale}
    @about-function{vec3-length}
    @about-function{vec3-normalize}
    @about-function{vec3-negate}
    @about-function{vec3-equal}
    @about-function{vec3-near}
    @about-function{vec3-min}
    @about-function{vec3-max}
    @about-function{vec3-interpolate}
    @about-function{vec3-x}
    @about-function{vec3-y}
    @about-function{vec3-z}
    @about-function{vec3-xy}
    @about-function{vec3-xy0}
    @about-function{vec3-xyz0}
    @about-function{vec3-xyz1}
    @about-function{vec3-xyzw}
    @about-function{vec3-zero}
    @about-function{vec3-one}
    @about-function{vec3-x-axis}
    @about-function{vec3-y-axis}
    @about-function{vec3-z-axis}
    @about-symbol{vec4-t}
    @about-macro{with-graphene-vec4}
    @about-macro{with-graphene-vec4s}
    @about-function{vec4-alloc}
    @about-function{vec4-free}
    @about-function{vec4-init}
    @about-function{vec4-init-from-vec4}
    @about-function{vec4-init-from-vec3}
    @about-function{vec4-init-from-vec2}
    @about-function{vec4-init-from-float}
    @about-function{vec4-to-float}
    @about-function{vec4-add}
    @about-function{vec4-subtract}
    @about-function{vec4-multiply}
    @about-function{vec4-divide}
    @about-function{vec4-dot}
    @about-function{vec4-scale}
    @about-function{vec4-length}
    @about-function{vec4-normalize}
    @about-function{vec4-negate}
    @about-function{vec4-equal}
    @about-function{vec4-near}
    @about-function{vec4-min}
    @about-function{vec4-max}
    @about-function{vec4-interpolate}
    @about-function{vec4-x}
    @about-function{vec4-y}
    @about-function{vec4-z}
    @about-function{vec4-w}
    @about-function{vec4-xy}
    @about-function{vec4-xyz}
    @about-function{vec4-zero}
    @about-function{vec4-one}
    @about-function{vec4-x-axis}
    @about-function{vec4-y-axis}
    @about-function{vec4-z-axis}
    @about-function{vec4-w-axis}
  @end{section}
  @begin[Matrix]{section}
    4x4 matrices.
    @about-symbol{matrix-t}
    @about-macro{with-graphene-matrix}
    @about-macro{with-graphene-matrices}
    @about-function{matrix-alloc}
    @about-function{matrix-free}
    @about-function{matrix-init-identity}
    @about-function{matrix-init-from-float}
    @about-function{matrix-init-from-vec4}
    @about-function{matrix-init-from-matrix}
    @about-function{matrix-init-from-2d}
    @about-function{matrix-init-perspective}
    @about-function{matrix-init-ortho}
    @about-function{matrix-init-look-at}
    @about-function{matrix-init-frustum}
    @about-function{matrix-init-scale}
    @about-function{matrix-init-translate}
    @about-function{matrix-init-rotate}
    @about-function{matrix-init-skew}
    @about-function{matrix-is-identity}
    @about-function{matrix-is-2d}
    @about-function{matrix-is-backface-visible}
    @about-function{matrix-is-singular}
    @about-function{matrix-to-float}
    @about-function{matrix-to-2d}
    @about-function{matrix-row}
    @about-function{matrix-value}
    @about-function{matrix-multiply}
    @about-function{matrix-determinant}
    @about-function{matrix-transform-vec4}
    @about-function{matrix-transform-vec3}
    @about-function{matrix-transform-point}
    @about-function{matrix-transform-point3d}
    @about-function{matrix-transform-rect}
    @about-function{matrix-transform-bounds}
    @about-function{matrix-transform-box}
    @about-function{matrix-transform-sphere}
    @about-function{matrix-transform-ray}
    @about-function{matrix-project-point}
    @about-function{matrix-project-rect-bounds}
    @about-function{matrix-project-rect}
    @about-function{matrix-untransform-point}
    @about-function{matrix-untransform-bounds}
    @about-function{matrix-unproject-point3d}
    @about-function{matrix-translate}
    @about-function{matrix-rotate}
    @about-function{matrix-rotate-x}
    @about-function{matrix-rotate-y}
    @about-function{matrix-rotate-z}
    @about-function{matrix-rotate-quaternion}
    @about-function{matrix-rotate-euler}
    @about-function{matrix-scale}
    @about-function{matrix-skew-xy}
    @about-function{matrix-skew-xz}
    @about-function{matrix-skew-yz}
    @about-function{matrix-transpose}
    @about-function{matrix-inverse}
    @about-function{matrix-perspective}
    @about-function{matrix-normalize}
    @about-function{matrix-x-translation}
    @about-function{matrix-y-translation}
    @about-function{matrix-z-translation}
    @about-function{matrix-x-scale}
    @about-function{matrix-y-scale}
    @about-function{matrix-z-scale}
    @about-function{matrix-decompose}
    @about-function{matrix-interpolate}
    @about-function{matrix-equal}
    @about-function{matrix-equal-fast}
    @about-function{matrix-near}
    @about-function{matrix-print}
  @end{section}
  @begin[Euler]{section}
    Euler angles.
    @about-symbol{euler-order-t}
    @about-symbol{euler-t}
    @about-macro{with-graphene-euler}
    @about-macro{with-graphene-eulers}
    @about-function{euler-alloc}
    @about-function{euler-free}
    @about-function{euler-init}
    @about-function{euler-init-with-order}
    @about-function{euler-init-from-matrix}
    @about-function{euler-init-from-quaternion}
    @about-function{euler-init-from-vec3}
    @about-function{euler-init-from-euler}
    @about-function{euler-init-from-radians}
    @about-function{euler-equal}
    @about-function{euler-x}
    @about-function{euler-y}
    @about-function{euler-z}
    @about-function{euler-order}
    @about-function{euler-alpha}
    @about-function{euler-beta}
    @about-function{euler-gamma}
    @about-function{euler-to-vec3}
    @about-function{euler-to-matrix}
    @about-function{euler-to-quaternion}
    @about-function{euler-reorder}
  @end{section}
  @begin[Quaternion]{section}
    Quaternion operations.
    @about-symbol{quaternion-t}
    @about-macro{with-graphene-quaternion}
    @about-macro{with-graphene-quaternions}
    @about-function{quaternion-alloc}
    @about-function{quaternion-free}
    @about-function{quaternion-init}
    @about-function{quaternion-init-identity}
    @about-function{quaternion-init-from-quaternion}
    @about-function{quaternion-init-from-vec4}
    @about-function{quaternion-init-from-matrix}
    @about-function{quaternion-init-from-angles}
    @about-function{quaternion-init-from-radians}
    @about-function{quaternion-init-from-angle-vec3}
    @about-function{quaternion-init-from-euler}
    @about-function{quaternion-to-vec4}
    @about-function{quaternion-to-matrix}
    @about-function{quaternion-to-angles}
    @about-function{quaternion-to-radians}
    @about-function{quaternion-to-angle-vec3}
    @about-function{quaternion-equal}
    @about-function{quaternion-dot}
    @about-function{quaternion-invert}
    @about-function{quaternion-normalize}
    @about-function{quaternion-add}
    @about-function{quaternion-multiply}
    @about-function{quaternion-scale}
    @about-function{quaternion-slerp}
  @end{section}
  @begin[Plane]{section}
    A plane in 3D space.
    @about-symbol{plane-t}
    @about-macro{with-graphene-plane}
    @about-macro{with-graphene-planes}
    @about-function{plane-alloc}
    @about-function{plane-free}
    @about-function{plane-init}
    @about-function{plane-init-from-vec4}
    @about-function{plane-init-from-plane}
    @about-function{plane-init-from-point}
    @about-function{plane-init-from-points}
    @about-function{plane-normalize}
    @about-function{plane-negate}
    @about-function{plane-equal}
    @about-function{plane-distance}
    @about-function{plane-transform}
    @about-function{plane-normal}
    @about-function{plane-constant}
  @end{section}
  @begin[Ray]{section}
    A ray emitted from an origin in a given direction.
    @about-symbol{ray-intersection-kind-t}
    @about-symbol{ray-t}
    @about-macro{with-graphene-ray}
    @about-macro{with-graphene-rays}
    @about-function{ray-alloc}
    @about-function{ray-free}
    @about-function{ray-init}
    @about-function{ray-init-from-ray}
    @about-function{ray-init-from-vec3}
    @about-function{ray-origin}
    @about-function{ray-direction}
    @about-function{ray-position-at}
    @about-function{ray-distance-to-point}
    @about-function{ray-distance-to-plane}
    @about-function{ray-closest-point-to-point}
    @about-function{ray-equal}
    @about-function{ray-intersect-sphere}
    @about-function{ray-intersects-sphere}
    @about-function{ray-intersect-box}
    @about-function{ray-intersects-box}
    @about-function{ray-intersect-triangle}
    @about-function{ray-intersects-triangle}
  @end{section}
  ")
;;; --- End of file graphene.package.lisp --------------------------------------
