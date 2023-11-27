;;; ----------------------------------------------------------------------------
;;; graphene.utils.lisp
;;;
;;; The documentation of this file is taken from the GRAPHENE Reference Manual
;;; and modified to document the Lisp binding to the Graphene library. See
;;; <https://ebassi.github.io/graphene/docs/>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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

(in-package :graphene)

(defmacro with-graphene-object ((var type &rest args) &body body)
  (cond ((eq 'box-t type)
         `(with-graphene-box (,var ,@args) ,@body))
        ((eq 'point-t type)
         `(with-graphene-point (,var ,@args) ,@body))
        ((eq 'rect-t type)
         `(with-graphene-rect (,var ,@args) ,@body))
        ((eq 'size-t type)
         `(with-graphene-size (,var ,@args) ,@body))
        ((eq 'quad-t type)
         `(with-graphene-quad (,var ,@args) ,@body))
        (t
         (error "WITH-GRAPHENE-OBJECT: Unknown type"))))

(export 'with-graphene-object)

(defmacro with-graphene-objects (vars &body body)
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-graphene-object ,var
           (with-graphene-objects ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-graphene-objects)

;;; --- graphene.utils.lisp ----------------------------------------------------
