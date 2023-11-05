;;; ----------------------------------------------------------------------------
;;; graphene.init.lisp
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

(in-package :graphene)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library graphene
      ((:and :unix (:not :darwin))
       (:or "libgraphene-1.0.so.0" "libgraphene-1.0.so"))
       (:darwin "libgraphene-1.0.dylib")
       (:windows "libgraphene-1.0-0.dll")
       (t (:default "libgraphene-1.0")))

    (cffi:use-foreign-library graphene)
    ;; push the hostname on *features*
    (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
    (pushnew :graphene *features*))

;;; --- End of file graphene.init.lisp -----------------------------------------
