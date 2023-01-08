;;; ----------------------------------------------------------------------------
;;; graphene.init.lisp
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

(in-package :graphene)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library graphene
      ((:and :unix (:not :darwin))
       (:or "libgraphene-1.0.so"))
       (:darwin "libgraphene-1.0.dylib")
       (:windows "libgraphene-1.0-0.dll")
       (t (:default "libgraphene-1.0")))

    (cffi:use-foreign-library graphene)
    (pushnew :graphene *features*))

;;; --- End of file graphene.init.lisp -----------------------------------------
