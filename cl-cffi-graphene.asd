;;; ----------------------------------------------------------------------------
;;; cl-cffi-graphene.asd
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

(defsystem :cl-cffi-graphene
  :name "cl-cffi-graphene"
  :version "1.10"                              ; Version of the Graphene Library
  :author  "Dieter Kaiser"
  :license "LLGPL"
  :depends-on (:cffi)
  :in-order-to ((asdf:test-op (test-op "cl-cffi-graphene/test")))
  :components
  ((:module src
    :serial t
    :components
    ((:file "graphene.package")
     (:file "graphene.init")
     (:file "graphene.point")
     (:file "graphene.size")
     (:file "graphene.rectangle")
     (:file "graphene.point3d")
     (:file "graphene.vector")
     (:file "graphene.matrix")
     (:file "graphene.quad")
     (:file "graphene.triangle")
     (:file "graphene.sphere")
     (:file "graphene.box")
     (:file "graphene.plane")
     (:file "graphene.frustum")
     (:file "graphene.quaternion")
     (:file "graphene.euler")
     (:file "graphene.ray")))))

;; Definine a test operation for the library

(defsystem :cl-cffi-graphene/test
  :name "cl-cffi-graphene/test"
  :depends-on (:cl-cffi-graphene :fiveam)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :graphene-suite
                                                    :graphene-test)))
  :pathname "test/"
  :serial t
  :components ((:file "rtest-graphene")
               (:file "rtest-graphene-box")
               (:file "rtest-graphene-euler")
               (:file "rtest-graphene-frustum")
               (:file "rtest-graphene-matrix")
               (:file "rtest-graphene-plane")
               (:file "rtest-graphene-point")
               (:file "rtest-graphene-point3d")
               (:file "rtest-graphene-quad")
               (:file "rtest-graphene-quaternion")
               (:file "rtest-graphene-ray")
               (:file "rtest-graphene-rectangle")
               (:file "rtest-graphene-size")
               (:file "rtest-graphene-sphere")
               (:file "rtest-graphene-triangle")
               (:file "rtest-graphene-vector")))

;;; --- End of file cl-cffi-graphene.asd ---------------------------------------
