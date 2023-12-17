;;; ----------------------------------------------------------------------------
;;; cl-cffi-graphene.asd
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

(defsystem :cl-cffi-graphene
  :name "cl-cffi-graphene"
  :version "0.4.0"
  :author  "Dieter Kaiser"
  :license "MIT"
  :depends-on (:cffi :iterate)
  :in-order-to ((asdf:test-op (test-op "cl-cffi-graphene/test")))
  :components
  ((:module src
    :serial t
    :components
    ((:file "graphene.package")
     (:file "graphene.init")
     (:file "graphene.point")
     (:file "graphene.size")
     (:file "graphene.point3d")
     (:file "graphene.vector")
     (:file "graphene.rectangle")
     (:file "graphene.matrix")
     (:file "graphene.quad")
     (:file "graphene.box")
     (:file "graphene.plane")
     (:file "graphene.triangle")
     (:file "graphene.sphere")
     (:file "graphene.frustum")
     (:file "graphene.quaternion")
     (:file "graphene.euler")
     (:file "graphene.ray")
     ))))

;; Definine a test operation for the library

(defsystem :cl-cffi-graphene/test
  :name "cl-cffi-graphene/test"
  :version "0.4.0"
  :author  "Dieter Kaiser"
  :license "MIT"
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
               (:file "rtest-graphene-vector")
               (:file "rtest-graphene-utils")))

;;; --- End of file cl-cffi-graphene.asd ---------------------------------------
