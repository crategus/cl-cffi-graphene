;;; ----------------------------------------------------------------------------
;;; liber.lisp
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

#-cl-cffi-graphene-documentation
(push :liber-documentation *features*)

(asdf:load-system :liber)
(asdf:load-system :cl-cffi-graphene :force t)

(defpackage :liber-graphene
  (:use :common-lisp)
  (:import-from :liber)
  (:export :generate-html
           :generate-html-single-page))

(in-package :liber-graphene)

;;; ---------------------------------------------------------------------------

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-graphene)))
         (output-directory (merge-pathnames "../books/cl-cffi-graphene/" base)))
    (format t "Generate HTML to ~a~%" output-directory)
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:graphene)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-graphene API documentation"
      :heading "cl-cffi-graphene"
      :css "crategus.css"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-graphene)))
         (output-directory
             (merge-pathnames "../books/cl-cffi-graphene/single-page/" base)))
    (format t "Generate Single PAGE HTML to ~a~%" output-directory)
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:graphene)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-graphene API documentation (single page)"
      :heading "cl-cffi-graphene"
      :css "crategus.css"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

;;; --- End of file liber.lisp -------------------------------------------------
