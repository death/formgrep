;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:formgrep
  :description "Try to find top-level Lisp forms matching an operator regex."
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on ("alexandria" "cl-fad" "eclector" "babel" "cl-ppcre")
  :components ((:file "formgrep")))
