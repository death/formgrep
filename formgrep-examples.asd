;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:formgrep-examples
  :description "Example programs that use formgrep."
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on ("formgrep" "pileup")
  :components ((:file "examples/top-matches")))
