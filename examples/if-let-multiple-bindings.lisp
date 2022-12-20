;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

(defpackage #:formgrep/examples/if-let-multiple-bindings
  (:documentation
   "Find occurrences of IF-LET forms with multiple bindings.")
  (:use #:cl)
  (:import-from
   #:formgrep)
  (:export
   #:show-if-let-multiple-bindings))

(in-package #:formgrep/examples/if-let-multiple-bindings)

(defun graph-some (function graph)
  (let ((visited (make-hash-table))
        (agenda (list graph)))
    (loop until (null agenda)
          do (let ((x (pop agenda)))
               (setf (gethash x visited) t)
               (cond ((funcall function x)
                      (return-from graph-some t))
                     ((consp x)
                      (when (not (gethash (car x) visited))
                        (push (car x) agenda))
                      (when (and (not (gethash (cdr x) visited))
                                 (not (eq (car x) (cdr x))))
                        (push (cdr x) agenda))))))))

(defun print-match (match)
  (format t "~&[~A:~D]~%  ~S~2%"
          (enough-namestring (formgrep:match-filename match))
          (formgrep:match-line match)
          (formgrep:match-form match)))

(defun show-if-let-multiple-bindings ()
  (handler-bind ((error #'formgrep:skip-file))
    (formgrep:do-form-matches (match :operator-regex "def")
      (when (graph-some (lambda (x)
                          (and (consp x)
                               (eq (car x) 'if-let)
                               (listp (cadr x))
                               (consp (caadr x))
                               (> (length (cadr x)) 1)))
                        (formgrep:match-form match))
        (print-match match)))))
