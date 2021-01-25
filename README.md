# formgrep

Try to find top-level Lisp forms matching an operator regex.

# Example

```lisp
CL-USER> (setf *default-pathname-defaults* #p"/home/death/quicklisp/local-projects/formgrep/")
#P"/home/death/quicklisp/local-projects/formgrep/"
CL-USER> (dolist (match (formgrep:formgrep "def" :file-type :wild))
           (format t "~A:~D - ~(~A ~A~)~%"
                   (enough-namestring (formgrep:match-filename match))
                   (formgrep:match-line match)
                   (first (formgrep:match-form match))
                   (second (formgrep:match-form match))))
README.md:46 - defun tree-some
README.md:52 - defun print-match
README.md:58 - defun test-defcstruct-count
README.md:63 - defun test-defcstruct-array
formgrep.asd:9 - defsystem formgrep
formgrep.lisp:5 - defpackage formgrep
formgrep.lisp:28 - defun map-files
formgrep.lisp:40 - defmacro do-files
formgrep.lisp:44 - defclass eclector-client
formgrep.lisp:47 - defstruct symref
formgrep.lisp:51 - defmethod interpret-symbol
formgrep.lisp:71 - defmethod evaluate-expression
formgrep.lisp:77 - defmethod evaluate-feature-expression
formgrep.lisp:82 - defun read-form
formgrep.lisp:88 - defun call-with-file-contents
formgrep.lisp:96 - defmacro with-file-contents
formgrep.lisp:99 - defun preprocess-operator-regex
formgrep.lisp:113 - defstruct match
formgrep.lisp:118 - defun map-matching-forms
formgrep.lisp:144 - defmacro do-matching-forms
formgrep.lisp:150 - defun formgrep
NIL
```

# Another example

Here is the original use-case for formgrep:

```lisp
(defun tree-some (function tree)
  (if (atom tree)
      (funcall function tree)
      (or (tree-some function (car tree))
          (tree-some function (cdr tree)))))

(defun print-match (match)
  (format t "~&[~A:~D]~%  ~S~2%"
          (enough-namestring (formgrep:match-filename match))
          (formgrep:match-line match)
          (formgrep:match-form match)))

(defun test-defcstruct-count ()
  (formgrep:do-matching-forms (match :operator-regex "defcstruct")
    (when (tree-some (lambda (x) (eq x :count)) (formgrep:match-form match))
      (print-match match))))

(defun test-defcstruct-array ()
  (formgrep:do-matching-forms (match :operator-regex "defcstruct")
    (when (tree-some (lambda (x) (eq x :array)) (formgrep:match-form match))
      (print-match match))))
```

# License

MIT
