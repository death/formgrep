;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

(defpackage #:formgrep
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:fad)
  (:import-from #:eclector.reader)
  (:import-from #:eclector.base)
  (:import-from #:babel)
  (:import-from #:ppcre)
  (:export
   #:formgrep
   #:match
   #:match-filename
   #:match-line
   #:match-form
   #:do-form-matches
   #:map-form-matches
   #:symref
   #:symref-name
   #:symref-qualifier
   #:eclector-client
   #:skip-form
   #:skip-file))

(in-package #:formgrep)

(defun skip-file (condition)
  (let ((restart (find-restart 'skip-file condition)))
    (when restart
      (invoke-restart restart))))

(defun skip-form (condition)
  (let ((restart (find-restart 'skip-form condition)))
    (when restart
      (invoke-restart restart))))

(defun file-of-type (type)
  (if (eq type :wild)
      (constantly t)
      (lambda (filename)
        (equal (pathname-type filename) type))))

(defun map-files (function predicate)
  (fad:walk-directory
   *default-pathname-defaults*
   (lambda (filename)
     (when (and (not (fad:directory-pathname-p filename))
                (funcall predicate filename))
       (with-simple-restart (skip-file "Skip processing this file.")
         (funcall function filename))))
   :directories :breadth-first
   :test (lambda (x)
           (not (equal ".git" (alexandria:last-elt (pathname-directory x)))))))

(defmacro do-files ((var predicate) &body forms)
  `(block nil
     (map-files (lambda (,var) ,@forms) ,predicate)))

(defclass eclector-client ()
  ())

(defstruct symref
  name
  qualifier)

(defmethod eclector.reader:interpret-symbol ((client eclector-client)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (declare (ignore input-stream internp))
  (cond ((or (eq package-indicator :current)
             (eq package-indicator :keyword)
             (and (stringp package-indicator)
                  (find-package package-indicator)))
         (let ((package (if (eq package-indicator :current)
                            *package*
                            package-indicator)))
           (intern symbol-name package)))
        ((null package-indicator)
         (make-symbol symbol-name))
        (t
         (make-symref :name symbol-name
                      :qualifier package-indicator))))

(defmethod eclector.reader:evaluate-expression ((client eclector-client)
                                                expression)
  (declare (ignore expression))
  ;; Don't bother with read-time evaluation.
  nil)

(defmethod eclector.reader:evaluate-feature-expression ((client eclector-client)
                                                        feature-expression)
  ;; Include all forms conditional on feature expressions.
  t)

(defmethod eclector.reader:make-structure-instance ((client eclector-client)
                                                    name
                                                    initargs)
  ;; Don't bother with structure instances, for now.
  nil)

(defun read-form (string &optional (start 0))
  (eclector.reader:read-from-string string nil nil :start start))

(when (find-package "MARRAY")
  (pushnew :formgrep-use-marray *features*))

(defun call-with-file-contents (function filename &key (encoding :latin-1))
  #+formgrep-use-marray
  (marray:with-file-mapping (octets filename)
    (funcall function (babel:octets-to-string octets :encoding encoding)))
  #-formgrep-use-marray
  (let ((octets (alexandria:read-file-into-byte-vector filename)))
    (funcall function (babel:octets-to-string octets :encoding encoding))))

(defmacro with-file-contents ((contents-var filename &rest args) &body forms)
  `(call-with-file-contents (lambda (,contents-var) ,@forms) ,filename ,@args))

(defun preprocess-operator-regex (operator-regex)
  ;; Accept symbols as regex designators.
  (setf operator-regex (string operator-regex))
  ;; Since we splice the operator regex into a larger regex, we need
  ;; to treat anchors specially.
  (when (alexandria:starts-with #\^ operator-regex)
    (setf operator-regex (subseq operator-regex 1)))
  (when (alexandria:ends-with #\$ operator-regex)
    (setf operator-regex
          (concatenate 'string
                       (subseq operator-regex 0 (1- (length operator-regex)))
                       "\\b")))
  operator-regex)

(defstruct match
  filename
  line
  form)

(defun map-form-matches (function &key (root-directory *default-pathname-defaults*)
                                       (operator-regex "def")
                                       (file-type "lisp")
                                       (include-file-p (file-of-type file-type))
                                       (encoding :latin-1)
                                       (package *package*)
                                       (test (constantly t))
                                       (eclector-client (make-instance 'eclector-client)))
  (let* ((eclector.reader:*client* eclector-client)
         (operator-regex (preprocess-operator-regex operator-regex))
         (scanner (ppcre:create-scanner (format nil "^\\b*\\(([^: ]*:)?~A"
                                                operator-regex)
                                        :case-insensitive-mode t
                                        :multi-line-mode t))
         (*default-pathname-defaults* (pathname root-directory))
         (*package* package))
    (handler-bind ((error #'eclector.base:recover))
      (do-files (filename include-file-p)
        (with-file-contents (contents filename :encoding encoding)
          (let ((line 1)
                (line-counter-start 0))
            (ppcre:do-matches (match-start match-end scanner contents)
              (incf line
                    (count #\Newline contents
                           :start line-counter-start
                           :end match-start))
              (setf line-counter-start match-start)
              (with-simple-restart (skip-form "Skip processing this form.")
                (let* ((form (read-form contents match-start))
                       (match (make-match :filename filename :line line :form form)))
                  (when (funcall test match)
                    (funcall function match)))))))))))

(defmacro do-form-matches ((match-var &rest args) &body forms)
  `(block nil
     (map-form-matches (lambda (,match-var) ,@forms) ,@args)))

;; Default values for keyword arguments should always match
;; MAP-FORM-MATCHES.
(defun formgrep (operator-regex &rest args
                                &key (root-directory *default-pathname-defaults*)
                                     (file-type "lisp")
                                     (include-file-p (file-of-type file-type))
                                     (encoding :latin-1)
                                     (package *package*)
                                     (test (constantly t))
                                     (eclector-client (make-instance 'eclector-client)))
  (declare (ignore root-directory include-file-p encoding package test eclector-client))
  (let ((matches '()))
    (apply #'map-form-matches
           (lambda (match)
             (push match matches))
           :operator-regex operator-regex
           args)
    (nreverse matches)))
