;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

(defpackage #:formgrep/examples/symbol-usage
  (:documentation
   "Create a basic report about usage of symbols.")
  (:use #:cl)
  (:import-from
   #:formgrep)
  (:import-from
   #:fare-csv)
  (:export
   #:create-symbol-usage-report
   #:write-report-to-file))

(in-package #:formgrep/examples/symbol-usage)

(defstruct symbol-entry
  qualifier
  name
  (mentions 0)
  (min-depth most-positive-fixnum)
  (max-depth 0))

(defstruct (symbol-usage-report
            (:conc-name report-))
  root-directory
  (creation-time (get-universal-time))
  (creation-machine (machine-instance))
  (symbol-entries (make-hash-table :test 'equal)))

(defvar *default-qualifier* "CL-USER")

(defmacro with-unqualified-symbols-package (&body forms)
  `(let ((*default-qualifier* *default-qualifier*)
         (*package*
           (make-package "FORMGREP-UNQUALIFIED-SYMBOLS"
                         :use '("COMMON-LISP"))))
     (unwind-protect
          (progn ,@forms)
       (delete-package *package*))))

(defvar *report* nil)

(defun create-symbol-usage-report (&key (root-directory *default-pathname-defaults*)
                                        (progress-stream *standard-output*))
  (let ((*report* (make-symbol-usage-report :root-directory root-directory))
        (i 0))
    (with-unqualified-symbols-package
      (handler-bind (#+sbcl (sb-ext:package-lock-violation #'formgrep:skip-form))
        (formgrep:do-form-matches (match :operator-regex "" :root-directory root-directory)
          (note-match match)
          (when (zerop (mod (incf i) 1000))
            (write-char #\. progress-stream)
            (force-output progress-stream)))))
    *report*))

(defvar *agenda* '())

(defvar *occurs* (make-hash-table))

(defvar *match* nil)

(defstruct agenda-item
  form
  depth)

(defvar *agenda-item* nil)

(defun note-match (match)
  (let ((*agenda* (list (make-agenda-item :form (formgrep:match-form match)
                                          :depth 0)))
        (*match* match))
    (clrhash *occurs*)
    (loop while *agenda*
          do (let* ((*agenda-item* (pop *agenda*))
                    (form (agenda-item-form *agenda-item*)))
               (note-form form)
               ;; Dumb way to figure out an unqualified symbol's
               ;; package.
               (when (and (zerop (current-depth))
                          (consp form)
                          (eq (car form) 'in-package))
                 (setf *default-qualifier* (ensure-string (cadr form))))))))

(defun ensure-string (object)
  (if (stringp object)
      object
      (princ-to-string object)))

(defun current-depth ()
  (agenda-item-depth *agenda-item*))

(defun enqueue-form (form &optional (depth-adjust 1))
  (unless (or (numberp form)
              (characterp form)
              (stringp form)
              (pathnamep form)
              (gethash form *occurs*))
    (push (make-agenda-item :form form :depth (+ (current-depth) depth-adjust))
          *agenda*)))

(defun mark (form)
  (setf (gethash form *occurs*) t))

(defun note-form (form)
  (cond ((null form)
         ;; There's no real point in getting symbol usage about NIL.
         )
        ((or (symbolp form)
             (typep form 'formgrep:symref))
         (note-symbol form))
        ((consp form)
         (mark form)
         (enqueue-form (car form))
         (enqueue-form (cdr form) (if (consp (cdr form)) 0 1)))
        ((arrayp form)
         ;; Is it really worth traversing?
         (mark form)
         (dotimes (i (array-total-size form))
           (enqueue-form (row-major-aref form i) 0)))
        ((or (numberp form)
             (characterp form)
             (stringp form)
             (pathnamep form)))
        (t
         (warn "Form ~S" form))))

(define-modify-macro minf (&rest args) min)

(define-modify-macro maxf (&rest args) max)

(defun note-symbol (symbol)
  (let ((symbol-entry (intern-symbol-entry symbol)))
    (when symbol-entry
      (incf (symbol-entry-mentions symbol-entry))
      (let ((depth (current-depth)))
        (minf (symbol-entry-min-depth symbol-entry) depth)
        (maxf (symbol-entry-max-depth symbol-entry) depth)))))

(defun intern-symbol-entry (symbol)
  (multiple-value-bind (qualifier name)
      (cond ((typep symbol 'formgrep:symref)
             (values (formgrep:symref-qualifier symbol)
                     (formgrep:symref-name symbol)))
            ((null (symbol-package symbol))
             (return-from intern-symbol-entry nil))
            ((eq (symbol-package symbol) *package*)
             (values *default-qualifier*
                     (symbol-name symbol)))
            (t
             (values (package-name (symbol-package symbol))
                     (symbol-name symbol))))
    (let ((key (format nil "~A:~A" qualifier name)))
      (or (gethash key (report-symbol-entries *report*))
          (setf (gethash key (report-symbol-entries *report*))
                (make-symbol-entry :qualifier qualifier
                                   :name name))))))

(defun write-report-to-file (report filename &key (format :csv))
  (assert (eq format :csv) (format) "Only :CSV format is supported.")
  (with-open-file (stream filename :direction :output)
    (fare-csv:with-rfc4180-csv-syntax ()
      (fare-csv:write-csv-line
       '("qualifier" "name" "mentions" "min_depth" "max_depth")
       stream)
      (loop for symbol-entry being each hash-value of (report-symbol-entries report)
            do (fare-csv:write-csv-line
                (list (symbol-entry-qualifier symbol-entry)
                      (symbol-entry-name symbol-entry)
                      (symbol-entry-mentions symbol-entry)
                      (symbol-entry-min-depth symbol-entry)
                      (symbol-entry-max-depth symbol-entry))
                stream)))))
