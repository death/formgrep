;;;; +----------------------------------------------------------------+
;;;; | formgrep                                                       |
;;;; +----------------------------------------------------------------+

(defpackage #:formgrep/examples/top-matches
  (:documentation
   "Find top forms in Lisp sources, according to a metric.")
  (:use #:cl)
  (:import-from
   #:formgrep)
  (:import-from
   #:pileup)
  (:export
   #:top-matches))

(in-package #:formgrep/examples/top-matches)

(defun find-restarts (identifier &optional condition)
  (remove identifier
          (compute-restarts condition)
          :test-not #'eq
          :key #'restart-name))

(defun last-elt (list)
  (car (last list)))

(defun continue-scanning (condition)
  (let ((continue-restarts (find-restarts 'continue condition)))
    (when continue-restarts
      (invoke-restart (last-elt continue-restarts)))))

(defun count-atoms (form)
  (let ((occurs (make-hash-table)))
    (labels ((rec (x)
               (cond ((atom x) 1)
                     ((gethash x occurs) 0)
                     (t
                      (setf (gethash x occurs) t)
                      (+ (rec (car x)) (rec (cdr x)))))))
      (rec form))))

(defstruct (smatch (:type list))
  match
  score)

(defun top-matches (&key (root-directory *default-pathname-defaults*)
                         (metric #'count-atoms)
                         (n 5)
                         (progress-stream *standard-output*))
  "Find the top N forms in a directory of Lisp source files, according
to a metric.

Returns the list of the scored matches in descending order.

METRIC should a function that takes a form (which may contain cycles)
and returns a score.  The default metric counts the number of atoms in
a form, which is a rather crude heuristic for complexity."
  (let ((heap (pileup:make-heap #'< :key #'smatch-score))
        (i 0))
    (handler-bind (#+sbcl (sb-ext:package-lock-violation #'continue-scanning))
      (formgrep:do-form-matches (match :operator-regex "" :root-directory root-directory)
        (let ((score (funcall metric (formgrep:match-form match))))
          (pileup:heap-insert (make-smatch :match match :score score) heap)
          (when (> (pileup:heap-count heap) n)
            (pileup:heap-pop heap)))
        (when (zerop (mod (incf i) 1000))
          (write-char #\. progress-stream)
          (force-output progress-stream)))
      (nreverse
       (loop until (pileup:heap-empty-p heap)
             collect (pileup:heap-pop heap))))))
