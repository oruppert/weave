;; weave.lisp -- convert lisp to org
;;
;; Copyright (c) 2021, Olaf Ritter von Ruppert
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;; Weave

;;; I've always wanted a program to convert Lisp files using standard
;;; comments to org files.  The emacs /outshine-mode/ is good, but has
;;; its own comment conventions and is not that easy to get working as
;;; part of a build process (you have to have outshine installed.)

;;; Standard common lisp comment style is defined in the Hyperspec.
;;; - Comments starting with =;;;;= serve as a title for the code which
;;;   follows.
;;; - Comments starting with =;;;= serve as the description for the
;;;   code which follows.

;;; Org files are plain text files with the following conventions:
;;; - Lines starting with =*= are heading lines.
;;; - Blocks starting with =#+begin_src lisp= and ending with
;;;   =#+end_src= are lisp code.

;;; This program reads a lisp file from stdin and writes out an org
;;; file to stdout.  An example command line:
;;; =sbcl --script weave.lisp < weave.lisp > README.org=

;;;; Strings

(defparameter *whitespace* #(#\Space #\Tab #\Newline #\Return))

(defun whitespace-char-p (char)
  "char -> boolean"
  (and (find char *whitespace*) t))

(defun blank-p (string)
  "string -> boolean"
  (every #'whitespace-char-p string))

(defun string-prefix-p (prefix string)
  "string, string -> boolean"
  (eql 0 (search prefix string)))

;;;; Lines

(defclass line ()
  ((string
    :initarg :string
    :reader line-string)))

(defclass comment-line (line) ())
(defclass heading-line (line) ())
(defclass blank-line (line) ())
(defclass code-line (line) ())

(defun make-line (string)
  "string -> line"
  (cond ((string-prefix-p ";;;; " string)
	 (make-instance 'heading-line :string (subseq string 5)))
	((string-prefix-p ";;; " string)
	 (make-instance 'comment-line :string (subseq string 4)))
	((blank-p string)
	 (make-instance 'blank-line :string string))
	(t
	 (make-instance 'code-line :string string))))

;;;; Nodes

(defclass node ()
  ((children
    :initform (make-array 0 :adjustable t :fill-pointer 0))
   (last-child
    :initform nil)))

(defun add-child (node child)
  "Append CHILD to NODE."
  (with-slots (children last-child) node
    (vector-push-extend child children)
    (setf last-child child)))

(defclass document (node)
  ())

(defclass section (node)
  ((title :initarg :title :reader section-title)))

(defclass comment-block (node)
  ())

(defclass code-block (node)
  ())

(defmethod add-line ((self document) (line line))
  ;; We skip all lines before the first heading.
  (with-slots (last-child) self
    (when last-child
      (add-line last-child line))))

(defmethod add-line ((self document) (line heading-line))
  (add-child self (make-instance 'section :title (line-string line))))

(defgeneric accept-line (node line)
  (:method ((self comment-block) (line comment-line)) t)
  (:method ((self comment-block) (line blank-line)) t)
  (:method ((self comment-block) (line code-line)) nil)
  (:method ((self code-block) (line code-line)) t)
  (:method ((self code-block) (line blank-line)) t)
  (:method ((self code-block) (line comment-line)) nil))

(defgeneric line->block-class (line)
  (:method ((line comment-line)) 'comment-block)
  (:method ((line blank-line)) 'comment-block)
  (:method ((line code-line)) 'code-block))

(defmethod add-line ((self section) (line line))
  (with-slots (last-child) self
    (when (or (null last-child)
	      (not (accept-line last-child line)))
      (add-child self (make-instance (line->block-class line))))
    (add-child last-child
	       (line-string line))))

;;;; Print Org

(defmethod print-org ((string string) stream)
  (format stream "~a~%" string))

(defmethod print-org ((self node) stream)
  (with-slots (children) self
    (loop for child across children
       do (print-org child stream))))

(defmethod print-org :before ((self section) stream)
  (format stream "* ~a~%" (section-title self)))

(defmethod print-org :before ((self code-block) stream)
  (format stream "#+begin_src lisp~%"))

(defmethod print-org :after ((self code-block) stream)
  (format stream "#+end_src~%"))

;;;; Process standard input

;;; Nothing much left to do.  Create a document, read lines from
;;; standard input and add them to the document.  Finally print
;;; the document to stdout.  One tick:  We use
;;; ~(find-package :swank)~ to distinguish between interactive
;;; and script use (so we can load the file in slime without
;;; hanging.)

(unless (find-package :swank)
  (loop with document = (make-instance 'document)
     for line = (read-line *standard-input* nil)
     while line do (add-line document (make-line line))
     finally (print-org document t)))

;;; Happy org file creation.


