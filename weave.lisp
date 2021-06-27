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
;;; - Text blocks starting with =#+begin_src lisp= and ending with
;;;   =#+end_src= are lisp code.

;;; This program reads a lisp file from stdin and writes out an org
;;; file to stdout.  An example command line:
;;; =sbcl --script weave.lisp < weave.lisp > README.org=

;;;; Strings

;;; Just some useful string functions.

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

;;; Here we map strings to lines.  We have four line-types:
;;; comment-line, heading-line, blank-line and code-line.  Note that
;;; the space after the semicolon is part of the prefix.  You can't
;;; have /empty/ comment lines without a space after the semicolon.

(defclass line ()
  ((string
    :initarg :string
    :reader line-string)))

(defclass comment-line (line) ())
(defclass heading-line (line) ())
(defclass blank-line (line) ())
(defclass code-line (line) ())

(defun string->line (string)
  (cond ((string-prefix-p ";;;; " string)
	 (make-instance 'heading-line :string (subseq string 5)))
	((string-prefix-p ";;; " string)
	 (make-instance 'comment-line :string (subseq string 4)))
	((blank-p string)
	 (make-instance 'blank-line :string string))
	(t
	 (make-instance 'code-line :string string))))

;;;; Nodes

;;; The org output is made of nodes.  A node is a thing that has
;;; children.  We maintain a last-child slot which contains the last
;;; child of the node.

(defclass node ()
  ((children
    :initform (make-array 0 :adjustable t :fill-pointer 0))
   (last-child
    :initform nil)))

(defun append-child (node child)
  "Append CHILD to NODE."
  (with-slots (children last-child) node
    (vector-push-extend child children)
    (setf last-child child)))

;;; And now define all the org nodes.
;;; - A document node is the root node and contains sections.
;;; - A section node has a heading and contains text-block nodes.
;;; - A text-block contains strings.

(defclass document (node)
  ())

(defclass section (node)
  ((title :initarg :title :reader section-title)))

(defclass text-block (node)
  ())

(defclass comment-block (text-block)
  ())

(defclass code-block (text-block)
  ())

;;; The add-line method adds a line to a node.
;;; In the document case we add a line to the
;;; last child if it exists.

(defmethod add-line ((self document) (line line))
  (with-slots (last-child) self
    ;; We skip everything before the first heading.
    (when last-child
      (add-line last-child line))))

;;; Create a section an append it.

(defmethod add-line ((self document) (line heading-line))
  (append-child self (make-instance 'section :title (line-string line))))

;;; The section add-line method has to decide if a text-block accepts
;;; the given line.  It does so by using the accept-line method.

(defgeneric accept-line (node line)
  (:method ((self comment-block) (line comment-line)) t)
  (:method ((self comment-block) (line blank-line)) t)
  (:method ((self comment-block) (line code-line)) nil)
  (:method ((self code-block) (line code-line)) t)
  (:method ((self code-block) (line blank-line)) t)
  (:method ((self code-block) (line comment-line)) nil))

;;; Returns the text-block class corresponding to a given line.

(defgeneric line->block-class (line)
  (:method ((line comment-line)) 'comment-block)
  (:method ((line blank-line)) 'comment-block)
  (:method ((line code-line)) 'code-block))

;;; The section /add-line/ method creates a text-block if the
;;; last-child is nil or doesn't accept the given line.

(defmethod add-line ((self section) (line line))
  (with-slots (last-child) self
    (when (or (null last-child)
	      (not (accept-line last-child line)))
      (append-child self (make-instance (line->block-class line))))
    (append-child last-child
		  (line-string line))))

;;;; Print Org

;;; The print org method is simple.
;;; - It prints strings as lines.
;;; - Recurses into nodes.
;;; - Adds org headings.
;;; - And adds org-mode code-blocks to code-blocks.

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

;;; Not much left to do.  Create a document, read lines from
;;; standard input and add them to the document.  Finally print
;;; the document to stdout.  One trick:  We use
;;; ~(find-package :swank)~ to distinguish between interactive
;;; and script use (so we can load the file in slime without
;;; hanging.)

(unless (find-package :swank)
  (loop with document = (make-instance 'document)
     for string = (read-line *standard-input* nil)
     while string do (add-line document (string->line string))
     finally (print-org document t)))

;;; Happy org file creation.


