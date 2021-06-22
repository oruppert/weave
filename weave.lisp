;; weave.lisp -- Convert Lisp to Org
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

(defpackage :weave
  (:use :common-lisp))

(in-package :weave)

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
  ((children :initform (make-array 0 :adjustable nil :fill-pointer 0))
   (last-child :initform nil)))

(defmethod add-child ((self node) child)
  (with-slots (children last-child) self
    (vector-push-extend child children)
    (setf last-child child)))

(defclass document (node) ())

(defmethod add-line ((self document) (line line))
  ;; We skip all lines before the first heading.
  (with-slots (last-child) self
    (when last-child
      (add-line last-child line))))

(defmethod add-line ((self document) (line heading-line))
  (add-child self (make-instance 'section :title (line-string line))))

(defclass section (node)
  ((title :initarg :title :reader section-title)))

(defgeneric accept-line (node child)
  (:method ((self node) (line line)) nil))

(defgeneric block-class (line))

(defmethod add-line ((self section) (line line))
  (with-slots (last-child) self
    (when (or (null last-child)
	      (not (accept-line last-child line)))
      (add-child self (make-instance (block-class line))))
    (add-child last-child
	       (line-string line))))

(defclass comment-block (node) ())
(defmethod accept-line ((self comment-block) (line comment-line)) t)
(defmethod accept-line ((self comment-block) (line blank-line)) t)
(defmethod block-class ((line comment-line)) 'comment-block)
(defmethod block-class ((line blank-line)) 'comment-block)

(defclass code-block (node) ())
(defmethod accept-line ((self code-block) (line code-line)) t)
(defmethod accept-line ((self code-block) (line blank-line)) t)
(defmethod block-class ((line code-line)) 'code-block)

;;;; Print Org

(defmethod print-org ((string string) stream)
  (format stream "~a~%" string))

(defmethod print-org ((self node) stream)
  (with-slots (children) self
    (loop for child across children
       do (print-org child stream))))

(defmethod print-org :before ((self section) stream)
  (format stream "* ~A~%" (section-title self)))

(defmethod print-org :before ((self code-block) stream)
  (format stream "#+begin_src lisp~%"))

(defmethod print-org :after ((self code-block) stream)
  (format stream "#+end_src~%"))

;;;; Process standard input

(loop with document = (make-instance 'document)
   for line = (read-line *standard-input* nil)
   while line do (add-line document (make-line line))
   finally (print-org document t))


