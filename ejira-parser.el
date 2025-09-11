;;; ejira-parser.el --- Parsing to and from JIRA markup.

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman@gmail.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Version: 1.0
;; Package-Requires: ((org "8.3") (ox-jira) (language-detection) (s "1.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Two-directional parser for JIRA-markdown. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is done with regular expressions and is still in early beta state.

;; TODO:

;;; Code:

(require 'org)
(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)

(defvar ejira-parser-export-process-underscores t
  "If nil, the parser will not make underscores into anchors.")

(defun ejira-parser-org-to-jira (s)
  "Transform org-style string S into JIRA format."

  (if ejira-parser-export-process-underscores
      (org-export-string-as s 'jira t)
    (org-export-string-as
     (concat "#+OPTIONS: ^:nil\n" s)
     'jira t)))

(defun random-alpha ()
  "Generate a random lowercase character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-identifier (&optional length)
  "Create a random string of length LENGTH containing only lowercase letters."
  (mapconcat (lambda (_) (random-alpha)) (make-list (or length 16) nil) ""))

(defun jsonp (string)
  "Validates a JSON string."
  (condition-case nil
      (progn
        (json-read-from-string string)
        t)
    (error nil)))

(defun ejira-parser-jira-to-org (adf-input &optional level)
  "Transform ADF JSON (Atlassian document format) into org-style text.
If LEVEL is given, shift all headings to the right by that amount."
  ;; (message (cl-format nil "ADF-Input: =======~%~a~% END input =====~%" adf-input))
  (let (returnvalue)
    (cond ((jsonp adf-input)
           (let ((adf (json-parse-string adf-input :object-type 'alist :array-type 'array)))
             (cond ((ejira3-adf-type-p "doc" adf)
                    (setf returnvalue (ejira3--parse-adf-vector (alist-get 'content adf) level)))
                   (t (setf returnvalue "===ejira3-parse-error-not-adf===")))))
          ((and (listp adf-input)
                (ejira3-adf-type-p "doc" adf-input))
           (setf returnvalue (ejira3--parse-adf-vector (alist-get 'content adf-input))))
          (t (setf returnvalue adf-input)))
    (cl-format nil "~a" returnvalue)))

(defun ejira3-adf-type-p (type item)
  (equal type (alist-get 'type item)))

(defun ejira3--parse-adf-node (node &optional level parent)
  "Convert a single ADF node to Org-mode."
  (let ((type (alist-get 'type node))
        (content (alist-get 'content node))
        (text (alist-get 'text node))
        (marks (alist-get 'marks node))
        (attrs (alist-get 'attrs node))
        (indentlevel (if parent parent 0)))
    ;; (message (cl-format nil "Type: ==================== ~%~a" type))    
    (pcase type
      ("doc" (ejira3--parse-adf-vector content))
      ("paragraph" (if content (cl-format nil "~a~&~%" (ejira3--parse-adf-vector content)) " "))
      ("text" (ejira3--adf-text-to-org text marks))
      ("heading" (let ((adf-level (or (alist-get 'level attrs) 1)))
                   (concat (make-string (if (numperp level) (+ level adf-level) level) ?*) " " (ejira3--parse-adf-vector content) "\n\n")))
      ("codeBlock" (concat "#+BEGIN_SRC " (or (alist-get 'language attrs) "") "\n"
                           (ejira3--parse-adf-vector content)
                           "#+END_SRC\n\n"))
      ("bulletList" (concat
                     (mapconcat
                      (lambda (item)
                        (concat (make-string (* 2 indentlevel) ? ) "- " (s-trim (ejira3--parse-adf-vector (alist-get 'content item) level (1+ indentlevel)))))
                      content "\n")
                     (if (> indentlevel 0) "\n" "\n\n")))
       ("orderedList" (concat
                       (mapconcat
                        (lambda (item)
                        (concat (make-string (* 2 indentlevel) ? ) "1. " (s-trim (ejira3--parse-adf-vector (alist-get 'content item) level (1+ indentlevel)))))
                        content "\n")
                       "\n\n"))
      ("hardBreak" "\n")
      ("table" (ejira3--adf-table-to-org content))
      ("mention" (cl-format nil "[[~~accountid:~a][~a]] " (alist-get 'id attrs) (subseq (alist-get 'text attrs) 1)))
      (_ ""))))

(defun ejira3--adf-table-to-org (content)
  "Convert Table from ADF to Org. CONTENT is the content vector of the table node.
Since Org tables are limited to single line, we discard what does not fit."
  (message (cl-format nil "Table content: ==================== ~%~a~%END content ==================~%" content))
  (let ((table)
        (header))
    (cl-loop for row across content do
             (cl-loop for cell across (alist-get 'content row)
                      do (if (ejira3-adf-type-p "tableHeader" cell)
                             (setf header t)
                           (setf header nil))
                      collect (ejira3--parse-adf-vector (alist-get 'content cell))
                      into row-contents
                      finally (if header
                                  (setf table (append table (list (mapcar #'s-trim row-contents)) (list (list 'header))))
                                (setf table (append table (list (mapcar #'s-trim row-contents)))))
                      (message (cl-format nil "+++++++ Table: ~a" table)))
             finally (return
                      (cl-format nil "~{|~a|~%~}"
                                 (cl-loop for row-items in table
                                          collect (if (symbolp (car row-items))
                                                      "-----"
                                                    (cl-format nil "~{~a ~^| ~}" row-items))))))))

(defun ejira3--parse-adf-vector (vec &optional level parent)
  "Convert an ADF array parsed into a Lisp vector.
Returns a string in Org markup."
  (if (not (vectorp vec))
      "==ejira3-parse-error-no-vector==="
    (loop for obj across vec
          collect (ejira3--parse-adf-node obj level parent) into collection
          finally (return (cl-format nil "~{~a~}" collection)))))

(defun ejira3--parse-adf-object (obj &optional level parent)
  "Convert an ADF object parsed into a lisp list.
Returns a string in Org markup."
  (if (not (listp obj))
      "==ejira3-parse-error-no-list==="
    (loop for item in obj
          collect (ejira3--parse-adf-node item level parent) into collection
          finally (return (cl-format nil "~{~a~}" collection)))))

(defun ejira3--adf-text-to-org (text marks)
  "Apply ADF marks to text for Org-mode."
  (if (not marks)
      text
    (let ((result text))
      (cl-loop for mark across marks do
        (let ((type (alist-get 'type mark))
              (attrs (alist-get 'attrs mark)))
          (setq result
                (pcase type
                  ("strong" (concat "*" (s-trim result) "* "))
                  ("em" (concat "/" (s-trim result) "/ "))
                  ("code" (concat "~" (s-trim result) "~ "))
                  ("strike" (concat "+" (s-trim result) "+ "))
                  ("underline" (concat "_" (s-trim result) "_ "))
                  ("link" (concat "[[" (alist-get 'href attrs) "][" result "]]"))
                  (_ result)))))
      result)))


(provide 'ejira-parser)
;;; ejira-parser.el ends here
