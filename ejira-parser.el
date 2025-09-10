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

(defun ejira-parser-jira-to-org (adf-string &optional level)
  "Transform ADF JSON (Atlassian document format) into org-style text.
If LEVEL is given, shift all headings to the right by that amount."
  (let (returnvalue)
    (cond ((jsonp adf-string)
           (let ((adf (json-parse-string adf-string :object-type 'alist :array-type 'array)))
             (cond ((adf-type-p "doc" adf)
                    (cl-loop for item across (alist-get 'content adf) 
                          collect (ejira--parse-adf-item item) into org-string-list
                          finally (setf returnvalue (cl-format nil "~{~a~}~%" org-string-list))))
                   (t (setq returnvalue adf-string)))))
          ((and (listp adf-string)
                (adf-type-p "doc" adf-string))
           (cl-loop for item across (alist-get 'content adf-string)
                          collect (ejira--parse-adf-item item) into org-string-list
                          finally (setf returnvalue (cl-format nil "~{~a~}~%" org-string-list))))
          (t (setf returnvalue adf-string)))
    (cl-format nil "~a" returnvalue)))

(defun ejira--parse-adf-item (item)
  "Parse individual items of an ADF document's content node and convert to ORG string"
  (let (output)
    (if (vectorp item)
        (setf output (append output (cl-loop for subitem across item
                                          collect (ejira--parse-adf-item subitem) into suboutput
                                          finally (return (cl-format nil "~{~a~}" suboutput)))))
      (cond ((adf-type-p "paragraph" item)
             (setf output (append output (cl-loop for subitem across (alist-get 'content item)
                                               collect (ejira--parse-adf-item subitem) into suboutput
                                               finally (return (cl-format nil "~{~a~}~%~%" suboutput))))))
            ((adf-type-p "text" item)
             (cond ((alist-get 'marks item)
                    (let ((startmark)
                          (endmark))
                      (cl-loop for mark across (alist-get 'marks item) do
                            (cond ((adf-type-p "code" mark)
                                   (setf startmark (append startmark (list "+begin_src\n")))
                                   (setf endmark (append (list "\n+end_src") endmark)))
                                  ((adf-type-p "strong" mark)
                                   (setf startmark (append startmark (list "*")))
                                   (setf endmark (append (list "*") endmark)))
                                  ((adf-type-p "underline" mark)
                                   (setf startmark (append startmark (list "_")))
                                   (setf endmark (append (list "_") endmark)))
                                  ((adf-type-p "em" mark)
                                   (setf startmark (append startmark (list "/")))
                                   (setf endmark (append (list "/") endmark)))
                                  ((adf-type-p "strike" mark)
                                   (setf startmark (append startmark (list "+")))
                                   (setf endmark (append (list  "+") endmark)))))
                      (setf output (append output (cl-format nil "~{~a~}~a~{~a~}" startmark (alist-get 'text item) endmark)))))
                   (t 
                    (setf output (append output (alist-get 'text item))))))
            ((adf-type-p "inlineCard" item)
             (setf output (append output (ejira--alist-get item 'inlineCard 'attrs 'url ))))
            ((adf-type-p "bulletList" item)
             (setf output (append output
                                  (cl-loop for subitem across (alist-get 'content item)
                                           collect (ejira--parse-adf-item (alist-get 'content subitem))
                                           into suboutput
                                           finally (return (cl-format nil "~{- ~a~}" suboutput))))))
            ((adf-type-p "hardBreak" item)
             (setf output (append output (cl-format nil "~%~%")))))
      (cl-format nil "~a" output))))

(defun adf-type-p (type item)
  (equal type (alist-get 'type item)))

(provide 'ejira-parser)
;;; ejira-parser.el ends here
