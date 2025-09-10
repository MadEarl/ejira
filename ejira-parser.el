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

(defvar ejira-parser-patterns
      '(

        ;; Code block
        ("^{code\\(?::.*language=\\(?1:[a-z]+\\)\\)?[^}]*}\\(?2:.*\\(?:
.*\\)*?\\)?
?{code}"
         . (lambda ()
             (let ((lang (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               ;; (when (equal lang "none")
                 (setq lang (symbol-name (language-detection-string body)))
                 (when (equal lang "awk")
                   ;; Language-detection seems to fallback to awk. In that case
                   ;; it most likely is not code at all. (Some coworkers like to
                   ;; use source blocks for other things than code...)
                   (setq lang ""))
                 (when (equal lang "emacslisp")
                   (setq lang "elisp"))
                 ;; )
               (prog1
                   (concat
                    "#+BEGIN_SRC " lang "\n"
                    (replace-regexp-in-string "^" "  " body)
                    "\n#+END_SRC")

                 ;; Auto-detecting language alters match data, restore it.
                 (set-match-data md)))))

        ;; Quote block
        ("^{quote}\\(.*\\(?:
.*\\)*?\\)?
?{quote}"
         . (lambda ()
             (let ((body (match-string 1))
                   (md (match-data)))
               (prog1
                   (concat
                    "#+BEGIN_QUOTE\n"
                    (replace-regexp-in-string "^" "  " body)
                    "\n#+END_QUOTE")

                 ;; Auto-detecting language alters match data, restore it.
                 (set-match-data md)))))

        ;; Link to a user
        ("\\[~accountid:\\([a-z0-9:-]*\\)\\]"
         . (lambda ()
             (let* ((account-id (match-string 1))
                    (jira-users (ejira--get-users))
                    (user-fullname (cdr (assoc account-id jira-users))))
               (format "[[~accountid:%s][%s]]"
                       account-id user-fullname))))

        ;; Link
        ("\\[\\(?:\\(.*?\\)\\)?|\\([^\\]*?\\)\\]"
         . (lambda ()
             (let ((url (format "[%s]" (match-string 2)))
                   (placeholder (if (match-string 1)
                                    (format "[%s]" (match-string 1))
                                  "")))
               (format "[%s%s]" url placeholder))))

        ;; Table
        ("\\(^||.*||\\)\\(\\(?:
|.*|\\)*$\\)"
         . (lambda ()
             (let ((header (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               (with-temp-buffer
                 (insert
                  (concat
                   (replace-regexp-in-string "||" "|" header)
                   "\n|"
                   (replace-regexp-in-string
                    "" "-"
                    (make-string (- (s-count-matches "||" header) 1) ?+)
                    nil nil nil 1)
                   "-|"
                   body))
                 (org-table-align)

                 (prog1
                     ;; Get rid of final newline that may be injected by org-table-align
                     (replace-regexp-in-string "\n$" "" (buffer-string))

                   ;; org-table-align modifies match data, restore it.
                   (set-match-data md))))))

        ;; Bullet- or numbered list
        ;; For some reason JIRA sometimes inserts a space in front of the marker.
        ("^ ?\\([#*]+\\) "
         . (lambda ()
             (let* ((prefixes (match-string 1))
                    (level (- (length prefixes) 1))
                    (indent (make-string (max 0 (* 4 level)) ? )))
               ;; Save numbered lists with a placeholder, they will be calculated
               ;; later.
               (concat indent (if (s-ends-with? "#" prefixes) "########" "-") " "))))

        ;; Heading
        ("^h\\([1-6]\\)\\. "
         . (lambda ()
             (concat
              ;; NOTE: Requires dynamic binding to be active.
              (make-string (+ (if (boundp 'jira-to-org--convert-level)
                                  jira-to-org--convert-level
                                0)
                              (string-to-number (match-string 1))) ?*) " ")))

        ;; Escaped curly braces
        ("\\\\{\\([^}]*\\)}"
         . (lambda () (concat "{" (match-string 1) "}")))

        ;; Verbatim text
        ("{{\\(.*?\\)}}"
         . (lambda () (concat "=" (match-string 1) "=")))

        ;; Italic text
        ("\\([^a-z]\\|^\\)_\\(.*?\\)_\\([^a-z]\\|$\\)"
         . (lambda () (concat (match-string 1) "/" (match-string 2) "/" (match-string 3))))

        )
      "Regular expression - replacement pairs used in parsing JIRA markup.")


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


    
;; (defun ejira-parser-jira-to-org (s &optional level)
;;   "Transform JIRA-style string S into org-style.
;; If LEVEL is given, shift all
;; headings to the right by that amount."
;;   (condition-case nil
;;       (let ((backslash-replacement (random-identifier 32))
;;             (percent-replacement (random-identifier 32)))
;;         (with-temp-buffer
;;           (let ((replacements ())
;;                 (jira-to-org--convert-level (or level 0)))
            
;;             ;; Literal backslashes need to be handled separately, they mess up
;;             ;; other regexp patching. They get replaced with the identifier
;;             ;; first, and restored last.
;;             (insert (decode-coding-string
;;                      (s-replace-all
;;                       `(("\\" . ,backslash-replacement)
;;                         ("%"    . ,percent-replacement))
;;                       s)
;;                      'utf-8))
;;             (cl-loop
;;              for (pattern . replacement) in ejira-parser-patterns do
;;              (goto-char (point-min))
;;              (while (re-search-forward pattern nil t)
;;                (let ((identifier (random-identifier 32))
;;                      (rep (funcall replacement)))
;;                  (replace-match identifier)
                 
;;                  ;; Prepend to the list so that the replacements will be applied in
;;                  ;; reverse order.
;;                  (add-to-list 'replacements `(,identifier . ,rep)))))
            
;;             (mapc
;;              (lambda (r)
;;                (goto-char (point-min))
;;                (search-forward (car r))
;;                (replace-match (cdr r)))
;;              replacements)
;;             (goto-char (point-min))
;;             (let ((counters (make-list 6 1)))  ; JIRA Supports 6 levels of headings
;;               (dolist (n (split-string (buffer-string) "\n"))
;;                 (cond ((search-forward-regexp "^\\([[:blank:]]*\\)########"
;;                                               (line-end-position) t)
;;                        (let ((level (/ (length (match-string 1)) 4)))
;;                          (replace-match (format "%s%i." (match-string 1) (nth level counters)))
;;                          (setcar (nthcdr level counters) (1+ (nth level counters)))))
;;                       ((search-forward-regexp "^\\([[:blank:]]*\\)- " (line-end-position) t)
;;                        nil)
;;                       (t (setq counters (make-list 6 1))))
;;                 (forward-line 1)))
;;             (delete-trailing-whitespace))
            
;;           (s-replace-all
;;            `((,backslash-replacement . "\\")
;;              (,percent-replacement   . "%"))
;;            (buffer-string))))
;;     (error s)))

(provide 'ejira-parser)
;;; ejira-parser.el ends here
