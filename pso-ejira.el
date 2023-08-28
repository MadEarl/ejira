;;; pso-ejira.el --- PSO additions                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Wolfgang Mederle

;; Author: Wolfgang Mederle <wolfgang.mederle@intrafind.de>
;; Keywords: data, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; IntraFind additions

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'dash)
(require 'ejira-core)

;;;###autoload
(defun ejira-create-item-under-point ()
  "Create issue based on complete ejira-issue sans ID."
  (interactive)
  (let* ((item (ejira-get-id-under-point))
         (assignee "Unassigned")
         (summary (ejira--with-point-on (nth 1 item) (ejira--strip-properties (org-get-heading t t t t))))
         (properties (save-excursion
                       (goto-char (nth 2 item))
                       (org-entry-properties)))
         (issuetype (cdr (assoc "Issuetype" properties)))
         (type (cdr (assoc "TYPE" properties)))
         (project (cdr (assoc "CATEGORY" properties)))
         (description (ejira-parser-org-to-jira
                       (ejira--get-heading-body
                        (ejira--find-task-subheading (nth 1 item) ejira-description-heading-name))))
         (response (cond ((string= type "ejira-epic")
                          (jiralib2-create-issue
                           project
                           issuetype
                           summary
                           description
                           `(assignee . ,assignee)
                           `(customfield_10011 . ,summary))) ; Epic Name
                         (t
                          (jiralib2-create-issue
                           project
                           issuetype
                           summary
                           description
                           `(assignee . ,assignee))))))
    (org-entry-put nil "ID" (cdr (assoc 'key response)))
    (org-entry-put nil "URL" (cdr (assoc 'self response)))))

;;;###autoload
(defun ejira-if-plan-issue ()
  (interactive)
  (let* ((item (ejira-get-id-under-point))
         (startdate (read-string "Startdatum: " (org-read-date nil nil "++mon")))
         (properties (save-excursion
                       (goto-char (nth 2 item))
                       (org-entry-properties)))
         (effort (read-string "Effort: "
                              (cond ((cdr (assoc "EFFORT" properties))
                                     (cl-multiple-value-bind (hours minutes) (split-string (cdr (assoc "EFFORT" properties)) ":")
                                       (if (string= minutes "00")
                                           (concat hours "h")
                                         (concat hours "h " minutes "m"))))
                                     (t "0h")))))
    (jiralib2-if-plan-issue (nth 1 item) startdate effort)
    (ejira--update-task (nth 1 item))))

;;;###autoload
(defun ejira-if-unplan-issue ()
  (interactive)
  (let ((item (ejira-get-id-under-point)))
    (jiralib2-if-unplan-issue (nth 1 item))
    (ejira--update-task (nth 1 item))))



(provide 'pso-ejira)
;;; pso-ejira.el ends here
