;;; activities-list.el --- List activities           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

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

;; This library implements a list view for activities.

;;; Code:

(require 'map)
(require 'vtable)

(require 'activities)

(defgroup activities-list nil
  "Activities list buffer."
  :group 'activities)

(defcustom activities-list-time-format "%Y-%m-%d %H:%M:%S"
  "Time format for `activities-list' buffer."
  :type 'string)

(defmacro activities-list-command (command)
  "Expand to a lambda that applies its args to COMMAND and reverts the list buffer."
  `(lambda (&rest args)
     (let ((list-buffer (current-buffer)))
       (apply #',command args)
       (with-current-buffer list-buffer
         (vtable-revert-command)))))

;;;###autoload
(defun activities-list ()
  "List activities."
  (interactive)
  (unless activities-activities
    (user-error (substitute-command-keys "No activities defined (to define one, type \"\\[activities-new]\")")))
  (with-current-buffer (get-buffer-create "*Activities*")
    (let ((inhibit-read-only t))
      (read-only-mode)
      (erase-buffer)
      (make-vtable
       :columns
       `(( :name "Active" :primary descend
           :getter (lambda (object _table)
                     (if (activities-activity-active-p object)
                         "*" " ")))
         ( :name "Name"
           :getter (lambda (object _table)
                     (activities-activity-name object)))
         ( :name "Last saved"
           :getter (lambda (activity _table)
                     (pcase-let (((cl-struct activities-activity last) activity))
                       (when last
                         (map-elt (activities-activity-state-etc last) 'time))))
           :formatter activities-list--format-time)
         ( :name "Default saved"
           :getter (lambda (activity _table)
                     (pcase-let (((cl-struct activities-activity default) activity))
                       (when default
                         (map-elt (activities-activity-state-etc default) 'time))))
           :formatter activities-list--format-time))
       :objects-function (lambda ()
                           (map-values activities-activities))
       :sort-by '((2 . descend) (0 . descend))
       :actions `("q" (lambda (&rest _) (bury-buffer))
                  "n" (lambda (&rest _) (forward-line 1))
                  "p" (lambda (&rest _) (forward-line -1))
                  "RET" ,(activities-list-command activities-resume)
                  "k" ,(activities-list-command activities-kill)
                  "s" ,(activities-list-command activities-suspend)
                  "D" ,(activities-list-command activities-discard)))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defalias 'list-activities #'activities-list)

(defun activities-list--format-time (time)
  "Return TIME formatted according to `activities-list-time-format', which see.."
  (if time
      (format-time-string activities-list-time-format time)
    "never"))

;;;; Footer

(provide 'activities-list)

;;; activities-list.el ends here
