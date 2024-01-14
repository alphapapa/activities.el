;;; activity-tabs.el --- Integrate activities with tabs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))

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

;; This library integrates activities with `tab-bar-mode' tabs.

;;; Code:

;;;; Requirements

(require 'activity)

;;;; Customization

(defgroup activity-tabs nil
  "Integrates activities and tabs."
  :group 'activity)

(defcustom activity-tabs-before-resume-functions
  '(activity-tabs-switch-to-tab)
  "Functions called before resuming an activity.
Each is called with one argument, the activity."
  :type 'hook)

(defcustom activity-tabs-prefix "α:"
  "Prepended to activity names in tabs."
  :type 'string)

;;;; Mode

;;;###autoload
(define-minor-mode activity-tabs-mode
  "Integrate Activity with `tab-bar-mode'.
When active, activities are opened in new tabs and named
accordingly."
  :global t
  :group 'activity
  (if activity-tabs-mode
      (progn
        (tab-bar-mode 1)
        (advice-add #'activity-resume :before #'activity-tabs-before-resume)
        (advice-add #'activity-active-p :override #'activity-tabs-activity-active-p))
    (advice-remove #'activity-resume #'activity-tabs-before-resume)
    (advice-remove #'activity-active-p #'activity-tabs-activity-active-p)))

;;;; Functions

(defun activity-tabs-activity-active-p (activity)
  "Return non-nil if ACTIVITY is active.
That is, if any tabs have an `activity' parameter whose
activity's name is NAME."
  (pcase-let (((cl-struct activity name) activity))
    (cl-some (lambda (tab)
               (when-let ((activity (alist-get 'activity tab)))
                 (equal name (activity-name activity))))
             (funcall tab-bar-tabs-function))))

(defun activity-tabs-before-resume (activity &rest _)
  "Called before resuming ACTIVITY."
  (run-hook-with-args 'activity-tabs-before-resume-functions activity))

(defun activity-tabs-switch-to-tab (activity)
  "Switch to a tab for ACTIVITY."
  (pcase-let* (((cl-struct activity name) activity)
               (name (string-remove-prefix activity-bookmark-prefix name))
               (tab-name (concat activity-tabs-prefix name)))
    (tab-bar-switch-to-tab tab-name)))

;;;; Footer

(provide 'activity-tabs)

;;; activity-tabs.el ends here
