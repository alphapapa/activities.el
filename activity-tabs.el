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

(require 'tab-bar)

;;;; Customization

(defgroup activity-tabs nil
  "Integrates activities and tabs."
  :group 'activity)

(defcustom activity-tabs-before-resume-functions nil
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
        (advice-add #'activity-active-p :override #'activity-tabs-activity-active-p)
        (advice-add #'activity--set :override #'activity-tabs-activity--set)
        (advice-add #'activity-switch :override #'activity-tabs-switch)
        (advice-add #'activity-current :override #'activity-tabs-current))
    (advice-remove #'activity-resume #'activity-tabs-before-resume)
    (advice-remove #'activity-active-p #'activity-tabs-activity-active-p)
    (advice-remove #'activity--set #'activity-tabs-activity--set)
    (advice-remove #'activity-switch #'activity-tabs-switch)
    (advice-remove #'activity-current #'activity-tabs-current)))

;;;; Functions

(defun activity-tabs-switch (activity)
  "Switch to ACTIVITY.
Selects its tab, making one if needed.  Its state is not changed."
  (if-let ((tab (activity-tabs--tab activity)))
      (tab-bar-switch-to-tab (alist-get 'name tab))
    (tab-bar-new-tab)
    (tab-bar-rename-tab (activity-name-for activity))))

(defun activity-tabs--tab (activity)
  "Return ACTIVITY's tab."
  (pcase-let (((cl-struct activity name) activity))
    (cl-find-if (lambda (tab)
                  (when-let ((tab-activity (alist-get 'activity (cdr tab))))
                    (equal name (activity-name tab-activity))))
                (funcall tab-bar-tabs-function))))

(defun activity-tabs-current ()
  "Return current activity."
  (activity-tabs--tab-parameter 'activity (tab-bar--current-tab-find)))

(defun activity-tabs--tab-parameter (parameter tab)
  "Return TAB's PARAMETER."
  ;; TODO: Make this a gv.
  (alist-get parameter (cdr tab)))

(defun activity-tabs-activity--set (activity)
  "Set the current activity.
Sets the current tab's `activity' parameter to ACTIVITY."
  (let ((tab (tab-bar--current-tab-find)))
    (setf (alist-get 'activity (cdr tab)) activity)))

(defun activity-tabs-activity-active-p (activity)
  "Return non-nil if ACTIVITY is active.
That is, if any tabs have an `activity' parameter whose
activity's name is NAME."
  (activity-tabs--tab activity))

(defun activity-tabs-before-resume (activity &rest _)
  "Called before resuming ACTIVITY."
  (run-hook-with-args 'activity-tabs-before-resume-functions activity))

;; (defun activity-tabs-switch-to-tab (activity)
;;   "Switch to a tab for ACTIVITY."
;;   (pcase-let* (((cl-struct activity name) activity)
;;                (tab (cl-find-if (lambda (tab)
;;                                   (when-let ((tab-activity (alist-get 'activity tab)))
;;                                     (equal name (activity-name tab-activity))))
;;                                 (funcall tab-bar-tabs-function))) 
;;                (tab-name (if tab
;;                              (alist-get 'name tab)
;;                            (concat activity-tabs-prefix
;;                                    (string-remove-prefix activity-bookmark-prefix name)))))
;;     (tab-bar-switch-to-tab tab-name)))

;;;; Footer

(provide 'activity-tabs)

;;; activity-tabs.el ends here
