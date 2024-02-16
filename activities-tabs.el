;;; activities-tabs.el --- Integrate activities with tabs  -*- lexical-binding: t; -*-

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

;; This library integrates activities with `tab-bar-mode' tabs.

;;; Code:

;;;; Requirements

(require 'activities)

(require 'tab-bar)

;;;; Variables

(defvar activities-tabs-tab-bar-tab-face-function-original nil
  "Records the original value of `tab-bar-tab-face-function'.
When `activities-tabs-mode' is enabled.")

(defvar activities-kill-buffers)

;;;; Customization

(defgroup activities-tabs nil
  "Integrates activities and tabs."
  :group 'activities)

(defcustom activities-tabs-before-resume-functions nil
  "Functions called before resuming an activity.
Each is called with one argument, the activity."
  :type 'hook)

(defcustom activities-tabs-prefix "α:"
  "Prepended to activity names in tabs."
  :type 'string)

(defface activities-tabs
  `((t :inherit font-lock-function-name-face))
  "Applied to tab-bar faces for tabs representing activities.")

;;;; Mode

;;;###autoload
(define-minor-mode activities-tabs-mode
  "Integrate Activities with `tab-bar-mode'.
When active, activities are opened in new tabs and named
accordingly."
  :global t
  :group 'activities
  (let ((override-map '((activities-activity-active-p . activities-tabs-activity-active-p)
                        (activities--set . activities-tabs-activity--set)
                        (activities--switch . activities-tabs--switch)
                        (activities-current . activities-tabs-current)
                        (activities-close . activities-tabs-close))))
    (if activities-tabs-mode
        (progn
          (tab-bar-mode 1)
          (add-hook 'window-configuration-change-hook #'activities-tabs--window-configuration-change)
          (advice-add #'activities-resume :before #'activities-tabs-before-resume)
          (pcase-dolist (`(,symbol . ,function) override-map)
            (advice-add symbol :override function))
          ;; The mode command could be called to activate the mode
          ;; when it already is, in which case we must not swap the
          ;; tab-face-function again, which would discard the actual,
          ;; original value.  (IOW, this must be idempotent.)
          ;; TODO: A way to prevent modes' body forms from being
          ;; reevaluated when they are already active.
          (unless activities-tabs-tab-bar-tab-face-function-original
            (setf activities-tabs-tab-bar-tab-face-function-original tab-bar-tab-face-function
                  tab-bar-tab-face-function #'activities-tabs--tab-bar-tab-face-function)))
      (remove-hook 'window-configuration-change-hook #'activities-tabs--window-configuration-change)
      (advice-remove #'activities-resume #'activities-tabs-before-resume)
      (pcase-dolist (`(,symbol . ,function) override-map)
        (advice-remove symbol function))
      (when activities-tabs-tab-bar-tab-face-function-original
        (setf tab-bar-tab-face-function activities-tabs-tab-bar-tab-face-function-original
              activities-tabs-tab-bar-tab-face-function-original nil)))))

;;;; Functions

(cl-defun activities-tabs-close (activity)
  "Close ACTIVITY.
Its state is not saved, and its frames, windows, and tabs are
closed."
  (activities--switch activity)
  (activities-tabs--kill-buffers)
  (tab-bar-close-tab))

(defun activities-tabs--window-configuration-change ()
  "Add frame's windows' buffers to the current tab's `buffer-list' parameter."
  (cl-assert tab-bar-mode)
  (let ((tab (tab-bar--current-tab-find)))
    (walk-windows (lambda (window)
                    (cl-pushnew (window-buffer window)
                                (alist-get 'activities-buffer-list (cdr tab)))))))

(defun activities-tabs--kill-buffers ()
  ;; TODO: Exclude e.g. special buffers from being killed.
  ;; TODO: Frame parameter name should be prefixed with `activities'.
  "Kill buffers that are only in the current tab's buffer list.
Only does so when `activities-kill-buffers' is non-nil."
  (when activities-kill-buffers
    (let* ((all-tabs (funcall tab-bar-tabs-function))
           (current-tab (tab-bar--current-tab-find))
           (tab-buffers
            (cl-reduce
             (lambda (acc tab)
               (seq-difference acc (activities-tabs--tab-parameter 'activities-buffer-list tab)))
             (remove current-tab all-tabs)
             :initial-value (activities-tabs--tab-parameter 'activities-buffer-list current-tab)))
           (target-buffers (cl-remove-if (lambda (buffer)
                                           (run-hook-with-args-until-success
                                            'activities-anti-kill-predicates buffer))
                                         tab-buffers)))
      (mapc #'kill-buffer target-buffers))))

(defun activities-tabs--switch (activity)
  "Switch to ACTIVITY.
Selects its tab, making one if needed.  Its state is not changed."
  (if-let ((tab (activities-tabs--tab activity)))
      (tab-bar-switch-to-tab (alist-get 'name tab))
    (tab-bar-new-tab))
  (tab-bar-rename-tab (activities-name-for activity)))

(defun activities-tabs--tab (activity)
  "Return ACTIVITY's tab."
  (pcase-let (((cl-struct activities-activity name) activity))
    (cl-find-if (lambda (tab)
                  (when-let ((tab-activity (alist-get 'activity (cdr tab))))
                    (equal name (activities-activity-name tab-activity))))
                (funcall tab-bar-tabs-function))))

(defun activities-tabs-current ()
  "Return current activity."
  (activities-tabs--tab-parameter 'activity (tab-bar--current-tab-find)))

(defun activities-tabs--tab-parameter (parameter tab)
  "Return TAB's PARAMETER."
  ;; TODO: Make this a gv.
  (alist-get parameter (cdr tab)))

(defun activities-tabs--tab-bar-tab-face-function (tab)
  "Return a face for TAB.
If TAB represents an activity, face `activities-tabs' is added as
inherited."
  ;; TODO: Propose a tab-bar equivalent of `tab-line-tab-face-functions'.
  (let ((face (funcall activities-tabs-tab-bar-tab-face-function-original tab)))
    (if (activities-tabs--tab-parameter 'activity tab)
        `(:inherit (activities-tabs ,face))
      face)))

(defun activities-tabs-activity--set (activity)
  "Set the current activity.
Sets the current tab's `activity' parameter to ACTIVITY."
  (let ((tab (tab-bar--current-tab-find)))
    (setf (alist-get 'activity (cdr tab)) activity)))

(defun activities-tabs-activity-active-p (activity)
  "Return non-nil if ACTIVITY is active.
That is, if any tabs have an `activity' parameter whose
activity's name is NAME."
  (activities-tabs--tab activity))

(defun activities-tabs-before-resume (activity &rest _)
  "Called before resuming ACTIVITY."
  (run-hook-with-args 'activities-tabs-before-resume-functions activity))

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

(provide 'activities-tabs)

;;; activities-tabs.el ends here
