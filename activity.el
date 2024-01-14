;;; activity.el --- Manage, switch between, and suspend/resume sets of windows, frames, and buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience
;; Version: 0.1-pre

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

;; Inspired by Genera's and KDE's concepts of "activities", this
;; library allows the user to select an "activity", the loading of
;; which restores a window configuration and/or frameset, along with
;; the buffers shown in each window.  Saving an activity saves the
;; state for later restoration.  Switching away from an activity saves
;; the last-used state for later switching back to, while still
;; allowing the activity's initial or default state to be restored on
;; demand.  Restoring an activity loads the last-used state, or the
;; initial/default state when a universal argument is provided.

;; The implementation uses the bookmark system to save buffers'
;; states--that is, any major mode that supports the bookmark system
;; is compatible.  A buffer whose major mode does not support the
;; bookmark system (or does not support it well enough to restore
;; useful state) is not compatible and can't be fully restored, or
;; perhaps not at all; but solving that is as simple as implementing
;; bookmark support for the mode, which is usually trivial.

;; Integration with Emacs's `tab-bar-mode' is provided: a window
;; configuration or frameset can be restored to a window or set of
;; frames, or to a tab or set of tabs.

;; Various hooks are provided, both globally and per-activity, so that
;; the user can define functions to be called when an activity is
;; saved, restored, or switched from/to.  For example, this could be
;; used to limit the set of buffers offered for switching to within an
;; activity, or to track the time spent in an activity.

;;; Code:

;;;; Customization

(defgroup activity nil
  "Activities."
  :link '(emacs-commentary-link "activity")
  :link '(url-link "https://github.com/alphapapa/activity.el")
  :group 'convenience)

;;;; Footer

(provide 'activity)

;;; activity.el ends here
