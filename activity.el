;;; activity.el --- Manage, switch between, and suspend/resume sets of windows, frames, and buffers  -*- lexical-binding: t; -*-

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

;;;; Requirements

(require 'cl-lib)
(require 'bookmark)
(require 'map)
(require 'subr-x)

;;;; Variables

(defvar activity-completing-read-history nil
  "History for `activity-completing-read'.")

(defvar activity-window-parameters-translators
  `((window-preserved-size
     (serialize . ,(pcase-lambda (`(,buffer ,direction ,size))
                     `(,(buffer-name buffer) ,direction ,size)))
     (deserialize . ,(pcase-lambda (`(,buffer-name ,direction ,size))
                       `(,(get-buffer buffer-name) ,direction ,size)))))
  "Functions used to serialize and deserialize certain window parameters.
For example, the value of `window-preserved-size' includes a
buffer, which must be serialized to a buffer name, and then
deserialized back to the buffer after it is reincarnated.")

;;;; Customization

(defgroup activity nil
  "Activities."
  :link '(emacs-commentary-link "activity")
  :link '(url-link "https://github.com/alphapapa/activity.el")
  :group 'convenience)

(defcustom activity-bookmark-prefix "Activity: "
  "Prefix applied to activity bookmark names."
  :type 'string)

(defcustom activity-window-persistent-parameters
  (list (cons 'header-line-format 'writable)
        (cons 'mode-line-format 'writable)
        (cons 'tab-line-format 'writable)
        (cons 'no-other-window 'writable)
        (cons 'no-delete-other-windows 'writable)
        (cons 'window-preserved-size 'writable)
        (cons 'window-side 'writable)
        (cons 'window-slot 'writable))
  "Additional window parameters to persist.
See Info node `(elisp)Window Parameters'.  See also option
`activity-set-window-persistent-parameters'."
  :type '(alist :key-type (symbol :tag "Window parameter")
                :value-type (choice (const :tag "Not saved" nil)
                                    (const :tag "Saved" writable))))

(cl-defstruct activity
  "FIXME: Docstring."
  name default-state last-state etc)

(cl-defstruct activity-state
  "FIXME: Docstring."
  window-state etc)

;;;; Commands

(defun activity-save (name)
  "Save activity as NAME."
  (interactive (list (activity-completing-read :prompt "Save activity as: ")))
  (let ((record `((handler . activity-bookmark-handler)
                  (activity . ,(make-activity :name name :default-state (activity-state))))))
    (bookmark-store name record nil)))

;;;; Functions

(defun activity-state ()
  "Return the current activity's state."
  (make-activity-state
   :window-state (activity--window-state (selected-frame))))

(defun activity--window-state (frame)
  "Return FRAME's window state."
  (let* ((window-persistent-parameters (append activity-window-persistent-parameters
                                               window-persistent-parameters))
         (window-state (with-selected-frame frame
                         (window-state-get nil 'writable))))
    (activity--window-serialized window-state)))

(defun activity--window-serialized (state)
  "Return window STATE having serialized its parameters."
  (cl-labels ((translate-state (state)
                "Set windows' buffers in STATE."
                (pcase state
                  (`(leaf . ,_attrs) (translate-leaf state))
                  ((pred atom) state)
                  (`(,_key . ,(pred atom)) state)
                  ((pred list) (mapcar #'translate-state state))))
              (translate-leaf (leaf)
                "Translate window parameters in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters) attrs))
                  (pcase-dolist (`(,parameter . ,(map serialize))
                                 activity-window-parameters-translators)
                    (when (map-elt parameters parameter)
                      (setf (map-elt parameters parameter)
                            (funcall serialize (map-elt parameters parameter)))))
                  (setf (map-elt attrs 'parameters) parameters)
                  (cons 'leaf attrs))))
    (translate-state state)))

(defun activity--windows-set (config)
  "Set window configuration according to CONFIG."
  (setf window-persistent-parameters (copy-sequence activity-window-persistent-parameters))
  (pcase-let* ((window-persistent-parameters (append activity-window-persistent-parameters
                                                     window-persistent-parameters))
               (state (activity--bufferize-window-state state)))
    ;; HACK: Since `bookmark--jump-via' insists on calling a buffer-display
    ;; function after handling the bookmark, we use an immediate timer to
    ;; set the window configuration.
    (run-at-time nil nil (lambda ()
                           (window-state-put state (frame-root-window))))))

(defun activity--bufferize-window-state (state)
  "Return window state STATE with its buffers reincarnated."
  (cl-labels ((bufferize-state (state)
                "Set windows' buffers in STATE."
                (pcase state
                  (`(leaf . ,_attrs) (translate-leaf (bufferize-leaf state)))
                  ((pred atom) state)
                  (`(,_key . ,(pred atom)) state)
                  ((pred list) (mapcar #'bufferize-state state))))
              (bufferize-leaf (leaf)
                "Recreate buffers in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters buffer) attrs)
                             ((map activity-buffer-record) parameters)
                             (`(,_buffer-name . ,buffer-attrs) buffer)
                             (new-buffer (activity--buffer-for activity-buffer-record)))
                  (setf (map-elt attrs 'buffer) (cons new-buffer buffer-attrs))
                  (cons 'leaf attrs)))
              (translate-leaf (leaf)
                "Translate window parameters in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters) attrs))
                  (pcase-dolist (`(,parameter . ,(map deserialize))
                                 activity-window-parameters-translators)
                    (when (map-elt parameters parameter)
                      (setf (map-elt parameters parameter)
                            (funcall deserialize (map-elt parameters parameter)))))
                  (setf (map-elt attrs 'parameters) parameters)
                  (cons 'leaf attrs))))
    (if-let ((leaf-pos (cl-position 'leaf state)))
        ;; A one-window frame: the elements following `leaf' are that window's params.
        (append (cl-subseq state 0 leaf-pos)
                (translate-leaf (bufferize-leaf (cl-subseq state leaf-pos))))
      ;; Multi-window frame.
      (bufferize-state state))))

(cl-defstruct activity-buffer
  "FIXME: Docstring."
  (bookmark nil :documentation "Bookmark record")
  (filename nil :documentation "Filename, if file-backed")
  (name nil :documentation "Buffer name")
  (etc nil :documentation "Alist for other data."))

(defun activity--buffer-for (record)
  "Return buffer for activity buffer RECORD."
  (pcase-let (((cl-struct activity-buffer bookmark filename name) record))
    (cond (bookmark (activity--bookmark-buffer record))
          (filename (activity--filename-buffer record))
          (name (activity--name-buffer record))
          (t (error "Activity record is invalid: %S")))))

(defun activity--bookmark-buffer (record)
  "Return buffer for bookmark RECORD."
  ;; NOTE: Be aware of the following note from burly.el:
  ;; NOTE: Due to changes in help-mode.el which serialize natively
  ;; compiled subrs in the bookmark record, which cannot be read
  ;; back (which actually break the entire bookmark system when
  ;; such a record is saved in the bookmarks file), we have to
  ;; workaround a failure to read here.  See bug#56643.
  (pcase-let* (((cl-struct activity-buffer bookmark) record))
    (save-window-excursion
      (condition-case err
          (progn
            (bookmark-jump record)
            (when-let ((local-variable-map
                        (bookmark-prop-get bookmark 'activity-buffer-local-variables)))
              (cl-loop for (variable . value) in local-variable-map
                       do (setf (buffer-local-value variable (current-buffer)) value))))
        (error (delay-warning 'activity
                              (format "Error while opening bookmark: ERROR:%S  RECORD:%S" err record))))
      (current-buffer))))

(defcustom activity-major-mode-alist
  (list (cons 'org-mode
              (list (cons 'make-url-fn #'activity--org-mode-buffer-url)
                    (cons 'follow-url-fn #'activity-follow-url-org-mode))))
  "Alist mapping major modes to the appropriate Activity functions."
  :type '(alist :key-type symbol
                :value-type (set (cons (const make-url-fn) (function :tag "Make-URL function"))
                                 (cons (const follow-url-fn) (function :tag "Follow-URL function")))))

(defun activity--filename-buffer (record)
  "Return buffer for filename RECORD."
  (pcase-let* (((cl-struct activity-buffer filename) record)
               (buffer (find-file-noselect filename))
               (major-mode (buffer-local-value 'major-mode buffer))
               (follow-fn (map-nested-elt activity-major-mode-alist (list major-mode 'follow-url-fn))))
    (cl-assert follow-fn nil "Major mode not in `activity-major-mode-alist': %s" major-mode)
    (funcall follow-fn :buffer buffer :record record)))

(defun activity--name-buffer (record)
  "Return buffer for name RECORD."
  (pcase-let (((cl-struct activity-buffer name) record))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create (concat "*Activity (error): " name "*"))
          (insert "Activity was unable to get a buffer named: " name "\n"
                  "Record: " (format "%S" record) "\n"
                  "Please report this error to the developer\n\n")
          (current-buffer)))))

(cl-defun activity-completing-read (&key (prompt "Open activity: "))
  "Return an activity name read with completion.
PROMPT is passed to `completing-read', which see."
  (completing-read prompt (activity-names)
		   nil nil activity-bookmark-prefix activity-completing-read-history))

(defun activity-activities ()
  "Return list of activities."
  (bookmark-maybe-load-default-file)
  (cl-remove-if-not (pcase-lambda (`(,_name . ,(map handler)))
                      (equal #'activity-bookmark-handler handler))
                    bookmark-alist))

(cl-defun activity-names (&optional (activities (activity-activities)))
  "Return list of names of ACTIVITIES."
  (thread-last activities
               (mapcar #'car)
               (mapcar (lambda (name)
                         (string-remove-prefix activity-bookmark-prefix name)))))

(defun activity-bookmark-handler (bookmark)
  "Switch to BOOKMARK's activity.")

;;;; Footer

(provide 'activity)

;;; activity.el ends here
