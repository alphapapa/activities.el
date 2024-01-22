;;; activity.el --- Suspend/resume sets of windows, frames, and buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1") (persist "0.5"))

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
(require 'persist)
(require 'subr-x)

;;;; Debugging

(require 'warnings)

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load'
;; the file to enable `activity-debug' messages.  This is commented
;; out by default because, even though the messages are only displayed
;; when `warning-minimum-log-level' is `:debug' at runtime, if that is
;; so at expansion time, the expanded macro calls format the message
;; and check the log level at runtime, which is not zero-cost.

;; (eval-and-compile (setq-local warning-minimum-log-level nil) (setq-local warning-minimum-log-level :debug))

(cl-defmacro activity-debug (&rest args)
  "Display a debug warning showing the runtime value of ARGS.
The warning automatically includes the name of the containing
function, and it is only displayed if `warning-minimum-log-level'
is `:debug' at expansion time (otherwise the macro expands to a
call to `ignore' with ARGS and is eliminated by the
byte-compiler).  When debugging, the form also returns nil so,
e.g. it may be used in a conditional in place of nil.

Each of ARGS may be a string, which is displayed as-is, or a
symbol, the value of which is displayed prefixed by its name, or
a Lisp form, which is displayed prefixed by its first symbol.

Before the actual ARGS arguments, you can write keyword
arguments, i.e. alternating keywords and values.  The following
keywords are supported:

  :buffer BUFFER   Name of buffer to pass to `display-warning'.
  :level  LEVEL    Level passed to `display-warning', which see.
                   Default is :debug."
  ;; TODO: Can we use a compiler macro to handle this more elegantly?
  (pcase-let* ((fn-name (when byte-compile-current-buffer
                          (with-current-buffer byte-compile-current-buffer
                            ;; This is a hack, but a nifty one.
                            (save-excursion
                              (beginning-of-defun)
                              (cl-second (read (current-buffer)))))))
               (plist-args (cl-loop while (keywordp (car args))
                                    collect (pop args)
                                    collect (pop args)))
               ((map (:buffer buffer) (:level level)) plist-args)
               (level (or level :debug))
               (string (cl-loop for arg in args
                                concat (pcase arg
                                         ((pred stringp) "%S ")
                                         ((pred symbolp)
                                          (concat (upcase (symbol-name arg)) ":%S "))
                                         ((pred listp)
                                          (concat "(" (upcase (symbol-name (car arg)))
                                                  (pcase (length arg)
                                                    (1 ")")
                                                    (_ "...)"))
                                                  ":%S "))))))
    (if (eq :debug warning-minimum-log-level)
        `(let ((fn-name ,(if fn-name
                             `',fn-name
                           ;; In an interpreted function: use `backtrace-frame' to get the
                           ;; function name (we have to use a little hackery to figure out
                           ;; how far up the frame to look, but this seems to work).
                           `(cl-loop for frame in (backtrace-frames)
                                     for fn = (cl-second frame)
                                     when (not (or (subrp fn)
                                                   (special-form-p fn)
                                                   (eq 'backtrace-frames fn)))
                                     return (make-symbol (format "%s [interpreted]" fn))))))
           (display-warning fn-name (format ,string ,@args) ,level ,buffer)
           nil)
      `(ignore ,@args))))

;;;; Macros

(defmacro activity-with (activity &rest body)
  "Evaluate BODY with ACTIVITY active.
Selects ACTIVITY's frame/tab and then switches back."
  (declare (indent defun) (debug (sexp body)))
  (let ((original-state-var (gensym)))
    `(let ((,original-state-var `( :frame ,(selected-frame)
                                   :window ,(selected-window)
                                   :tab-index ,(when (bound-and-true-p tab-bar-mode)
                                                 (tab-bar--current-tab-index)))))
       (unless (activity-active-p ,activity)
         (error "Activity %S not active" (activity-name ,activity)))
       (unwind-protect
           (progn
             (activity-switch ,activity)
             ,@body)
         (pcase-let (((map :frame :window :tab-index) ,original-state-var))
           (when frame
             (select-frame frame))
           (when tab-index
             (tab-bar-select-tab (1+ tab-index)))
           (when window
             (select-window window)))))))

;;;; Variables

(with-demoted-errors "activity: Variable `activity-activities' failed to load persisted data: %S"
  (persist-defvar activity-activities nil "FIXME: Docstring."))

(defvar activity-buffer-local-variables nil
  "Variables whose value are saved and restored by activities.
Intended to be bound around code calling `activity-' commands.")

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

(defcustom activity-always-persist t
  "Always persist activity states to disk when saving.
When disabled, only persist them when exiting Emacs or disabling
`activity-mode'.

Generally, leaving this enabled should be fine.  However, in case
of unusual bugs, it could be helpful to only save upon exiting
Emacs, so that any unusual state that caused a crash would not be
persisted."
  :type 'boolean)

(defcustom activity-name-prefix "α: "
  "Prefix applied to activity names in frames/tabs."
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

(defcustom activity-after-resume-functions nil
  "Functions called after resuming an activity.
Called with one argument, the activity."
  :type 'hook)

(defcustom activity-before-resume-functions nil
  "Functions called before resuming an activity.
Called with one argument, the activity."
  :type 'hook)

(cl-defstruct activity
  "FIXME: Docstring."
  name default last etc)

(cl-defstruct activity-state
  "FIXME: Docstring."
  window-state etc)

;;;; Commands

(defun activity-new (name)
  "Save current state as a new activity with NAME."
  ;; Not sure if this is needed, but let's experiment.
  (interactive (list (read-string "New activity name: ")))
  (when (member name (activity-names))
    (user-error "Activity named %S already exists" name))
  ;; FIXME: Rename tab/frame to have prefix.
  (let ((activity (make-activity :name name)))
    (activity--set activity)
    (activity-save activity :defaultp t :lastp t)
    activity))

(cl-defun activity-resume (activity &key resetp)
  "Resume ACTIVITY.
If RESETP (interactively, with universal prefix), reset to
ACTIVITY's default state; otherwise, resume its last state, if
available."
  (interactive (list (activity-completing-read) :resetp current-prefix-arg))
  (activity-switch activity)
  (activity-set activity :state (if resetp 'default 'last)))

(defun activity-suspend (activity)
  "Suspend ACTIVITY.
Its last is saved, and its frames, windows, and tabs are
closed."
  (interactive (list (activity-completing-read :prompt "Suspend activity: ")))
  (activity-save activity :lastp t)
  (activity-close activity))

(cl-defun activity-save (activity &key defaultp lastp persistp)
  "Save states of ACTIVITY.
If DEFAULTP, save its default state; if LASTP, its last.  If
PERSISTP, force persisting of data (otherwise, data is persisted
according to option `activity-always-persist', which see)."
  (unless (or defaultp lastp)
    (user-error "Neither DEFAULTP nor LASTP specified"))
  (activity-with activity
    (pcase-let* (((cl-struct activity name default last) activity)
                 (new-state (activity-state)))
      (setf (activity-default activity) (if (or defaultp (not default)) new-state default)
            (activity-last activity) (if (or lastp (not last)) new-state last)
            (map-elt activity-activities name) activity)))
  (activity--persist persistp))

(defun activity-save-all ()
  "Save all active activities' last states.
In order to be safe for `kill-emacs-hook', this demotes errors."
  (interactive)
  (with-demoted-errors "activity-save-all: ERROR: %S"
    (dolist (activity (cl-remove-if-not #'activity-active-p (map-values activity-activities)))
      (activity-save activity :lastp t))))

(defun activity-reset (activity)
  "Reset ACTIVITY to its default state."
  (interactive (list (activity-current)))
  (unless activity
    (user-error "No active activity"))
  (activity-set activity :state 'default))

(defun activity-discard (activity)
  "Discard ACTIVITY and its state.
It will not be recoverable."
  (interactive (list (activity-completing-read :prompt "Discard activity: ")))
  (ignore-errors
    ;; FIXME: After fixing all the bugs, remove ignore-errors.
    (activity-close activity))
  (setf activity-activities (map-delete activity-activities (activity-name activity))))

;;;; Activity mode

;; This mode automatically saves active activities.

(defvar activity-mode-timer nil
  "Automatically saves activities according to `activity-mode-idle-frequency'.")

(defgroup activity-mode nil
  "Automatically save activities."
  :group 'activity)

(defcustom activity-mode-idle-frequency 5
  "Automatically save activities when Emacs has been idle this many seconds."
  :type 'natnum)

;;;###autoload
(define-minor-mode activity-mode
  "Automatically remember activities' state.
accordingly."
  :global t
  :group 'activity
  (if activity-mode
      (progn
        (setf activity-mode-timer
              (run-with-idle-timer activity-mode-idle-frequency t #'activity-save-all))
        (add-hook 'kill-emacs-hook #'activity-mode--killing-emacs))
    (when (timerp activity-mode-timer)
      (cancel-timer activity-mode-timer)
      (setf activity-mode-timer nil))
    (remove-hook 'kill-emacs-hook #'activity-mode--killing-emacs)))

(defun activity-mode--killing-emacs ()
  "Persist all activities' states.
To be called from `kill-emacs-hook'."
  (let ((activity-always-persist t))
    (activity-save-all)))

;;;; Functions

(cl-defun activity-set (activity &key (state 'last))
  "Set ACTIVITY as the current one.
Its STATE (`last' or `default') is loaded into the current frame."
  (activity--set activity)
  (activity-with activity
    (pcase-let (((cl-struct activity name default last) activity))
      (pcase state
        ('default (activity--windows-set (activity-state-window-state default)))
        ('last (if last
                   (activity--windows-set (activity-state-window-state last))
                 (activity--windows-set (activity-state-window-state default))
                 (message "Activity %S has no last state.  Resuming default." name)))))))

(defun activity--set (activity)
  "Set current frame's activity parameter to ACTIVITY."
  (set-frame-parameter nil 'activity activity))

(defun activity--persist (&optional forcep)
  "Persist `activity-activities' to disk if enabled or FORCEP.
See option `activity-always-persist'."
  (when (or forcep activity-always-persist)
    (persist-save 'activity-activities)))

(defun activity-current ()
  "Return the current activity."
  (frame-parameter nil 'activity))

(cl-defun activity-close (activity)
  "Close ACTIVITY.
Its state is not saved, and its frames, windows, and tabs are
closed."
  (pcase-let* (((cl-struct activity name) activity)
               (frame (cl-find-if
                       (lambda (frame)
                         (equal name (activity-name (frame-parameter frame 'activity))))
                       (frame-list))))
    ;; TODO: Set frame parameter when resuming.
    (unless (length= 1 (frame-list))
      ;; Not only frame: delete it.
      (delete-frame frame))))

(defun activity-named (name)
  "Return activity having NAME."
  (map-elt activity-activities name))

(defun activity-switch (activity)
  "Switch to ACTIVITY.
Select's ACTIVITY's frame, making a new one if needed.  Its state
is not changed."
  (select-frame (or (activity--frame activity)
                    (make-frame `((activity . ,activity)))))
  (set-frame-name (activity-name-for activity))
  ;; Not sure if calling `redisplay' is necessary.
  (redisplay))

(defun activity--frame (activity)
  "Return ACTIVITY's frame."
  (pcase-let (((cl-struct activity name) activity))
    (cl-find-if (lambda (frame)
                  (when-let ((frame-activity (frame-parameter frame 'activity)))
                    (equal name (activity-name frame-activity))))
                (frame-list))))

(defun activity-state ()
  "Return an activity state for the current frame."
  (make-activity-state
   :window-state (activity--window-state (selected-frame))))

(defun activity-active-p (activity)
  "Return non-nil if ACTIVITY is active.
That is, if any frames have an `activity' parameter whose
activity's name is NAME."
  (activity--frame activity))

(defun activity--window-state (frame)
  "Return FRAME's window state."
  (with-selected-frame frame
    ;; Set window parameter.
    ;; (mapc (lambda (window)
    ;;         (let ((value (activity--serialize (window-buffer window))))
    ;;           (set-window-parameter window 'activity-buffer value)))
    ;;       (window-list))
    (let* ((window-persistent-parameters (append activity-window-persistent-parameters
                                                 window-persistent-parameters))
           (window-state (window-state-get nil 'writable)))
      ;; Clear window parameters we set (because they aren't kept
      ;; current, so leaving them could be confusing).
      ;; (mapc (lambda (window)
      ;;         (set-window-parameter window 'activity-buffer nil))
      ;;       (window-list))
      (activity--window-serialized window-state))))

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
                             ((map parameters ('buffer `(,buffer-or-buffer-name . ,_buffer-attrs))) attrs))
                  (setf (map-elt parameters 'activity-buffer)
                        ;; HACK: Set buffer props parameter (maybe not the "right" place).
                        (activity--serialize (get-buffer (car (map-elt attrs 'buffer))))
                        ;; HACK: Replace unserializable buffer (though `window-state-get'
                        ;; should be filtering that out...).  NOTE: We must include the
                        ;; "buffr-attrs" as-is.
                        (car (map-elt attrs 'buffer)) (buffer-name (get-buffer buffer-or-buffer-name)))
                  (pcase-dolist (`(,parameter . ,(map serialize))
                                 activity-window-parameters-translators)
                    (when (map-elt parameters parameter)
                      (setf (map-elt parameters parameter)
                            (funcall serialize (map-elt parameters parameter)))))
                  (setf (map-elt attrs 'parameters) parameters)
                  (cons 'leaf attrs))))
    (translate-state state)))

(defun activity--windows-set (state)
  "Set window configuration according to STATE."
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
                             ((map activity-buffer) parameters)
                             (`(,_buffer-name . ,buffer-attrs) buffer)
                             (new-buffer (activity--deserialize activity-buffer)))
                  (activity-debug activity-buffer new-buffer)
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
  (bookmark nil :documentation "Bookmark props")
  (filename nil :documentation "Filename, if file-backed")
  (name nil :documentation "Buffer name")
  (local-variables nil)
  (etc nil :documentation "Alist for other data."))

(cl-defmethod activity--serialize ((buffer buffer))
  "Return `activity-buffer' struct for BUFFER."
  (with-current-buffer buffer
    (make-activity-buffer :bookmark (ignore-errors
                                      (bookmark-make-record))
                          :filename (buffer-file-name buffer)
                          :name (buffer-name buffer)
                          ;; TODO: Handle indirect buffers, narrowing.
                          :etc `((indirectp . ,(not (not (buffer-base-buffer buffer))))
                                 (narrowedp . ,(buffer-narrowed-p)))
                          :local-variables
                          (when activity-buffer-local-variables
                            (cl-loop
                             for variable in activity-buffer-local-variables
                             when (buffer-local-boundp variable (current-buffer))
                             collect (cons variable
                                           (buffer-local-value variable (current-buffer))))))))

(cl-defmethod activity--deserialize ((struct activity-buffer))
  "Return buffer for `activity-buffer' STRUCT."
  (pcase-let (((cl-struct activity-buffer bookmark filename name) struct))
    (let ((buffer (cond (bookmark (activity--bookmark-buffer struct))
                        (filename (activity--filename-buffer struct))
                        (name (activity--name-buffer struct))
                        (t (error "Activity struct is invalid: %S" struct)))))
      (cl-assert (buffer-live-p buffer))
      (activity-debug struct buffer)
      buffer)))

(defun activity--bookmark-buffer (struct)
  "Return buffer for `activity-buffer' STRUCT."
  ;; NOTE: Be aware of the following note from burly.el:
  ;; NOTE: Due to changes in help-mode.el which serialize natively
  ;; compiled subrs in the bookmark props, which cannot be read
  ;; back (which actually break the entire bookmark system when
  ;; such a props is saved in the bookmarks file), we have to
  ;; workaround a failure to read here.  See bug#56643.
  (pcase-let* (((cl-struct activity-buffer bookmark) struct))
    (save-window-excursion
      (condition-case err
          (progn
            (bookmark-jump bookmark)
            (when-let ((local-variable-map
                        (bookmark-prop-get bookmark 'activity-buffer-local-variables)))
              (cl-loop for (variable . value) in local-variable-map
                       do (setf (buffer-local-value variable (current-buffer)) value))))
        (error (delay-warning 'activity
                              (format "Error while opening bookmark: ERROR:%S  RECORD:%S" err struct))))
      (current-buffer))))

(defcustom activity-major-mode-alist
  (list (cons 'org-mode
              (list (cons 'make-record-fn #'activity--org-mode-buffer-url)
                    (cons 'open-record-fn #'activity-follow-url-org-mode))))
  "Alist mapping major modes to the appropriate Activity functions."
  :type '(alist :key-type symbol
                :value-type (set (cons (const make-record-fn)
                                       (function :tag "Make-record function"))
                                 (cons (const open-record-fn)
                                       (function :tag "Follow-record function")))))

(defun activity--filename-buffer (activity-buffer)
  "Return buffer for ACTIVITY-BUFFER having `filename' set."
  (pcase-let* (((cl-struct activity-buffer filename) activity-buffer)
               (buffer (find-file-noselect filename))
               (major-mode (buffer-local-value 'major-mode buffer))
               (follow-fn (map-nested-elt activity-major-mode-alist (list major-mode 'open-record-fn))))
    (cl-assert follow-fn nil "Major mode not in `activity-major-mode-alist': %s" major-mode)
    (funcall follow-fn :buffer buffer :record activity-buffer)))

(defun activity--name-buffer (activity-buffer)
  "Return buffer for ACTIVITY-BUFFER having `name' set."
  (pcase-let (((cl-struct activity-buffer name) activity-buffer))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create (concat "*Activity (error): " name "*"))
          (insert "Activity was unable to get a buffer named: " name "\n"
                  "activity-buffer: " (format "%S" activity-buffer) "\n"
                  "Please report this error to the developer\n\n")
          (current-buffer)))))

(cl-defun activity-completing-read
    (&key (activities activity-activities) (prompt "Open activity: "))
  "Return an activity read with completion from ACTIVITIES.
PROMPT is passed to `completing-read', which see."
  (let* ((names (activity-names activities))
         (name (completing-read prompt names nil nil nil activity-completing-read-history)))
    (or (map-elt activity-activities name)
        (make-activity :name name))))

;; (defun activity--bookmarks ()
;;   "Return list of activity bookmarks."
;;   (bookmark-maybe-load-default-file)
;;   (mapcar (lambda (bookmark)
;;             (bookmark-prop-get bookmark 'activity))
;;           (cl-remove-if-not (pcase-lambda (`(,_name . ,(map handler)))
;;                               (equal #'activity-bookmark-handler handler))
;;                             bookmark-alist)))

(cl-defun activity-names (&optional (activities activity-activities))
  "Return list of names of ACTIVITIES."
  (map-keys activities))

(defun activity-bookmark-handler (bookmark)
  "Switch to BOOKMARK's activity."
  (activity-switch (map-elt activity-activities (car bookmark))))

(defun activity--buffer-local-variables (variables)
  "Return alist of buffer-local VARIABLES for current buffer.
Variables without buffer-local bindings in the current buffer are
ignored."
  (cl-loop for variable in variables
           when (buffer-local-boundp variable (current-buffer))
           collect (cons variable (buffer-local-value variable (current-buffer)))))

(defun activity-name-for (activity)
  "Return frame/tab name for ACTIVITY.
Adds `activity-name-prefix'."
  (concat activity-name-prefix (activity-name activity)))

;;;; Footer

(provide 'activity)

;;; activity.el ends here
