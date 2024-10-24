;;; project-dispatch.el --- Dispatch project commands with transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 aurtzy
;; Copyright (C) 2008-2023 The Magit Project Contributors
;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: aurtzy <aurtzy@gmail.com>
;; Keywords: convenience

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

;; TODO this commentary

;;; Code:

(require 'grep)
(require 'project)
(require 'transient)

;;;
;;; Macros.
;;;

(defmacro project-dispatch--with-environment (&rest body)
  "Run BODY with `project-dispatch' \"environment\" options set."
  ;; Define variables that determine the environment.
  `(let ((from-directory (or (project-dispatch--from-directory)
                             default-directory))
         (prefer-other-window (project-dispatch--prefer-other-window))
         ;; Only enable envrc if the initial environment has it enabled.
         (enable-envrc? (and (boundp 'envrc-mode) envrc-mode))
         ;; Save the environment to restore in case of problem.
         (old-default-directory default-directory)
         (old-project-current-directory-override
          project-current-directory-override)
         (old-display-buffer-overriding-action
          display-buffer-overriding-action))
     (unwind-protect
         ;; Don't let the current buffer affect execution in case it's not
         ;; related to the project.
         (with-temp-buffer
           (let ((default-directory from-directory)
                 ;; This handles edge cases with `project' commands.
                 (project-current-directory-override from-directory)
                 (display-buffer-overriding-action
                  (and prefer-other-window '(display-buffer-use-some-window
                                             (inhibit-same-window t)))))
             ;; Make sure commands are run in the correct direnv environment
             ;; if envrc-mode is enabled.
             (when (and enable-envrc? (functionp 'envrc-mode))
               (envrc-mode 1))
             ,@body))
       (setq default-directory
             old-default-directory
             project-current-directory-override
             old-project-current-directory-override
             display-buffer-overriding-action
             old-display-buffer-overriding-action))))


;;;
;;; Global variables.
;;;

(defgroup project-dispatch nil
  "Customization for `project-dispatch'."
  :group 'project-dispatch)

(defcustom project-dispatch-compile-suffixes '(("c" "make" "Make" "make -k"))
  "Commands for the `project-dispatch-compile' prefix.

The value should be a list of transient-like specification
entries (KEY NAME DESCRIPTION COMPILE-COMMAND), where KEY is the
project root path that is used as the alist key.

NAME is used to construct the compilation buffer name.  KEY and
DESCRIPTION are passed to transient suffix constructors as the
keybind and description, respectively.  The COMPILE-COMMAND value
is passed to `compile' as the shell command to run.

For example, the following may be used as a dir-locals.el value
for `project-dispatch-compile-suffixes' to add \"make -k\" and
\"guile --help\" in a particular project:

  ((\"m\" \"Make\" \"echo Running make...; make -k\")
   (\"g\" \"Guile help\" \"echo Get some help from Guile...; guile --help\")))"
  :type '(alist :key-type string
                :value-type (list string string string))
  :group 'project-dispatch)

(defcustom project-dispatch-find-file-command
  (lambda ()
    (interactive)
    (let* ((project (project-current t))
           (dirs (list default-directory)))
      (project-find-file-in (thing-at-point 'filename)
                            dirs
                            project
                            ;; TODO: Support some way of enabling INCLUDE-ALL
                            ;; include-all
                            )))
  "The command used for opening a file in a project.

This is called whenever the function `project-dispatch-find-file'
is invoked."
  :type 'function
  :group 'project-dispatch)

(defcustom project-dispatch-find-regexp-command
  ;; Modified version of `project-find-regexp' from `project.el'.
  (lambda (regexp)
    (interactive (list (project--read-regexp)))
    (xref-show-xrefs
     (apply-partially #'project--find-regexp-in-files
                      regexp
                      (project--files-in-directory default-directory nil))
     nil))
  "The command used for finding regexp matches in a project.

This is called whenever the function `project-dispatch-find-regexp'
is invoked."
  :type 'function
  :group 'project-dispatch)

(defcustom project-dispatch-shell-command
  ;; Modified version of `project-eshell' from `project.el'.
  (lambda ()
    (interactive)
    (let* ((eshell-buffer-name (project-prefixed-buffer-name "eshell"))
           (eshell-buffer (get-buffer eshell-buffer-name)))
      (if (and eshell-buffer (not current-prefix-arg))
          (pop-to-buffer eshell-buffer
                         (bound-and-true-p display-comint-buffer-action))
        (eshell t))))
  "The command used for opening a shell in a project.

This is called whenever the function
`project-dispatch-shell-command' is invoked."
  :type 'function
  :group 'project-dispatch)

(defcustom project-dispatch-switch-to-buffer-command #'project-switch-to-buffer
  "The command used for switching project buffers.

This is called whenever the function
`project-dispatch-switch-to-buffer' is invoked."
  :type 'function
  :group 'project-dispatch)


;;;
;;; Prefixes.
;;;

(transient-define-prefix project-dispatch ()
  "Dispatch some command for a project."
  ["Options"
   ("p" "Switch project" project-dispatch:--root-directory)
   ("d" "From directory" project-dispatch:--from-directory)
   ("o" "Prefer other window" "--prefer-other-window")]
  ["Project commands"
   :pad-keys t
   [("B" "Buffer list" project-dispatch-list-buffers)
    ("b" "Switch buffer" project-dispatch-switch-to-buffer)]
   [("k" "Kill buffers" project-dispatch-kill-buffers)
    ("m" "Magit status" project-dispatch-magit-status
     :if (lambda () (featurep 'magit)))]]
  ["From directory"
   :pad-keys t
   [("c" "Compile" project-dispatch-compile)
    ("D" "Dired" project-dispatch-dired)
    ("s" "Shell" project-dispatch-shell)]
   [("v" "VC dir" project-dispatch-vc-dir)
    ("!" "Run" project-dispatch-shell-command)
    ("M-x" "Extended command" project-dispatch-execute-extended-command)]]
  ["Find"
   [("f" "file" project-dispatch-find-file)]
   [("g" "regexp" project-dispatch-find-regexp)]])

(transient-define-prefix project-dispatch-compile ()
  "Dispatch compilation commands.

This prefix can be configured with
`project-dispatch-compile-suffixes'."
  ["Compile"
   :class transient-column
   :setup-children project-dispatch-compile--setup-suffixes])


;;;
;;; Infix handling.
;;;

(transient-define-infix project-dispatch:--root-directory ()
  :class transient-option
  :argument "--root-directory="
  :init-value (lambda (obj)
                (oset obj value (project-dispatch--find-root-directory
                                 default-directory)))
  :always-read t
  :reader (lambda (&rest _ignore)
            (project-dispatch--find-root-directory
             (project-prompt-project-dir))))

(defun project-dispatch--find-root-directory (directory &optional silent)
  "Attempt to find project root directory from DIRECTORY.  May return nil.

A message is printed if no root directory can be found.  SILENT
may be set to a non-nil value to suppress it."
  (if-let ((directory (directory-file-name (file-truename directory)))
           (project (project-current nil directory))
           (root-directory (project-root project)))
      (progn
        (project-remember-project project)
        root-directory)
    (unless silent
      (message "No parent project found for %s"
               directory))
    nil))

(defun project-dispatch--root-directory ()
  "Return the project root directory defined in transient arguments."
  (if-let ((args (transient-args transient-current-command))
           (root-dir (transient-arg-value "--root-directory=" args)))
      root-dir
    (project-root (project-current t))))

(defclass project-dispatch-option-switches (transient-switches)
  ()
  "Class used for a set of switches where exactly one is selected.")

(cl-defmethod transient-infix-read ((obj project-dispatch-option-switches))
  "Cycle through mutually exclusive switch options from OBJ.

This method skips over nil, so exactly one switch of this object
is always selected."
  (let ((choices (mapcar (apply-partially #'format (oref obj argument-format))
                         (oref obj choices))))
    (if-let ((value (oref obj value))
             (next-value (cadr (member value choices))))
        next-value
      (car choices))))

(transient-define-infix project-dispatch:--from-directory ()
  :class project-dispatch-option-switches
  :argument-format "--from-%s-directory"
  :argument-regexp "\\(--from-\\(root\\|sub\\)-directory\\)"
  :init-value (lambda (obj)
                (oset obj value "--from-root-directory"))
  :choices '("root" "sub"))

(defun project-dispatch--prompt-directory (root-directory)
  "Prompt for a subdirectory in project and return the selected path.

ROOT-DIRECTORY is used to determine the project."
  ;; XXX: This is based on `project-find-dir' in project.el, which has an issue
  ;; of not displaying empty directories.
  (let* ((project (project-current nil root-directory))
         (all-files (project-files project))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (all-dirs (mapcar #'file-name-directory all-files)))
    (funcall project-read-file-name-function
             "Select directory"
             ;; Some completion UIs show duplicates.
             (delete-dups all-dirs)
             nil 'file-name-history)))

(defun project-dispatch--from-directory ()
  "Return the working directory to be used for `project-dispatch' commands."
  (let ((args (transient-args transient-current-command))
        (root-directory (project-dispatch--root-directory)))
    (cond
     ((transient-arg-value "--from-root-directory" args)
      root-directory)
     ((transient-arg-value "--from-sub-directory" args)
      (project-dispatch--prompt-directory root-directory)))))

(defun project-dispatch--prefer-other-window ()
  "Return whether other window should be preferred when displaying buffers."
  (let ((args (transient-args transient-current-command)))
    (and args (transient-arg-value "--prefer-other-window" args))))


;;;
;;; Suffixes.
;;;

(transient-define-suffix project-dispatch-switch-to-buffer ()
  "Switch to buffer in project."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively project-dispatch-switch-to-buffer-command)))

(transient-define-suffix project-dispatch-list-buffers ()
  "Display a list of open buffers for project."
  (interactive)
  (project-dispatch--with-environment
   (project-list-buffers)))

(transient-define-suffix project-dispatch-dired ()
  "Open Dired in project root."
  (interactive)
  (project-dispatch--with-environment
   (dired (project-dispatch--from-directory))))

(transient-define-suffix project-dispatch-find-file ()
  "Find file in project."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively project-dispatch-find-file-command)))

(transient-define-suffix project-dispatch-kill-buffers ()
  "Kill all buffers related to project."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively #'project-kill-buffers)))

(transient-define-suffix project-dispatch-shell ()
  "Start an Eat terminal emulator in project."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively project-dispatch-shell-command)))

(transient-define-suffix project-dispatch-shell-command ()
  "Run a shell command asynchronously in a project."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively #'async-shell-command)))

(transient-define-suffix project-dispatch-execute-extended-command ()
  "Execute an extended command in project root."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively #'execute-extended-command)))

(transient-define-suffix project-dispatch-find-regexp ()
  "Search project for regexp."
  (interactive)
  (project-dispatch--with-environment
   (call-interactively project-dispatch-find-regexp-command)))

(transient-define-suffix project-dispatch-magit-status ()
  "Open the Magit dispatch transient for project."
  (interactive)
  (project-dispatch--with-environment
   (and (fboundp 'magit-status-setup-buffer) (magit-status-setup-buffer))))

(defun project-dispatch-compile--setup-suffixes (_)
  "Set up suffixes according to `project-dispatch-compile-suffixes'."
  (project-dispatch--with-environment
   (hack-dir-local-variables-non-file-buffer)
   ;; XXX: Since infix arguments from `project-dispatch' are not made
   ;; available for `project-dispatch-compile', work around it by setting
   ;; `default-directory' from the current (desired) environment to be used
   ;; later.
   (transient-parse-suffixes
    'project-dispatch-compile
    `(,@(mapcar
         (pcase-lambda (`(,key ,name ,description ,compile-command))
           `(,key
             ;; TODO: Color the command
             ,(concat description "  ::  " compile-command)
             (lambda ()
               (interactive)
               (let ((default-directory ,default-directory))
                 (project-dispatch--with-environment
                  (let* ((compilation-buffer-name-function
                          (lambda (major-mode-name)
                            (project-prefixed-buffer-name
                             (concat ,name "-" major-mode-name)))))
                    (compile ,compile-command)))))))
         project-dispatch-compile-suffixes)
      ("!"
       "Aternative command..."
       (lambda ()
         (interactive)
         (let ((default-directory ,default-directory))
           (project-dispatch--with-environment
            (call-interactively #'compile)))))))))

(transient-define-suffix project-dispatch-vc-dir ()
  "Run VC-Dir in project."
  (interactive)
  (project-dispatch--with-environment
   (vc-dir (project-dispatch--from-directory))))

(provide 'project-dispatch)
;;; project-dispatch.el ends here
