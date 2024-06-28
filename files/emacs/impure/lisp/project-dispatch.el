;;; project-dispatch.el --- Dispatch project commands with transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  aurtzy
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
;; TODO Remove hard dependencies on e.g. consult and eat
;; TODO "Find" commands should all be able to work off of subdirectories as well

;;; Code:

(require 'consult)
(require 'eat)
(require 'magit)
(require 'project)
(require 'transient)

(transient-define-prefix project-dispatch ()
  "Dispatch some command for a project."
  ["Project commands"
   ("p" "Switch project" project-dispatch:--root-directory)]
  [[("b" "Switch buffer" project-dispatch-switch-to-buffer)
    ("k" "Kill buffers" project-dispatch-kill-buffers)]
   [("m" "Magit status" project-dispatch-magit-status)
    ("C-b" "Buffer list" project-dispatch-list-buffers)]]
  ["From directory"
   ("d" "From directory" project-dispatch:--from-directory)]
  [[("c" "Compile" project-dispatch-compile)
    ("D" "Dired" project-dispatch-dired)
    ("s" "Shell" project-dispatch-shell)]
   [("v" "VC dir" project-dispatch-vc-dir)
    ("!" "Run" project-dispatch-shell-command)
    ("M-x" "Extended command" project-dispatch-execute-extended-command)]]
  ["Find"
   ("e" "Include external roots" "--include-external-roots")]
  [[("f" "file" project-dispatch-find-file)]
   [("g" "regexp" project-dispatch-find-regexp)]])

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

(defun project-dispatch--include-external-roots ()
  "Return whether or not external roots should be included in find commands."
  (let* ((args (transient-args transient-current-command)))
    (if args
        (transient-arg-value "--include-external-roots" args)
      nil)))

(transient-define-suffix project-dispatch-switch-to-buffer ()
  "Switch to buffer in project."
  (interactive)
  (let ((project-current-directory-override
         (project-dispatch--root-directory)))
    ;; TODO: Generalize this so there isn't a hard dependency on consult
    (consult-project-buffer)))

(transient-define-suffix project-dispatch-list-buffers ()
  "Display a list of open buffers for project."
  (interactive)
  (let ((project-current-directory-override
         (project-dispatch--root-directory)))
    (project-list-buffers)))

(transient-define-suffix project-dispatch-dired ()
  "Open Dired in project root."
  (interactive)
  (dired (project-dispatch--from-directory)))

(transient-define-suffix project-dispatch-find-file ()
  "Find file in project."
  (interactive)
  (let ((project-current-directory-override
         (project-dispatch--root-directory)))
    (if (project-dispatch--include-external-roots)
        (project-or-external-find-file)
      (project-find-file))))

(transient-define-suffix project-dispatch-kill-buffers ()
  "Kill all buffers related to project."
  (interactive)
  (let ((project-current-directory-override
         (project-dispatch--root-directory)))
    (call-interactively #'project-kill-buffers)))

(transient-define-suffix project-dispatch-shell ()
  "Start an Eat terminal emulator in project."
  (interactive)
  ;; TODO: We should be able to swap out what shell is used here
  (let ((default-directory (project-dispatch--from-directory)))
    (eat nil t)))

(transient-define-suffix project-dispatch-shell-command ()
  "Run a shell command asynchronously in a project."
  (interactive)
  (let ((default-directory (project-dispatch--from-directory)))
    (call-interactively #'async-shell-command)))

(transient-define-suffix project-dispatch-execute-extended-command ()
  "Execute an extended command in project root."
  (interactive)
  (let ((default-directory (project-dispatch--from-directory)))
    (call-interactively #'execute-extended-command)))

(transient-define-suffix project-dispatch-find-regexp ()
  "Search project for regexp."
  (interactive)
  (let* ((project (project-current (project-dispatch--root-directory)))
         (external-roots (project-external-roots project))
         (dirs (cons (project-dispatch--from-directory)
                     external-roots)))
    (consult-ripgrep dirs)))

(transient-define-suffix project-dispatch-magit-status ()
  "Open the Magit dispatch transient for project."
  (interactive)
  (magit-status-setup-buffer (project-dispatch--root-directory)))

(transient-define-suffix project-dispatch-compile ()
  "Compile the project."
  (interactive)
  (let ((default-directory (project-dispatch--from-directory)))
    (call-interactively #'compile)))

(transient-define-suffix project-dispatch-vc-dir ()
  "Run VC-Dir in project."
  (interactive)
  (vc-dir (project-dispatch--from-directory)))

(provide 'project-dispatch)
;;; project-dispatch.el ends here
