;;; init.el --- my init.el -*- lexical-binding: t -*-
;; Copyright © 2013-2022 Phil Hagelberg and contributors
;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
;; Copyright © 2012-2016 Kyle Meyer <kyle@kyleam.com>
;; Copyright © 2022-2024  Free Software Foundation, Inc.
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These are useful references that helped shape this init.el:
;; Phil Hagelberg's better-defaults configuration
;; David Wilson's (davwil, System Crafters) videos
;; Mickey Petersen's Mastering Emacs articles
;;
;; Organization scheme:
;;
;; This configuration is broken up into the following main sections:
;; "initial", "settings", "minor-modes", "transients", "major-modes".
;;
;; Generally, code in these sections should be wrapped in a `use-package'.
;; Additionally, the sections should be broken up into sub-sections with
;; heading comments (for outline support) that document what exactly is
;; configured, like a docstring of sorts.  A sub-section may have more than
;; one `use-package'.
;;
;; "initial" is a special section for exceptional cases with code that should
;; be at the top of the config, possibly run before everything else.
;; Variables/functions that may change due to external factors and can be used
;; in different parts of the configuration usually belong here.  For example,
;; custom user-defined variables of names or file locations are sensible
;; inclusions.
;;
;; "settings" consists of function/variable definitions and customizations.
;; One-shot code like loading a file also goes here.  This can also be
;; considered a miscellaneous section for configurations that don't belong
;; anywhere else.
;;
;; "minor-modes" consists of configurations for minor modes.
;;
;; "transients" consists of configurations for temporary/passing commands that
;; don't persist like minor/major modes do; a key bind for an command that,
;; say, edits some text and completes soon after the invocation is an example
;; of this.  This is not to be confused with `transient.el', although
;; configurations for Transient interfaces usually make sense to include here
;; as well.
;;
;; "major-modes" consists of major mode configurations.

;;; Code:


;;;
;;; (initial) Initial settings.
;;;

;;;; Add additional load paths.

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;;; Unconditionally require some packages.

(require 'denote)
(require 'transient)
(require 'use-package)

;;;; User info

(setq user-full-name "aurtzy"
      user-mail-address "aurtzy@gmail.com")

;;;; Fix some byte-compiler warnings.
;; See: https://github.com/jwiegley/use-package/issues/636

(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;;;; Set `custom-file' so customization information doesn't end up in init.el.
;; ...and load it, if available.

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;;;; Set a variable for directories to exclude in searches.

(defvar my-emacs-search-excluded-directories
  `(,@vc-directory-exclusion-list
    ".direnv"))

;;;; Set a variable pointing to local state directory.

(defvar my-emacs-state-dir
  (concat (or (getenv "XDG_STATE_HOME") "~/.local/state") "/emacs/"))

;;;; Set aliases file for translating names to denote identifiers.

(defvar my-emacs-denote-aliases-file "areas/aliases.lisp"
  "Aliases file path, relative to `denote-directory'.  Stores an
alist of aliases to denote IDs.")

;;;; Configure denote.
;; This is a hard dependency due some reliance on denote for managing and
;; accessing external files; notably, the "assets directories", where files
;; associated with notes can live.

(use-package denote
  :custom
  (denote-directory "~/data/")
  ;; Avoid traversing past data directories.
  (denote-excluded-directories-regexp "^[^/]*/.*")
  (denote-known-keywords '(">inbox" ">todos" ">done" ">events"))
  :config
  (add-hook 'after-save-hook #'my-emacs-denote-set-status-keywords)
  :preface
  (defun my-emacs-denote-set-status-keywords ()
    "Set the current note's status keywords, if applicable, and save the buffer.

This does nothing if the current buffer is not a denote note file."
    (interactive)
    (declare-function org-element-parse-buffer "org")
    (declare-function org-element-property "org")
    (defvar org-map-continue-from)
    (defvar org-ts-regexp)
    (require 'org)
    (when-let* ((file (buffer-file-name))
                (_ (denote-file-is-note-p file)))
      (let ((keywords (denote-extract-keywords-from-path file)))
        ;; "Inbox" entry.  Only apply if there are no keywords, in which case
        ;; it is likely to be either 1. a new note, or 2. an edge case that
        ;; needs additional sorting (perhaps via a new keyword).  This keyword
        ;; must be manually removed, as there is no way to decisively indicate
        ;; "sorting has completed".
        (unless keywords
          (setq keywords (cons ">inbox" keywords)))
        (pcase (file-name-extension file)
          ("org"
           ;; Statuses may change, so reset the rest before continuing.
           (setq keywords (seq-difference keywords '(">done" ">todos" ">events")))
           ;; "Task" entries.
           (let (task-status)
             ;; `task-status': nil for "no tasks found"; 'todo for "there is
             ;; an active TODO"; 'done for "there are tasks, and all of them
             ;; are done".
             (org-map-entries (lambda ()
                                (cond
                                 ((org-entry-is-done-p)
                                  (setq task-status 'done)
                                  ;; Assume that if a headline is marked as
                                  ;; done, any todos under it are no longer
                                  ;; relevant, so we skip its subheadings.
                                  (let ((initial-pos-bol (pos-bol)))
                                    (org-forward-heading-same-level 1)
                                    ;; If position is the same, we're at the
                                    ;; last heading of this level.
                                    (when (eql (pos-bol) initial-pos-bol)
                                      (org-next-visible-heading 1))
                                    (setq org-map-continue-from (point))))
                                 ((org-entry-is-todo-p)
                                  (setq task-status 'todo)
                                  ;; We can conclude that the entry still has
                                  ;; stuff to do at this point.
                                  (setq org-map-continue-from (point-max))))))
             (pcase task-status
               ('todo (setq keywords (cons ">todos" keywords)))
               ('done (setq keywords (cons ">done" keywords)))))
           ;; "Event" entries.
           (when (save-excursion
                   (ignore-error search-failed
                     (goto-char (point-min))
                     (search-forward-regexp org-ts-regexp)))
             (setq keywords (cons ">events" keywords)))))
        (setq keywords (cl-remove-duplicates keywords :test #'equal))
        (let ((denote-rename-confirmations nil))
          (denote-rename-file
           file 'keep-current keywords 'keep-current 'keep-current)
          (save-buffer)))))

  ;; Functions for accessing assets directories.

  (defun my-emacs-denote-assets-directory (file)
    "Return assets directory from FILE note.  Prompt to create, if nonexistent."
    (interactive
     (list (denote-file-prompt nil "Select FILE associated with assets")))
    (let* ((file (or file (buffer-file-name)))
           (identifier (denote-retrieve-filename-identifier-with-error file))
           (assets-dir (file-name-concat (file-name-directory file) identifier)))
      (unless (file-exists-p assets-dir)
        (unless (y-or-n-p "Assets directory doesn't exist.  Create it? ")
          (user-error "Assets directory doesn't exist"))
        (make-directory assets-dir t))
      (file-name-as-directory assets-dir)))

  (defun my-emacs-denote-aliases-assoc-ref (alias)
    "Return the denote ID associated with ALIAS.  May return nil."
    (if-let* ((mappings-file (file-name-concat denote-directory
                                               my-emacs-denote-aliases-file))
              ((file-exists-p mappings-file))
              (mappings (read (with-temp-buffer
                                (insert-file-contents mappings-file)
                                (buffer-string)))))
        (cdr (assoc alias mappings))
      nil)))


;;;
;;; (settings) Function/variable definitions and customizations.
;;;

;;;; Provide function for setting project to associated assets directory.

(use-package project
  :preface
  (defun my-emacs-project-set-assets-directory-override ()
    "Set a file-local project directory override to associated assets directory."
    (let* ((file (buffer-file-name))
           (assets-directory (my-emacs-denote-assets-directory file)))
      (make-local-variable 'project-current-directory-override)
      (setq project-current-directory-override assets-directory))))

;;;; Translate ANSI escape sequences in compilation buffer to text properties.

(use-package ansi-color
  :config
  (require 'compile)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;; Set personal dictionary location.

(use-package ispell
  :config
  (when-let* ((identifier (my-emacs-denote-aliases-assoc-ref "emacs")))
    (setq ispell-personal-dictionary
          (let* ((notes-file (denote-get-path-by-id identifier))
                 (assets-dir (my-emacs-denote-assets-directory notes-file)))
            (file-name-concat assets-dir ".static/aspell.en.pws")))))

;;;; Add org link type for entry-local denote assets.

(use-package org
  :config
  (org-link-set-parameters
   "denote-asset"
   :follow #'my-emacs-denote-asset-follow
   :complete #'my-emacs-denote-asset-complete)

  (defun my-emacs-denote-asset-follow (path &optional prefix)
    "Follow PATH for `denote-asset' org link with potential PREFIX argument."
    (let* ((identifier (denote-retrieve-filename-identifier-with-error
                        (buffer-file-name)))
           (assets-dir (file-name-concat (file-name-directory
                                          (denote-get-path-by-id identifier))
                                         identifier))
           (asset-file (file-name-concat assets-dir path)))
      (org-link-open-as-file asset-file prefix)))

  (defun my-emacs-denote-asset-complete ()
    "Provide completion for and return a `denote-asset' link."
    (let* ((identifier
            (denote-retrieve-filename-identifier-with-error (buffer-file-name)))
           (assets-dir
            (my-emacs-denote-assets-directory (denote-get-path-by-id identifier)))
           (relative-path
            (file-relative-name (read-file-name "Denote asset: " assets-dir)
                                assets-dir)))
      (concat "denote-asset:" relative-path))))

;;;; Add active data directories to org agenda files.

(use-package org
  :config
  (org-store-new-agenda-file-list
   (cl-delete-duplicates
    (append (mapcar (lambda (data-dir-name)
                      (file-name-as-directory
                       (file-name-concat denote-directory data-dir-name)))
                    '("workshop" "areas" "library"))
            org-agenda-files)
    :test #'equal)))

;;;; Define function for creating new notes files.

(use-package org
  :config
  (require 'autoinsert)
  (defun my-emacs-find-new-notes-file (file title)
    "Create new notes FILE with TITLE."
    (if (file-exists-p file)
        (user-error "File already exists: %s" file)
      ;; Don't prompt to auto-insert since we're inserting a specific custom
      ;; template here.
      (let ((auto-insert nil))
        (with-current-buffer (find-file file)
          (skeleton-insert
           `(nil
             "#+title: " ,title \n
             "#+filetags: " _ \n \n
             "* Agenda")))))))

;;;; Explicitly set `org-agenda-span'.

(use-package org-agenda
  :custom
  (org-agenda-span 10))

;;;; Specify additional `project.el' root markers.

(use-package project
  :custom
  (project-vc-extra-root-markers `(,dir-locals-file
                                   "manifest.scm"
                                   ".envrc"
                                   "agenda.org")))

;;;; Configure how to uniquify buffer names.

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-strip-common-suffix nil))

;;;; Make the frame background slightly transparent.

(use-package emacs
  :preface
  (defun toggle-frame-transparency ()
    "Toggles frame transparency."
    (interactive)
    (if (not (eql 100 (frame-parameter nil 'alpha-background)))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background
                           (alist-get 'alpha-background
                                      default-frame-alist))))
  :config
  (add-to-list 'default-frame-alist '(alpha-background . 90)))

;;;; Set up global visuals for Emacs, including theming and fonts.

(use-package emacs
  :custom
  (modus-themes-hl-line '(accented))
  (visible-bell t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  :custom-face
  (mode-line ((t . (:family "Iosevka Etoile"))))
  (mode-line-inactive ((t . (:family "Iosevka Etoile"))))
  :config
  (load-theme 'modus-vivendi)
  (add-to-list 'default-frame-alist '(font . "Hack-11"))
  (tool-bar-mode -1)
  (global-hl-line-mode 1))

;;;; Make it harder to exit Emacs.

(use-package emacs
  :custom
  (confirm-kill-emacs #'confirm-kill-emacs-yes-or-no-p)
  :preface
  (defvar confirm-kill-emacs-active-p nil)
  (defun confirm-kill-emacs-yes-or-no-p (prompt)
    "`yes-or-no-p', wrapped with an additional condition for confirming
quits:  if a previous call to this function is still active, auto-return `t'."
    (interactive)
    (if confirm-kill-emacs-active-p
        t
      (set 'confirm-kill-emacs-active-p t)
      (unwind-protect
          (yes-or-no-p prompt)
        (set 'confirm-kill-emacs-active-p nil)))))

;;;; Always check for the most recent file to load.

(use-package emacs
  :custom
  (load-prefer-newer t))

;;;; Control backups and deletion of files.

(use-package emacs
  :custom
  (backup-directory-alist `(("." . ,my-emacs-state-dir)))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 4)
  (delete-by-moving-to-trash t))

;;;; Enable directory-local variables for remote files.

(use-package emacs
  :custom
  (enable-remote-dir-locals t))

;;;; Enable `imenu' support for `use-package'.

(use-package use-package-core
  :custom
  (use-package-enable-imenu-support t))

;;;; Customize `flymake'.

(use-package flymake
  :custom
  (flymake-number-of-errors-to-display 4))

;;;; Show help at point.

(use-package emacs
  :custom (help-at-pt-display-when-idle t))

;;;; Add "tooltip" org link for arbitrary tooltips.

(use-package ol
  :config
  (org-link-set-parameters
   "tooltip"
   :follow (lambda (path _prefix) (message "%s" path))))


;;;
;;; (minor-modes) Minor modes.
;;;

;;;; Shrink `paredit' lighter.

(use-package paredit
  :delight " (ed")

;;;; Configure `envrc-global-mode'.

(use-package envrc
  :custom
  (envrc-none-lighter '(" env/"
                        (:propertize "N" face envrc-mode-line-none-face)))
  (envrc-on-lighter '(" env/"
                      (:propertize "O" face envrc-mode-line-on-face)))
  (envrc-error-lighter '(" env/"
                         (:propertize "E" face envrc-mode-line-error-face)))
  :hook (after-init . envrc-global-mode))

;;;; Configure `editorconfig-mode'.

(use-package editorconfig
  :delight " EdCfg"
  :config (editorconfig-mode 1))

;;;; Configure `global-page-break-lines-mode'.

(use-package page-break-lines
  :delight
  :config (global-page-break-lines-mode t))

;;;; Enable `compilation-minor-mode' in log files.

(use-package compile
  :mode ("\\.log\\'" . compilation-minor-mode))

;;;; Override read-only key-bind for `compilation-minor-mode' in Comint buffers.

(use-package comint
  :bind ( :map comint-mode-map
          ("C-x C-q" . my-emacs/toggle-compilation-minor-mode))
  :preface
  (defun my-emacs/toggle-compilation-minor-mode ()
    (interactive)
    (if compilation-minor-mode
        (progn
          (compilation-minor-mode -1)
          (read-only-mode -1)
          (message "Disabled compilation mode"))
      (compilation-minor-mode 1)
      (read-only-mode 1)
      (message "Enabled compilation mode"))))

;;;; Prettify-rename denote buffers.

(use-package denote :config (denote-rename-buffer-mode t))

;;;; Add fontification for denote files in `dired-mode'.

(use-package dired :hook (dired-mode . denote-dired-mode))

;;;; Enable context menus.

(use-package mouse
  :config
  (context-menu-mode))

;;;; Disable menu bar, which doesn't correctly apply transparency on GNOME.

(use-package emacs
  :config
  (menu-bar-mode -1))

;;;; Load `flymake'-related setup functions.

(use-package flymake)

(use-package package-lint-flymake)

;;;; Enable some Emacs file history tracking modes.

(use-package emacs
  :config
  (savehist-mode 1)
  (save-place-mode 1)
  (recentf-mode 1))


;;;
;;; (transients) Transients.
;;;

;;;; Set up `vundo' key-bind.

(use-package vundo
  :bind ("C-M-?" . vundo))

;;;; Set up `envrc' command map.

(use-package envrc
  :bind ("C-c d" . #'envrc-command-map))

;;;; Add a Transient menu for managing denote files.
;; TODO: Support denote silos.

(use-package transient
  :bind ("C-c A" . my-emacs-denote-dispatch)
  :preface
  (require 'thingatpt)

  ;; TODO: Add commands from:
  ;; <https://protesilaos.com/emacs/denote#h:998ae528-9276-47ec-b642-3d7355a38f27>
  (transient-define-prefix my-emacs-denote-dispatch ()
    [:description
     (lambda ()
       (concat (propertize "Context: " 'face 'transient-heading)
               (if-let* ((identifier (my-emacs-denote-context-id)))
                   (propertize (file-name-nondirectory
                                (denote-get-path-by-id identifier))
                               'face 'transient-value)
                 (propertize "None detected" 'face 'transient-inapt-suffix))))
     ""]
    ["Denote commands"
     :advice my-emacs-denote-with-context-apply
     ;; TODO: Context commands should be merged with find commands, with
     ;; context ID (if available) as initial input.
     ["File"
      ("f c" "from context" my-emacs-denote-current-note)
      ;; TODO: Use `consult-line-multi' with `org-agenda-files' instead?
      ("f f" "find" denote-open-or-create)
      ("f g" "find regexp" org-occur-in-agenda-files)
      ("f n" "new" denote-subdirectory)]
     ;; TODO: Command: Create static assets directory.
     ["Assets directory"
      ("d c" "from context" my-emacs-denote-dired-current-assets-directory)
      ;; TODO: This should prompt with completion for only notes with an
      ;; assets directory.
      ("d f" "find" my-emacs-denote-dired-assets-directory)
      ("d l" "list (data directory)"
       (lambda () (interactive)
         (dired (read-directory-name "Dired (directory): " denote-directory))))
      ;; TODO: Add a "new" command to split from the "find" command.
      ]
     ["Rename"
      ;; TODO: Command: Move files between data directories.
      ("r k" "keywords" my-emacs-denote-rename-file-keywords)
      ("r t" "title" my-emacs-denote-rename-file-title)
      ("r r" "file" my-emacs-denote-rename-file)]]
    ["Manage"
     ("m a" "Aliases file" my-emacs-denote-find-aliases-file)])

  (defclass my-emacs-denote-suffix (transient-suffix)
    ((advice :initform #'my-emacs-denote-with-context-apply)
     (inapt-if-not :initform #'my-emacs-denote-context-id)))

  (defun my-emacs-denote-context-id (&optional prompt?)
    "Extract and return denote ID from context.

Return nil if nothing found in context.  If PROMPT?, allow
prompting for denote ID as a fallback."
    ;; TODO: Investigate potential bug?  Seems desirable to use
    ;; `denote-get-identifier-at-point', but it doesn't work as expected.
    (cl-flet ((extract-id-or-nil (string)
                (if string (denote-extract-id-from-string string))))
      (or (extract-id-or-nil (word-at-point))
          (extract-id-or-nil (ignore-errors (dired-get-filename)))
          (extract-id-or-nil (buffer-file-name))
          (extract-id-or-nil default-directory)
          (if prompt?
              (denote-retrieve-filename-identifier-with-error
               (denote-file-prompt))))))

  (defun my-emacs-denote-with-context-apply (fun &rest args)
    "Apply FUN to ARGS in the with buffer set to the contextual denote file."
    (with-current-buffer (find-file-noselect (denote-get-path-by-id
                                              (my-emacs-denote-context-id t)))
      (apply fun args)))

  (transient-define-suffix my-emacs-denote-rename-file ()
    :class my-emacs-denote-suffix
    (interactive)
    (call-interactively #'denote-rename-file))

  (transient-define-suffix my-emacs-denote-rename-file-title ()
    :class my-emacs-denote-suffix
    (interactive)
    (call-interactively #'denote-rename-file-title))

  (transient-define-suffix my-emacs-denote-rename-file-keywords ()
    :class my-emacs-denote-suffix
    (interactive)
    (call-interactively #'denote-rename-file-keywords))

  (transient-define-suffix my-emacs-denote-dired-assets-directory (file)
    "Open dired in assets directory associated with denote FILE."
    (interactive (list (denote-file-prompt)))
    (dired (my-emacs-denote-assets-directory file)))

  (transient-define-suffix my-emacs-denote-dired-current-assets-directory ()
    "Open dired in assets directory associated with current buffer file name."
    :class my-emacs-denote-suffix
    (interactive)
    (dired (my-emacs-denote-assets-directory
            (denote-get-path-by-id (my-emacs-denote-context-id)))))

  (transient-define-suffix my-emacs-denote-current-note ()
    "Attempt to find associated denote file from current buffer's file name."
    :class my-emacs-denote-suffix
    (interactive)
    (find-file (denote-get-path-by-id (my-emacs-denote-context-id))))

  (transient-define-suffix my-emacs-denote-find-aliases-file ()
    "Find `my-emacs-denote-aliases-file' from `denote-directory'."
    (interactive)
    (find-file (file-name-concat denote-directory my-emacs-denote-aliases-file))))

;;;; Add `disproject.el' command to initialize a `dir-locals-file' project.

(use-package disproject
  :config
  (transient-insert-suffix 'disproject-manage-projects-dispatch '(0 0)
    '("d" "with `dir-locals-file'" my-emacs-disproject-init-dir-locals-file))
  :config
  (require 'transient)
  (transient-define-suffix my-emacs-disproject-init-dir-locals-file (dir)
    "Initialize a project in DIR with an initial `dir-locals-file'.

This assumes that `dir-locals-file' is a valid project root.

The current buffer name's `file-name-base' is used as initial
input, with the expectation that this function will be most often
used from notes files."
    (interactive "GNew project in: ")
    (let ((project-dir (file-name-concat
                        dir (read-string "Project name: "
                                         (file-name-base (buffer-name))))))
      (make-directory project-dir t)
      (find-file (file-name-concat project-dir dir-locals-file)))))

;;;; Add command to find notes files.

(use-package org
  :config
  (declare-function org-get-tags "org")
  (defvar auto-insert)
  (defun my-emacs-find-notes-file ()
    "Edit a notes file."
    (interactive)
    (let* ((default-directory
            "~/data/areas/notes/")
           (choices
            (mapcar
             (lambda (file)
               (with-current-buffer (org-get-agenda-file-buffer file)
                 (let ((title
                        (or (org-get-title) ""))
                       (tags
                        (save-excursion
                          (goto-char (point-min))
                          (mapconcat #'identity (org-get-tags) ":"))))
                   (cons (concat file " \t" title " \t" tags)
                         file))))
             (mapcar #'file-relative-name
                     ;; Exclude dotfiles.
                     (directory-files-recursively default-directory
                                                  "^[^\\.].*\\.org$"))))
           (choice
            (completing-read "File or new notes name: " choices))
           (file-choice
            (alist-get choice choices nil nil #'equal)))
      (cond
       (file-choice
        (find-file file-choice))
       (t
        (let* ((default-directory
                (read-directory-name "Data directory: "))
               (default-prefix
                (format-time-string "%Y%m%dT%H%M%SZ~" nil t))
               (notes-file
                (read-string "File name: "
                             (cons (concat default-prefix choice ".org")
                                   (1+ (length default-prefix)))))
               (entry-type (string-remove-suffix
                            "s"
                            (capitalize
                             (file-name-nondirectory
                              (directory-file-name default-directory)))))
               (base-name
                (and (string-match "^\\([^~]+~\\)?\\(.*\\)\\.org$" notes-file)
                     (match-string 2 notes-file))))
          (my-emacs-find-new-notes-file
           notes-file (concat entry-type " notes: " base-name))))))))

;;;; Configure `disproject' commands.

(use-package disproject
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch))
  :custom
  (disproject-switch-to-buffer-command #'consult-project-buffer)
  (disproject-find-file-command #'consult-find)
  (disproject-find-regexp-command #'consult-ripgrep)
  (disproject-find-line-command #'consult-line-multi)
  (disproject-find-special-file-suffixes
   '(["Other options"
      (disproject-infix-customize-switch)]
     ["Special files"
      ("c" disproject-find-special-file :file "CHANGELOG")
      ("g g" disproject-find-special-file :file "guix.scm")
      ("g m" disproject-find-special-file :file "manifest.scm")
      (disproject-find-dir-locals-file)
      (disproject-find-dir-locals-2-file)
      ("m" disproject-find-special-file :file ("Makefile" "makefile"
                                               "GNUmakefile"))])))


;;;
;;; (major-modes) Major modes.
;;;

;;;; Don't parse "<" or ">" as parentheses in `org-mode'.

(use-package org
  :hook (org-mode . my-emacs-set-angled-brackets-to-symbol-constituent)
  :preface
  (defun my-emacs-set-angled-brackets-to-symbol-constituent ()
    "Set the syntax for angled brackets (\"<\", \">\") to \"symbol constituent\"."
    (modify-syntax-entry ?< "_")
    (modify-syntax-entry ?> "_")))


;;; DEPRECATED OLD LAYOUT BELOW
;; Configurations should be slowly organized to appropriate places above, and
;; eventually there shouldn't be anything under here anymore.

;;; Global configurations

;;;; Visual interface

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (use-dialog-box nil)
  (echo-keystrokes 0.25))

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t))

;;;; Point control

(make-variable-buffer-local 'scroll-margin)
(make-variable-buffer-local 'scroll-conservatively)

(use-package emacs
  ;; Make point semi-centered with deadzone
  :preface
  (defun remove-scroll-margin ()
    (setq scroll-margin 0))
  :custom
  (scroll-margin 5)
  (scroll-conservatively 1000)
  (next-screen-context-lines 5)
  :config
  (add-hook 'shell-mode-hook
            'remove-scroll-margin)
  (add-hook 'eshell-mode-hook
            'remove-scroll-margin)
  ;; Configure scrollbar/mouse scrolling
  ;;
  ;; TODO: check out mwheel.el and experiment with
  ;; the nano/vim-like vscrolling that I like
  :custom
  (scroll-preserve-screen-position t)
  :config
  (put 'scroll-left 'disabled nil)
  (pixel-scroll-precision-mode t)
  ;; EXPERIMENTAL (comment out precision mode when testing)
  ;;
  ;; (defun custom-scroll-down (arg)
  ;;   (interactive)
  ;;   (message "test - hello")
  ;;   (scroll-down arg))
  ;; (let ((mwheel-scroll-down-function 'custom-scroll-down))
  ;;       (defun custom-mwheel-scroll (event &optional arg)
  ;; 	(interactive (list last-input-event current-prefix-arg))
  ;; 	(funcall-interactively mwheel-scroll event arg)))
  ;; (keymap-global-set "<wheel-down>" 'custom-mwheel-scroll)
  ;; OTHER IDEAS/NOTES:
  ;; - somehow override scroll commands to move point instead of screen
  ;; - any other variables i could use?
  ;; - closest solution right now: https://www.emacswiki.org/emacs/Scrolling
  )

;;;; Editing

(use-package adaptive-wrap
  :config
  (global-adaptive-wrap-prefix-mode 1)
  :preface
  (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
    adaptive-wrap-prefix-mode
    (lambda ()
      (adaptive-wrap-prefix-mode 1))
    :group 'adaptive-wrap)
  (declare-function adaptive-wrap-prefix-mode "adaptive-wrap"))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

;; TODO: Transient-ify avy?
;; https://karthinks.com/software/avy-can-do-anything/
;; Requires figuring out how to separate the action+selection step avy has, so
;; that instead a transient shows up first for actions, then a candidate is
;; selected to act on.
;; We could start with "j" as the (usually default) "jump" action.
(use-package avy
  :bind (("M-j" . avy-goto-char)
         :map isearch-mode-map
         ("M-j" . avy-isearch)))

(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (require-final-newline t))

(use-package emacs
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim))
  :config
  (put 'downcase-region 'disabled nil))

(use-package emacs
  :custom
  (fill-column 80)
  :config
  (global-display-fill-column-indicator-mode 1)
  (toggle-text-mode-auto-fill))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "  (%s/%s)"))

(use-package newcomment
  :bind (("S-<return>" . comment-indent-new-line)))

(use-package unfill
  :after embark
  :defines (embark-region-map
            embark-sentence-map
            embark-paragraph-map)
  :bind ( :map embark-region-map
          ("M-f" . unfill-region)
          :map
          embark-sentence-map
          ("M-f" . unfill-paragraph)
          :map
          embark-paragraph-map
          ("M-f" . unfill-paragraph)))

;; Resource:
;; https://www.vernon-grant.com/Emacs/Discovering-Emacs/4-using-whitespace-mode.html
(use-package whitespace
  :delight (global-whitespace-mode)
  :custom
  (whitespace-style '(face
                      tabs
                      tab-mark
                      empty
                      trailing))
  (whitespace-global-modes '(not shell-mode
                                 help-mode
                                 magit-mode
                                 magit-diff-mode
                                 org-mode
                                 ibuffer-mode
                                 dired-mode))
  (whitespace-display-mappings '((newline-mark ?\n [172 ?\n] [36 ?\n])
                                 (space-mark ?\s [183] [46])
                                 (tab-mark ?\t [187 ?\t] [62 ?\t])))
  :config
  (use-package color
    :config
    (let* ((ws-color (color-lighten-name "#444444" 50)))
      (custom-set-faces
       `(whitespace-newline                ((t (:foreground ,ws-color))))
       `(whitespace-space                  ((t (:foreground ,ws-color))))
       `(whitespace-tab                    ((t (:foreground ,ws-color))))))
    :preface
    (declare-function color-lighten-name "color"))
  (global-whitespace-mode t))

;;;; Completion suite

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :preface
  (declare-function consult--customize-put "consult")
  :defines (xref-show-xrefs-function
            xref-show-definitions-function)
  :functions (consult-register-format
              consult-register-window
              consult-xref)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c M" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ;; orig. repeat-complex-command
         ("C-x M-:" . consult-complex-command)
         ;; orig. switch-to-buffer
         ("C-x b" . consult-buffer)
         ;; orig. switch-to-buffer-other-window
         ("C-x 4 b" . consult-buffer-other-window)
         ;; orig. switch-to-buffer-other-frame
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; orig. bookmark-jump
         ("C-x r b" . consult-bookmark)
         ;; orig. project-switch-to-buffer
         ;; ("C-x p b" . consult-project-buffer) ;use disproject instead
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;; orig. abbrev-prefix-mark (unrelated)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; orig. yank-pop
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ;; orig. goto-line
         ("M-g g" . consult-goto-line)
         ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-HISTORY)
         :map isearch-mode-map
         ;; orig. isearch-edit-string
         ("M-e" . consult-isearch-history)
         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)
         ;; needed by consult-line to detect isearch
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; orig. next-matching-history-element
         ("M-s" . consult-history)
         ;; orig. previous-matching-history-element
         ("M-r" . consult-history))
  :custom
  (consult-find-args (pcase my-emacs-search-excluded-directories
                       (`(,first-dir . ,rest-dirs)
                        (concat "find . ( ("
                                " -name " first-dir
                                (mapconcat
                                 (lambda (name)
                                   (concat " -or -name " name))
                                 rest-dirs)
                                " ) -prune -or -true )"))))
  (consult-async-split-style 'perl-comma)
  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (add-to-list 'consult-async-split-styles-alist
               '(perl-comma :initial "," :function consult--split-perl))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package corfu
  :bind ( :map corfu-map
          ([return] . nil)
          ([tab] . corfu-insert-candidate))
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode t)
  :preface
  (declare-function global-corfu-mode "corfu")
  (declare-function corfu--goto "corfu")
  (declare-function corfu-insert "corfu")
  (defun corfu-insert-candidate ()
    "Insert the current candidate, picking the first if none is selected."
    (interactive)
    (unless (> corfu--index 0)
      (corfu--goto 0))
    (corfu-insert)))

(use-package embark
  :bind ( :map override-global-map
          ("C-." . embark-act)
          ("M-." . embark-dwim)
          ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :preface
  (declare-function marginalia-mode "marginalia")
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :defines (crm-separator)
  :preface
  (declare-function vertico-mode "vertico")
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :bind (:map vertico-map ("<return>" . vertico-directory-enter))
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :config
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;;; Guix

(use-package autoinsert
  :config
  (define-auto-insert
    '("manifest\\.scm" . "Guix manifest file")
    `(nil
      "(specifications->manifest (list" _ "))")))

(use-package guix-popup
  :bind ( :map mode-specific-map
          ("g" . guix)))

(use-package guix-prettify
  :hook ((shell-mode . guix-prettify-mode)
         (dired-mode . guix-prettify-mode)
         (eat-mode . guix-prettify-mode)))

;;;; Minor modes

(use-package display-line-numbers
  :after prog-mode
  :hook (prog-mode . display-line-numbers-mode))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package emacs
  :custom
  (auto-insert-directory (concat user-emacs-directory "inserts"))
  :config
  (auto-insert-mode 1))

(use-package eglot
  :commands eglot
  :config
  ;; https://reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
  (use-package ccls
    :when (package-installed-p 'ccls)
    :after eglot
    :defines ccls-sem-highlight-method
    :custom
    (ccls-initialization-options '(:index
                                   (:comments 2)
                                   :completion
                                   (:detailedLabel t)))
    :config
    (setq ccls-sem-highlight-method 'font-lock)
    ;; TEMP: remove this comment when the above works; this does not work yet due
    ;; to unavailable feature; see: https://github.com/joaotavora/eglot/issues/615
    ;; depending on its implementation the above may need to be tweaked
    ))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package hl-todo
  :config
  (global-hl-todo-mode t)
  :preface
  (declare-function global-hl-todo-mode "hl-todo"))

(use-package repeat
  :config
  (repeat-mode t))

;;;; Miscellaneous

(use-package emacs
  :bind (("C-z" . nil)))

(use-package server
  :config
  (server-start))

;;; Major modes

(use-package dashboard
  :preface
  (declare-function dashboard-setup-startup-hook "dashboard")
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5)
                     (bookmarks . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package dired
  :custom
  (dired-listing-switches "-alh")
  (dired-vc-rename-file t))

(use-package magit
  :commands magit
  :init
  (use-package magit-todos
    :custom
    (magit-todos-keyword-suffix "[[:space:]]\\|:\\|$")
    :config
    ;; TEMP: Partially fix "xxx" keyword parse
    ;; (https://github.com/alphapapa/magit-todos/issues/101)
    (add-to-list 'magit-todos-keywords-list "XXX" t)
    :preface
    (declare-function magit-todos-mode "magit-todos"))
  :config
  (transient-append-suffix #'magit-stash "f"
    '("e" "Edit message" magit-stash-edit-message))
  (use-package magit-section
    :config
    (use-package magit-status
      ;; XXX: Reserve "C-<tab>" for other things (like `tab-bar-mode')
      :bind ( :map magit-status-mode-map
              ("C-<tab>" . nil)
              ("C-c C-<tab>" . magit-section-cycle)
              :repeat-map magit-section-repeat-map
              ("C-<tab>" . magit-section-cycle))))
  (use-package forge)
  :preface
  ;; See discussion on editing stash messages here:
  ;; https://www.github.com/magit/magit/issues/2650
  (defun magit-stash-edit-message (stash message)
    "Change STASH's message to MESSAGE."
    (interactive
     (let* ((stash (magit-read-stash "Rename"))
            (old-msg (magit-git-string "show" "-s" "--format=%s" stash)))
       (list stash (magit-read-string "Stash message" old-msg))))
    (let ((commit (magit-rev-parse stash))
          (inhibit-magit-refresh t))
      (magit-stash-drop stash)
      (magit-stash-store message "refs/stash" commit))
    (magit-refresh))
  (put 'magit-clean 'disabled nil))

(use-package org
  :preface
  (defun org-export-to-pdf-cd (&optional _ _ _ _ _)
    "Change default directory to the canonicalized dirname of this buffer."
    ;; TODO: Fixes issue with export to pdf failing when it's not canonicalized.
    ;; Needs more investigation and might be a good idea to report upstream.
    (cd (file-name-directory (file-truename (buffer-file-name)))))
  :commands org-mode
  :bind (("C-c C-x <backtab>" . org-clock-out)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode . (lambda ()
                       (electric-pair-local-mode -1))))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "SKIP(s@/!)")))
  (org-cycle-inline-images-display t)
  (org-export-in-background t)
  ;; TODO: add a cleaner function that deletes files after some time limit
  (org-preview-latex-image-directory "~/.cache/emacs/ltximg/")
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  :init
  (let ((export-dir "~/Documents/org"))
    (make-directory export-dir t)
    (define-auto-insert
      '(org-mode . "Org file")
      ;; TODO: Set #+category to truncated string of project name
      `(nil
        "#+title: " _ \n
        "#+author:" \n
        "#+date:" \n
        "#+options: author:nil date:nil num:nil toc:nil tags:nil" \n
        "#+startup: showall" \n
        "#+export_file_name: " ,export-dir "/export" \n
        "#+latex_header: \\usepackage{libertine}" \n
        "#+latex_header: \\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}" \n
        "#+latex_header: \\usepackage{lmodern}" \n
        "#+latex_header: \\usepackage[margin=3cm]{geometry}" \n
        "#+latex_header: \\usepackage{setspace}" \n
        "# #+latex_header: \\doublespacing" \n
        "#+options: timestamp:nil" \n
        \n
        \n)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)))
  (plist-put org-format-latex-options :scale 2.0)
  (advice-add 'org-latex-export-to-pdf
              :before #'org-export-to-pdf-cd)
  (setcdr (assoc 'plain-list-item org-blank-before-new-entry) nil)
  (add-to-list 'org-latex-default-packages-alist '("hidelinks" "hyperref" nil))
  :config
  (add-to-list 'org-file-apps
               '("epub" . "xdg-open %s")))

;;;;; Programming languages

(use-package cc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package elisp-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

;; TODO: I have embark override "C-." and "M-.", which are both useful; "C-."
;; has another keybind so it can be ignored, but "M-."
;; (geiser-edit-symbol-at-point) does not.  It might be a good idea to add it
;; to embark-dwim.
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix")
  (add-to-list 'geiser-guile-load-path "~/guix-config/modules"))

(use-package go-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("^go\\.mod\\'" . go-mod-ts-mode)))

(use-package js
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

(use-package python
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-hook 'python-mode-hook
            (lambda ()
              (set-fill-column 79)))
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (set-fill-column 79)))
  :custom
  (python-interpreter "python3")
  (python-shell-dedicated 'project))

(use-package lisp-mode
  :hook ((lisp-mode . enable-paredit-mode)))

(use-package lisp-data-mode
  :hook ((lisp-data-mode . enable-paredit-mode)))

(use-package scheme
  :hook ((scheme-mode . enable-paredit-mode)
         (scheme-mode . guix-devel-mode))
  :init
  (font-lock-add-keywords 'scheme-mode
                          '(("(\\(lambda\\*\\)"
                             (1 font-lock-keyword-face))))
  (put 'lambda* 'scheme-indent-function 1))

(use-package rust-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(rs\\|rlib\\)\\'" . rust-ts-mode)))

(use-package sh-script
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

;;; init.el ends here
