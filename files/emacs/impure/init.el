;;; init.el --- my init.el -*- lexical-binding: t -*-
;; Copyright © 2013-2022 Phil Hagelberg and contributors
;; Copyright © 2023-2025 aurtzy <aurtzy@gmail.com>
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
;; anywhere else.  With the exception of one-shot code, usually code placed
;; here should be deferred in some manner.
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


(eval-when-compile

;;;; Fix some byte-compiler warnings.

  ;; See: https://github.com/jwiegley/use-package/issues/636
  (setq use-package-expand-minimally byte-compile-current-file)

;;;; Always defer `use-package' forms.

  (setq use-package-always-defer t))

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

(use-package denote :demand
  :custom
  (denote-directory "~/data/")
  ;; Avoid traversing past data directories.
  (denote-excluded-directories-regexp "^[^/]*/.*")
  (denote-file-name-slug-functions '((keyword . my-emacs-denote-sluggify-keyword)))
  (denote-known-keywords '("#inbox"))
  :config
  (add-hook 'denote-after-new-note-hook #'my-emacs-denote-mirror-title-to-first-headline)
  (add-hook 'denote-after-new-note-hook #'my-emacs-denote-set-new-note-status-keywords)
  (add-hook 'before-save-hook #'my-emacs-denote-mirror-title-to-first-headline)
  (add-hook 'denote-after-rename-file-hook #'my-emacs-denote-set-status-keywords)
  :preface
  (defun my-emacs-denote-mirror-title-to-first-headline ()
    "Set the first headline to the note's title property (or create it).

This does not do anything if the buffer file does not satisfy
`denote-file-is-note-p'."
    (declare-function org-edit-headline "org")
    (when (and (denote-filename-is-note-p (buffer-file-name))
               (equal "org" (file-name-extension (buffer-file-name))))
      (save-excursion
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (unless (org-at-heading-p)
          (org-insert-heading))
        (if-let* ((title (org-get-title)))
            (org-edit-headline title)
          (user-error "Buffer has no Org title property")))))

  (defun my-emacs-denote-sluggify-keyword (string)
    "Make an appropriate keyword from STRING."
    (defvar org-tag-re)
    (require 'org)
    (downcase (denote-slug-hyphenate
               ;; We assume that `org-tag-re' can match single characters as
               ;; components of a complete tag string.  We then use this
               ;; assumption to filter characters that are considered not a
               ;; valid part of the tag regexp.  This may fail if the regexp
               ;; expands in the future to match multi-character sequences.
               (apply #'concat
                      (seq-map (lambda (char)
                                 (let ((str (string char)))
                                   (if (string-match org-tag-re str) str "")))
                               string)))))

  (defun my-emacs-denote-set-status-keywords (&optional identifier new-note?)
    "Set the current note's status keywords, if applicable.

IDENTIFIER, if non-nil, specifies the identifier of the note to set
keywords for.  Otherwise, use `denote-current-data' to determine the
note to modify.

When NEW-NOTE? is non-nil and the note initially has no keywords,
the \"#inbox\" keyword is included."
    (require 'org)
    (when-let* ((id (or identifier (alist-get 'id denote-current-data)))
                (file (or (denote-get-path-by-id id)
                          (and-let* ((file (buffer-file-name))
                                     (_ (string=
                                         id (denote-extract-id-from-string
                                             (file-name-base file)))))
                            file))))
      (let* ((keywords (denote-extract-keywords-from-path file))
             (new-keywords keywords)
             (denote-rename-confirmations nil)
             (denote-save-buffers (if-let* ((buf (get-file-buffer file)))
                                      ;; Keep the current modified-state of
                                      ;; the buffer, to avoid saving
                                      ;; newly-created notes in case they are
                                      ;; scrapped instead.
                                      (not (buffer-modified-p buf)))))
        ;; "Inbox" status: only apply if there are no keywords, in which case
        ;; it is likely to be either 1. a new note, or 2. an edge case that
        ;; needs additional sorting (perhaps via a new keyword).  This keyword
        ;; must be manually removed, as there is no way to decisively indicate
        ;; "sorting has completed".
        (when (and new-note? (null keywords))
          (push "#inbox" new-keywords))
        (unless (equal keywords new-keywords)
          ;; Don't run this function more than once.
          (let ((denote-after-rename-file-hook
                 (delq 'my-emacs-denote-set-status-keywords
                       denote-after-rename-file-hook)))
            (denote-rename-file
             file 'keep-current new-keywords 'keep-current 'keep-current))))))

  (defun my-emacs-denote-set-new-note-status-keywords ()
    (my-emacs-denote-set-status-keywords nil :new-note))

  ;; Functions for accessing assets directories.

  (defun my-emacs-denote-assets-directory (file &optional prompt?)
    "Return assets directory from FILE note.

If PROMPT?, user will be prompted to create the directory if it is not
found.  Otherwise, nil may be returned."
    (interactive
     (list (denote-file-prompt nil "Select FILE associated with assets")))
    (let* ((file (or file (buffer-file-name)))
           (identifier (denote-retrieve-filename-identifier-with-error file))
           (assets-dir-as-file (file-name-concat
                                (file-name-directory file) identifier))
           (assets-dir (file-name-as-directory assets-dir-as-file)))
      (cond
       ((file-exists-p assets-dir)
        assets-dir)
       (prompt?
        (if (y-or-n-p "Assets directory doesn't exist.  Create it? ")
            (make-directory assets-dir t)
          (user-error "Assets directory doesn't exist"))
        assets-dir))))

  (defun my-emacs-denote-aliases-assoc-ref (alias)
    "Return the denote ID associated with ALIAS.  May return nil."
    (if-let* ((mappings-file (file-name-concat denote-directory
                                               my-emacs-denote-aliases-file))
              ((file-exists-p mappings-file))
              (mappings (read (with-temp-buffer
                                (insert-file-contents mappings-file)
                                (buffer-string)))))
        (cdr (assoc alias mappings))
      nil))
  :functions ( org-next-visible-heading org-at-heading-p
               org-insert-heading org-get-title))


;;;
;;; (settings) Function/variable definitions and customizations.
;;;

;;;; Maximize fontifications with tree-sitter!!1!

(use-package treesit
  :custom (treesit-font-lock-level 4))

;;;; Add some upstream improvements to `csharp-ts-mode' indentation.
;; TEMP: Should be made redundant with Emacs 31.

(use-package csharp-mode
  :config
  (setf (alist-get 'c-sharp csharp-ts-mode--indent-rules)
        (append
         ;; See: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-09/msg00394.html
         '(((parent-is "array_creation_expression") parent-bol 0)
           ((match "{" "initializer_expression" ) parent-bol 0)
           ((parent-is "member_access_expression")
            parent-bol csharp-ts-mode-indent-offset))
         (alist-get 'c-sharp csharp-ts-mode--indent-rules))))

;;;; Apply a fix to `csharp--color-forwards' by redefining it.

(use-package csharp-mode
  :config
  ;; Redefine this function (copied from `csharp-mode'), with a tweak.
  (defun csharp--color-forwards (font-lock-face)
    (let (id-beginning)
      (goto-char (match-beginning 0))
      (forward-word)
      (while (and (not (or (eq (char-after) ?\;)
                           (eq (char-after) ?\{)
                           ;; HACK: Tweak here.  We don't want to keep
                           ;; fontifying if there's nothing left in the
                           ;; buffer.  This fixes an end-of-buffer error when
                           ;; fontifying, which can be seen in Markdown/Org
                           ;; code snippets or while using e.g. OmniSharp and
                           ;; hovering over namespaces.
                           (/= (point) (point-max))))
                  (progn
                    (forward-char)
                    (c-forward-syntactic-ws)
                    (setq id-beginning (point))
                    (> (skip-chars-forward
                        (c-lang-const c-symbol-chars))
                       0))
                  (not (get-text-property (point) 'face)))
        (c-put-font-lock-face id-beginning (point) font-lock-face)
        (c-forward-syntactic-ws)))))

;;;; Enable terminal emulation with Eat in Eshell.

(use-package eat :demand
  :config (eat-eshell-mode 1))

;;;; Add guile-lsp-server as a server choice for `eglot'.

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(scheme-mode . ("guile-lsp-server"))))

;;;; Configure and setup `dashboard' during initialization.

(use-package dashboard :demand
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5)
                     (bookmarks . 10)))
  :config
  (dashboard-setup-startup-hook)
  :functions (dashboard-setup-startup-hook))

;;;; Start Emacs server for this session, if there isn't already one.

(use-package server :demand
  :config (server-start))

;;;; Configure `org' settings.

(use-package org
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "SKIP(s@/!)")))
  (org-cycle-inline-images-display t)
  (org-export-in-background t)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)))
  (setcdr (assoc 'heading org-blank-before-new-entry) t)
  (setcdr (assoc 'plain-list-item org-blank-before-new-entry) nil)
  (add-to-list 'org-file-apps
               '("epub" . "xdg-open %s")))

;;;; Configure `python' settings.

(use-package python
  :custom
  (python-interpreter "python3")
  (python-shell-dedicated 'project))

;;;; Include Guix-related modules in `geiser-guile-load-path'.

;; TODO: I have embark override "C-." and "M-.", which are both useful; "C-."
;; has another keybind so it can be ignored, but "M-."
;; (geiser-edit-symbol-at-point) does not.  It might be a good idea to add it
;; to embark-dwim.
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix")
  (add-to-list 'geiser-guile-load-path "~/guix-config/modules"))

;;;; Configure `magit'.

(use-package magit
  ;; XXX: Reserve "C-<tab>" for other things (like `tab-bar-mode')
  :bind ( :map magit-status-mode-map
          ("C-<tab>" . nil)
          ("C-c C-<tab>" . magit-section-cycle)
          :repeat-map magit-section-repeat-map
          ("C-<tab>" . magit-section-cycle))
  :config
  (put 'magit-clean 'disabled nil))

;;;; Configure `forge'.

(use-package forge :demand :after magit)

;;;; Configure `magit-todos'.

(use-package magit-todos :demand :after magit
  :custom
  (magit-todos-keyword-suffix "[[:space:]]\\|:\\|$")
  :config
  ;; TEMP: Partially fix "xxx" keyword parse
  ;; (https://github.com/alphapapa/magit-todos/issues/101)
  (add-to-list 'magit-todos-keywords-list "XXX" t))

;;;; Provide function for setting project to associated assets directory.

(use-package project
  :preface
  (defun my-emacs-project-set-assets-directory-override ()
    "Set a file-local project directory override to associated assets directory."
    (let* ((file (buffer-file-name))
           (assets-directory (my-emacs-denote-assets-directory file t)))
      (make-local-variable 'project-current-directory-override)
      (setq project-current-directory-override assets-directory))))

;;;; Set personal dictionary location.

(use-package ispell
  :custom
  (ispell-personal-dictionary
   (when-let* ((identifier (my-emacs-denote-aliases-assoc-ref "emacs")))
     (let* ((notes-file (denote-get-path-by-id identifier))
            (assets-dir (my-emacs-denote-assets-directory notes-file t)))
       (file-name-concat assets-dir ".static/aspell.en.pws")))))

;;;; Add org link type for entry-local denote assets.

(use-package ol
  :config
  (org-link-set-parameters
   "denote-asset"
   :follow #'my-emacs-denote-asset-follow
   :complete #'my-emacs-denote-asset-complete)
  :preface
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
            (my-emacs-denote-assets-directory
             (denote-get-path-by-id identifier) t))
           (relative-path
            (file-relative-name (read-file-name "Denote asset: " assets-dir)
                                assets-dir)))
      (concat "denote-asset:" relative-path)))
  :functions (org-link-set-parameters org-link-open-as-file))

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
    :test #'equal))
  :functions (org-store-new-agenda-file-list))

;;;; Define function for creating new notes files.

(use-package autoinsert
  :init
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
  :custom (org-agenda-span 10))

;;;; Specify additional `project.el' root markers.

(use-package project
  :custom (project-vc-extra-root-markers `(,dir-locals-file
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
  (visible-bell t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  :custom-face
  (mode-line ((t . (:family "Iosevka Etoile"))))
  (mode-line-inactive ((t . (:family "Iosevka Etoile"))))
  :config
  ;; Disable any active themes before loading one to prevent unintentional
  ;; mixing of themes.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark t)
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
  :custom (load-prefer-newer t))

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
  :custom (enable-remote-dir-locals t))

;;;; Enable `imenu' support for `use-package'.

(use-package use-package-core
  :custom (use-package-enable-imenu-support t))

;;;; Customize `flymake'.

(use-package flymake
  :custom (flymake-number-of-errors-to-display 4))

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

;;;; Don't manage C# Razor files with Eglot.

(use-package eglot :after web-mode
  :hook (eglot-managed-mode . my-emacs-eglot-managed-mode-off-if-razor)
  :config
  (defun my-emacs-eglot-managed-mode-off-if-razor ()
    "Disable `eglot--managed-mode' if buffer is a C# Razor file."
    (when (and (eglot-managed-p)
               (buffer-file-name)
               (derived-mode-p '(web-mode csharp-mode))
               (string-match "\\.razor$" (buffer-file-name)))
      (eglot--managed-mode-off)))
  :functions (eglot--managed-mode-off eglot-managed-p))

;;;; Enable `guix-devel-mode' in Scheme buffers.

(use-package scheme :hook (scheme-mode . guix-devel-mode))

;;;; Enable `paredit-mode' in Lisp-mode-related buffers.

(use-package elisp-mode :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package lisp-mode :hook ((lisp-mode . enable-paredit-mode)
                              (lisp-data-mode . enable-paredit-mode)))

(use-package scheme :hook (scheme-mode . enable-paredit-mode))

;;;; Hide `dired' details by default.

(use-package dired :hook (dired-mode . dired-hide-details-mode))

;;;; Shrink `paredit' lighter.

(use-package paredit
  :delight " (ed")

;;;; Configure `envrc-global-mode'.

(use-package envrc :hook (after-init . envrc-global-mode)
  :custom
  (envrc-none-lighter '(" env/"
                        (:propertize "N" face envrc-mode-line-none-face)))
  (envrc-on-lighter '(" env/"
                      (:propertize "O" face envrc-mode-line-on-face)))
  (envrc-error-lighter '(" env/"
                         (:propertize "E" face envrc-mode-line-error-face))))

;;;; Configure `editorconfig-mode'.

(use-package editorconfig :demand
  :delight " EdCfg"
  :config (editorconfig-mode 1))

;;;; Configure `global-page-break-lines-mode'.

(use-package page-break-lines :demand
  :delight
  :config (global-page-break-lines-mode t))

;;;; Enable `compilation-minor-mode' in log files.

(use-package compile :mode ("\\.log\\'" . compilation-minor-mode))

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

(use-package denote :demand
  :config (denote-rename-buffer-mode t))

;;;; Add fontification for denote files in `dired-mode'.

(use-package dired :hook (dired-mode . denote-dired-mode))

;;;; Enable context menus.

(use-package mouse
  :config (context-menu-mode))

;;;; Disable menu bar, which doesn't correctly apply transparency on GNOME.

(use-package emacs
  :config (menu-bar-mode -1))

;;;; Set up `flymake-mode' for programming-mode buffers.

(use-package flymake :hook prog-mode)

;;;; Enable some Emacs file history tracking modes.

(use-package emacs
  :config
  (savehist-mode 1)
  (save-place-mode 1)
  (recentf-mode 1))


;;;
;;; (transients) Transients.
;;;

;;;; Use Casual I-search by default.

(use-package casual-isearch :demand
  :bind (("C-s" . my-emacs-casual-isearch-forward-maybe-tmenu)
         ("C-r" . my-emacs-casual-isearch-backward-maybe-tmenu)
         :map minibuffer-local-isearch-map
         ("RET" . my-emacs-casual-isearch-exit-minibuffer))
  :preface
  (defvar my-emacs--casual-isearch-open-menu? nil)
  (defun my-emacs-casual-isearch-exit-minibuffer ()
    (interactive)
    (setq my-emacs--casual-isearch-open-menu? t)
    (exit-minibuffer))
  (transient-define-prefix my-emacs-casual-isearch-forward-maybe-tmenu
    (&optional regexp-p)
    (interactive "P")
    (let ((my-emacs--casual-isearch-open-menu? nil))
      (isearch-mode t (not (null regexp-p)))
      (isearch-edit-string)
      (when my-emacs--casual-isearch-open-menu?
        (transient-setup 'casual-isearch-tmenu))))
  (transient-define-prefix my-emacs-casual-isearch-backward-maybe-tmenu
    (&optional regexp-p)
    (interactive "P")
    (let ((my-emacs--casual-isearch-open-menu? nil))
      (isearch-mode nil (not (null regexp-p)))
      (isearch-edit-string)
      (when my-emacs--casual-isearch-open-menu?
        (transient-setup 'casual-isearch-tmenu)))))

;;;; Replace Casual EditKit's Project menu with Disproject.

(use-package casual-editkit
  :config
  (transient-replace-suffix 'casual-editkit-main-tmenu "P"
    '("P" "Project›" disproject-dispatch)))

;;;; Configure Sharper CLI wrapper.

(use-package sharper :demand
  :config
  (require 'disproject)
  ;; HACK: Replace `sharper--run-async-shell' completely to use a `compile'
  ;; buffer instead for running commands.  Also respect disproject
  ;; environment, when possible.
  (defun my-emacs-sharper-run-with-compile ( command bufname
                                             &optional
                                             _buffer-reuse-behavior)
    (let ((compilation-buffer-name-function
           (lambda (name-of-mode)
             (concat "*" name-of-mode "-" (string-trim-left bufname "\\*")))))
      (disproject-with-env+root
        (compile command))))
  (advice-add #'sharper--run-async-shell :override
              #'my-emacs-sharper-run-with-compile))

;;;; Configure Casual.

(use-package casual :demand
  :bind ("C-c t" . my-emacs-casual-auto-tmenu)
  :preface
  (defun my-emacs-casual-auto-tmenu (&optional edit?)
    "DWIM-dispatch a Casual menu, based on the current major mode.

With prefix arg, unconditionally open the EditKit menu."
    (interactive (list current-prefix-arg))
    (declare-function casual-agenda-tmenu "casual-agenda")
    (declare-function casual-bookmarks-tmenu "casual-bookmarks")
    (declare-function casual-calc-tmenu "casual-calc")
    (declare-function casual-calendar-tmenu "casual-calendar")
    (declare-function casual-dired-tmenu "casual-dired")
    (declare-function casual-editkit-main-tmenu "casual-editkit-main")
    (declare-function casual-help-tmenu "casual-help")
    (declare-function casual-eshell-tmenu "casual-eshell")
    (declare-function casual-ibuffer-tmenu "casual-ibuffer")
    (declare-function casual-image-tmenu "casual-image")
    (declare-function casual-info-tmenu "casual-info")
    (declare-function casual-isearch-tmenu "casual-isearch")
    (declare-function casual-make-tmenu "casual-make")
    (declare-function casual-man-tmenu "casual-man")
    (declare-function casual-re-builder-tmenu "casual-re-builder")
    ;; Use `call-interactively' instead of `transient-setup' to enable use of
    ;; autoloads.
    (call-interactively
     (cond
      (edit? #'casual-editkit-main-tmenu)
      ((derived-mode-p 'org-agenda-mode) #'casual-agenda-tmenu)
      ((derived-mode-p 'bookmark-bmenu-mode) #'casual-bookmarks-tmenu)
      ((derived-mode-p 'calc-mode) #'casual-calc-tmenu)
      ((derived-mode-p 'calendar-mode) #'casual-calendar-tmenu)
      ((derived-mode-p 'dired-mode) #'casual-dired-tmenu)
      ((derived-mode-p 'eshell-mode) #'casual-eshell-tmenu)
      ((derived-mode-p 'help-mode) #'casual-help-tmenu)
      ((derived-mode-p 'ibuffer-mode) #'casual-ibuffer-tmenu)
      ((derived-mode-p 'image-mode) #'casual-image-tmenu)
      ((derived-mode-p 'Info-mode) #'casual-info-tmenu)
      ((derived-mode-p 'make-mode) #'casual-make-tmenu)
      ((derived-mode-p 'Man-mode) #'casual-man-tmenu)
      ((derived-mode-p 'reb-lisp-mode) #'casual-re-builder-tmenu)
      ;; Assume that if nothing matches, EditKit is likely what the user
      ;; wants.
      (#'casual-editkit-main-tmenu)))))

;;;; Configure AGitjo.

(use-package agitjo :demand :after magit)

;;;; Add command to open files externally, with xdg-open.

(use-package emacs
  :preface
  (defun my-emacs-current-buffer-xdg-open ()
    "Run \"xdg-open\" on the file that the current buffer is visiting.

By default, attempt to use `buffer-file-name'; if that is not available,
then `default-directory'."
    (interactive)
    (call-process "xdg-open" nil nil nil
                  (or (buffer-file-name) default-directory))))

;;;; Add commands for managing `trusted-content'.

(use-package emacs
  :preface
  (defun my-emacs-trust-content-in-file-or-directory (file-or-directory)
    "Add the current buffer's file name to `trusted-content'."
    (interactive "fFile or directory: ")
    (add-to-list 'trusted-content
                 (if (file-directory-p file-or-directory)
                     (file-name-as-directory file-or-directory)
                   file-or-directory))
    (message "Added to `trusted-content': %s" file-or-directory)))

(use-package disproject
  :config
  (transient-define-suffix my-emacs-disproject-trust-content ()
    "Add the project's root directory to `trusted-content'.

The default description provides an indicator that is colored as
`transient-enabled-suffix' or `transient-disabled-suffix' based on
whether the directory is trusted or not, respectively."
    :description
    (lambda ()
      (concat "Trust content"
              (if-let* ((scope (disproject--scope))
                        (project (disproject-scope-selected-project scope))
                        (root (disproject-project-root project)))
                  (concat
                   " ("
                   (if (member root trusted-content)
                       (propertize "*" 'face 'success)
                     (propertize "!" 'face 'error))
                   ")")
                "")))
    (interactive)
    (disproject-with-root
      (add-to-list 'trusted-content default-directory)))
  (transient-insert-suffix 'disproject-dispatch "M-x"
    '("C-t" my-emacs-disproject-trust-content))
  :functions (disproject--scope disproject-with-root-apply))

;;;; Add custom global key-binds for Org mode.

(use-package org
  :bind (("C-c C-x <backtab>" . org-clock-out)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

;;;; Translate ANSI escape sequences in compilation buffer to text properties.

(use-package ansi-color :demand :after compile
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;; Add suffix to `magit-stash' for editing stash messages.

(use-package magit-stash :demand :after magit
  :autoload ( magit-stash magit-read-stash magit-git-string magit-read-string
              magit-rev-parse magit-stash-drop magit-stash-store magit-refresh)
  :config
  (defun magit-stash-edit-message (stash message)
    "Change STASH's message to MESSAGE."
    (interactive
     (let* ((stash (magit-read-stash "Rename"))
            (old-msg (magit-git-string "show" "-s" "--format=%s" stash)))
       (list stash (magit-read-string "Stash message" old-msg))))
    (let ((commit (magit-rev-parse stash)))
      (magit-stash-drop stash)
      (magit-stash-store message "refs/stash" commit))
    (magit-refresh))
  ;; See discussion on editing stash messages here:
  ;; https://www.github.com/magit/magit/issues/2650
  (transient-append-suffix #'magit-stash "f"
    '("e" "Edit message" magit-stash-edit-message)))

;;;; Set up `vundo' key-bind.

(use-package vundo
  :bind ("C-M-?" . vundo))

;;;; Set up `envrc' command map.

(use-package envrc
  :bind ("C-c d" . #'envrc-command-map))

;;;; Add a Transient menu for managing denote files.
;; TODO: Support denote silos.

(use-package transient
  :bind ("C-c n" . my-emacs-denote-dispatch)
  :preface
  (require 'thingatpt)

  (defun my-emacs-denote-context-id (&optional prompt?)
    "Extract and return denote ID from context.

Return nil if nothing found in context.  If PROMPT?, allow
prompting for denote ID as a fallback."
    (cl-flet ((extract-id-or-nil (string)
                (if string (denote-extract-id-from-string string))))
      (or (transient-scope)
          (extract-id-or-nil (word-at-point))
          (extract-id-or-nil (ignore-errors (dired-get-filename)))
          (extract-id-or-nil (buffer-file-name))
          (extract-id-or-nil default-directory)
          (if prompt?
              (denote-retrieve-filename-identifier-with-error
               (denote-file-prompt))))))

  (defun my-emacs-denote-context-file ()
    "Get the current context's file name.  May return nil."
    (if-let* ((id (my-emacs-denote-context-id)))
        (cond
         ((denote-get-path-by-id id))
         ;; File may not necessarily exist if it's not saved, yet.
         ((let ((current-file (buffer-file-name)))
            (and (equal id (denote-extract-id-from-string current-file))
                 current-file))))))

  (defun my-emacs-denote-with-context-apply (fun &rest args)
    "Apply FUN to ARGS in the with buffer set to the contextual denote file."
    (if-let* ((file (my-emacs-denote-context-file)))
        (with-current-buffer (find-file-noselect file)
          (apply fun args))
      (error "Denote context is invalid")))

  (transient-define-suffix my-emacs-denote-dired-assets-directory (file)
    "Open dired in assets directory associated with denote FILE."
    (interactive (list (denote-file-prompt)))
    (dired (my-emacs-denote-assets-directory file t)))

  (transient-define-suffix my-emacs-denote-find-aliases-file ()
    "Find `my-emacs-denote-aliases-file' from `denote-directory'."
    (interactive)
    (find-file (file-name-concat denote-directory my-emacs-denote-aliases-file)))

  (transient-define-suffix my-emacs-denote-find-context-file ()
    "Open the contextual denote file from transient state."
    (interactive)
    (find-file (denote-get-path-by-id (my-emacs-denote-context-id t))))

  (transient-define-suffix my-emacs-denote-find-context-assets-directory ()
    "Run Dired in the assets directory from current denote context."
    (interactive)
    (dired (my-emacs-denote-assets-directory
            (denote-get-path-by-id (my-emacs-denote-context-id t)) t)))

  (transient-define-suffix my-emacs-denote-find-denote-directory ()
    "Run Dired in `denote-directory'."
    (interactive)
    (dired denote-directory))

  (transient-define-suffix my-emacs-denote-find-regexp ()
    "Find regexp in `denote-directory' notes."
    (interactive)
    (let ((default-directory denote-directory))
      (consult-ripgrep (denote-directory-files))))

  (transient-define-suffix my-emacs-denote-switch-context-id ()
    "Switch context to another denote file."
    :transient t
    (interactive)
    ;; Ignore the current buffer so we can select the current denote file.
    (with-temp-buffer
      (oset (transient-prefix-object) scope
            (denote-retrieve-filename-identifier-with-error
             (denote-file-prompt)))))

  (transient-define-suffix my-emacs-rename-to-location (dir)
    "Move context's denote files (including assets) to another location."
    (interactive (list (denote-subdirectory-prompt)))
    (if-let* ((file (my-emacs-denote-context-file)))
        (with-current-buffer (find-file-noselect file)
          (let* ((file-nondirectory (file-name-nondirectory file))
                 (target (file-name-concat dir file-nondirectory)))
            ;; File may not necessarily be saved to disk yet, so rename based
            ;; off buffer instead of file path.
            (rename-visited-file target)
            (message "Renamed: %s -> %s" file target))
          (when-let* ((assets-dir (my-emacs-denote-assets-directory file))
                      (assets-dir-as-file (directory-file-name assets-dir))
                      (target (file-name-concat
                               dir (file-name-nondirectory assets-dir-as-file))))
            (rename-file assets-dir-as-file target)
            (message "Renamed: %s -> %s" file target)))
      (user-error "No file found from context")))

  ;; TODO: Add commands from:
  ;; <https://protesilaos.com/emacs/denote#h:998ae528-9276-47ec-b642-3d7355a38f27>
  (transient-define-prefix my-emacs-denote-dispatch ()
    :refresh-suffixes t
    [:description
     (lambda ()
       (concat (propertize "Denote context: " 'face 'transient-heading)
               (if-let* ((file (my-emacs-denote-context-file)))
                   (propertize (file-name-nondirectory file)
                               'face 'transient-value)
                 (propertize "None detected" 'face 'transient-inapt-suffix))))
     ("n" "Switch denote context" my-emacs-denote-switch-context-id)
     ("N" "New notes file" denote-subdirectory)]
    ["Contextual commands"
     :inapt-if-not my-emacs-denote-context-id
     :pad-keys t
     [ :advice my-emacs-denote-with-context-apply
       ;; TODO: Add command: Create static assets directory.
       ;; TODO: Display "create assets directory" if it doesn't exist.
       ("a" "Open assets directory" my-emacs-denote-find-context-assets-directory)
       ("f" "Open file" my-emacs-denote-find-context-file)]
     ["Rename"
      :advice my-emacs-denote-with-context-apply
      ("r k" "keywords" denote-rename-file-keywords)
      ("r m" "to another location" my-emacs-rename-to-location)
      ("r t" "title" denote-rename-file-title)
      ("r r" "file" denote-rename-file)]]
    ["Find"
     ("A" "aliases file" my-emacs-denote-find-aliases-file)
     ("D" "denote directory" my-emacs-denote-find-denote-directory)
     ("R" "regexp in notes" my-emacs-denote-find-regexp)]
    (interactive)
    (transient-setup
     'my-emacs-denote-dispatch nil nil
     :scope (my-emacs-denote-context-id))))

;;;; Add `disproject.el' command to initialize a `dir-locals-file' project.

(use-package disproject
  :config
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
      (find-file (file-name-concat project-dir dir-locals-file))))

  (transient-insert-suffix 'disproject-manage-projects-dispatch '(0 0)
    '("d" "with `dir-locals-file'" my-emacs-disproject-init-dir-locals-file)))

;;;; Add command to find notes files.

(use-package org
  :config
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
           notes-file (concat entry-type " notes: " base-name)))))))
  :functions (org-get-agenda-file-buffer org-get-tags))

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
      ("c" disproject-find-special-file :file ("CHANGELOG" "CHANGELOG.org"))
      ("e" disproject-find-special-file :file ".envrc")
      ("g g" disproject-find-special-file :file "guix.scm")
      ("g h" disproject-find-special-file :file "hall.scm")
      ("g m" disproject-find-special-file :file "manifest.scm")
      (disproject-find-dir-locals-file)
      (disproject-find-dir-locals-2-file)
      ("m" disproject-find-special-file :file ("Makefile" "makefile"
                                               "GNUmakefile"))
      ("r" disproject-find-special-file :file ("README.org" "README"
                                               "README.md"))])))


;;;
;;; (major-modes) Major modes.
;;;

;;;; Map C#-related major modes to tree-sitter alternatives.

(use-package csharp-mode
  :init
  ;; FIXME: See: https://codeberg.org/guix/guix/issues/580
  (add-to-list 'treesit-load-name-override-list
               '(c-sharp "libtree-sitter-c_sharp" "tree_sitter_c_sharp"))
  (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode)))

;;;; Configure a polymode with `web-mode' and `csharp-ts-mode' for Razor files.

(use-package web-mode :mode (("\\.razor\\'" . poly-razor-mode))
  :init
  (require 'polymode)
  (require 'pcase)

  (defun poly-razor-mode-code-directive-tail-matcher (_arg)
    "Match the tail of a @code directive chunk."
    (re-search-backward "{")
    (or (ignore-error scan-error
          (forward-sexp)
          (cons (1- (point)) (point)))
        (cons (point-max) (point-max))))

  (define-hostmode poly-razor-mode-hostmode
    :mode 'web-mode
    ;; Prevent fontification of inner modes, since `web-mode' tries to
    ;; override with its own.
    :protect-font-lock t)
  (define-innermode poly-razor-mode-code-directive-innermode
    ;; FIXME: `pm-get-mode-symbol-from-name' prioritizes exact name match over
    ;; remapping, so `csharp-mode' is never translated even if a remap to
    ;; `csharp-ts-mode' is available.
    :mode 'csharp-ts-mode
    :head-matcher "@code\\s-*{"
    :tail-matcher #'poly-razor-mode-code-directive-tail-matcher
    ;; Attempting to fontify these with `web-mode' will not work due to the
    ;; host mode font-lock protection.
    :head-mode 'fundamental
    :tail-mode 'fundamental
    :body-indent-offset 'web-mode-code-indent-offset)
  (define-polymode poly-razor-mode
    :hostmode 'poly-razor-mode-hostmode
    :innermodes '(poly-razor-mode-code-directive-innermode))

  ;; HACK: `treesit.el' only supports embedding tree-sitter languages inside
  ;; tree-sitter languages, so we hack up our own embedding logic here to fix
  ;; issues with e.g. fontification getting confused as a result of errors
  ;; from parsing the entire buffer instead of just the innermode region.
  (defun poly-razor-mode-update-ranges ()
    (when (and poly-razor-mode (buffer-modified-p))
      (save-excursion
        (let (ranges parser)
          (pm-map-over-spans
           (lambda (span)
             (pcase span
               (`(body ,begin ,end ,_obj)
                (when (with-current-buffer (pm-span-buffer span)
                        (and treesit-primary-parser
                             ;; We assume that there's only one primary
                             ;; parser, so it's fine to set it multiple times.
                             (setq parser treesit-primary-parser)))
                  (push (cons begin end) ranges))))))
          (when parser
            (treesit-parser-set-included-ranges parser ranges))))))
  (add-hook 'post-command-hook #'poly-razor-mode-update-ranges)

  ;; HACK: Update syntax highlighting more aggressively to work around an
  ;; issue with fontification not applying.  Possibly related to this issue:
  ;; https://github.com/polymode/polymode/issues/297
  (defun poly-razor-mode-font-lock-update (&rest _)
    (when poly-razor-mode (call-interactively #'font-lock-debug-fontify)))
  (defun poly-razor-mode-font-lock-update-when-web-mode (&rest _)
    ;; Optimization: Only re-fontify if the buffer is modified while editing
    ;; in `web-mode' (`csharp-ts-mode' fontifies fine on its own).
    (when (and (eq 'web-mode major-mode) (buffer-modified-p))
      (poly-razor-mode-font-lock-update)))
  (add-hook 'post-command-hook #'poly-razor-mode-font-lock-update-when-web-mode)

  ;; HACK: Fix indentation with bracketed template blocks.
  (defun poly-razor-mode-bracketed-block-previous-position (&optional pos)
    "Return the position of the previous bracketed block.

`web-mode-indent-line' assumes that the position of
`web-mode-block-previous' has the correct indentation offset, which is
not necessarily true when there are nested directives (a result of using
`previous-single-property-change'?).  This is intended to replace that
behavior (see: search \"I142\" in `web-mode-indent-line') with an
alternative algorithm that is based on traversing s-expressions
naturally formed by the brackets."
    (setq pos (or pos (point)))
    (save-excursion
      (goto-char pos)
      (goto-char (pos-bol))
      (cond
       ((looking-at-p "^\\s-*}")
        (re-search-forward "}")
        (backward-sexp)
        (web-mode-block-previous-position))
       ((looking-at-p "^\\s-*\\({\\|else\\)")
        (backward-sexp)
        (web-mode-block-previous-position))
       (t
        (web-mode-block-beginning-position (pos-eol))))))
  (defun poly-razor-mode-indent-line-advice (fun &rest args)
    "Hijack `web-mode-block-previous' to get correct indentation for a block."
    (if poly-razor-mode
        ;; NOTE: `web-mode-indent-line' is a pretty big function that does
        ;; lots of things, so try to limit the number of cases where overrides
        ;; apply, if possible.
        (cl-letf
            ;; This assumes that `web-mode-block-previous-position' is always
            ;; called in `web-indent-line' with the intention of finding the
            ;; start of a bracketed block.
            (((symbol-function 'web-mode-block-previous-position)
              #'poly-razor-mode-bracketed-block-previous-position))
          (apply fun args))
      (apply fun args)))
  (advice-add #'web-mode-indent-line :around
              #'poly-razor-mode-indent-line-advice)

  ;; HACK: Fix indentation for markup that proceeds a closing bracket.
  (defun poly-razor-mode-markup-indentation-advice (fun pos)
    "Return intended indentation for current line.

Around advice, for `web-mode-markup-indentation'.

`web-mode-indent-line' does not take closing brackets
(i.e. \"}\") into account when computing offset.  To remedy this, we
search backwards for the next non-whitespace character.  If this
character is a closing bracket, we assume POS's line should match its
indentation."
    (or (if poly-razor-mode
            (save-excursion
              (goto-char pos)
              (and (skip-chars-backward "[:space:]")
                   (char-equal ?} (char-before))
                   (current-indentation))))
        (funcall fun pos)))
  (advice-add #'web-mode-markup-indentation :around
              #'poly-razor-mode-markup-indentation-advice)
  :functions ( web-mode-indent-line web-mode-block-previous-position
               web-mode-block-beginning-position web-mode-markup-indentation
               font-lock-debug-fontify))

;;;; Map Shell-related major modes to tree-sitter alternatives.

(use-package sh-script
  :init (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

;;;; Map Rust major modes to tree-sitter alternatives.

(use-package rust-ts-mode
  :init (add-to-list 'auto-mode-alist
                     '("\\.\\(rs\\|rlib\\)\\'" . rust-ts-mode)))

;;;; Make formatting tweaks in Scheme buffers.

(use-package scheme
  :init
  (font-lock-add-keywords 'scheme-mode
                          '(("(\\(lambda\\*\\)"
                             (1 font-lock-keyword-face))))
  (put 'lambda* 'scheme-indent-function 1))

;;;; Map Python major modes to tree-sitter alternatives.

(use-package python
  :init (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;;;; Set `fill-column' for Python modes.

(use-package python
  :init
  (defun my-emacs-python-set-fill-column ()
    (set-fill-column 79))
  (add-hook 'python-mode-hook #'my-emacs-python-set-fill-column)
  (add-hook 'python-ts-mode-hook #'my-emacs-python-set-fill-column))

;;;; Map JavaScript major modes to tree-sitter alternatives.

(use-package js
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

;;;; Map Go-language-related major modes to tree-sitter alternatives.

(use-package go-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("^go\\.mod\\'" . go-mod-ts-mode)))

;;;; Map C-language-related major modes to tree-sitter alternatives.

(use-package c-ts-mode
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

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

(use-package adaptive-wrap :demand
  :config
  (global-adaptive-wrap-prefix-mode 1)
  :preface
  (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
    adaptive-wrap-prefix-mode
    (lambda ()
      (adaptive-wrap-prefix-mode 1))
    :group 'adaptive-wrap)
  (declare-function adaptive-wrap-prefix-mode "adaptive-wrap"))

(use-package autorevert :demand
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
  :bind ( :map embark-region-map
          ("M-f" . unfill-region)
          :map
          embark-sentence-map
          ("M-f" . unfill-paragraph)
          :map
          embark-paragraph-map
          ("M-f" . unfill-paragraph))
  :defines (embark-region-map
            embark-sentence-map
            embark-paragraph-map))

;; Resource:
;; https://www.vernon-grant.com/Emacs/Discovering-Emacs/4-using-whitespace-mode.html
(use-package whitespace :demand
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
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1)
  :functions (marginalia-mode))

(use-package orderless :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :preface
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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  :defines (crm-separator)
  :functions (vertico-mode))

;;;; Guix

(use-package autoinsert :demand
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

(use-package elec-pair :demand
  :config (electric-pair-mode 1))

(use-package eldoc
  :custom (eldoc-documentation-strategy #'eldoc-documentation-compose))

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

(use-package hl-todo :demand
  :config (global-hl-todo-mode t))

(use-package repeat :demand
  :config (repeat-mode t))

;;;; Miscellaneous

(use-package emacs
  :bind (("C-z" . nil)))

;;; Major modes

(use-package dired
  :custom
  (dired-listing-switches "-alh")
  (dired-vc-rename-file t))

(use-package org
  :custom
  ;; TODO: add a cleaner function that deletes files after some time limit
  (org-preview-latex-image-directory "~/.cache/emacs/ltximg/")
  :config
  (plist-put org-format-latex-options :scale 2.0)
  (advice-add 'org-latex-export-to-pdf
              :before #'org-export-to-pdf-cd)
  (add-to-list 'org-latex-default-packages-alist '("hidelinks" "hyperref" nil))
  :preface
  (defun org-export-to-pdf-cd (&optional _ _ _ _ _)
    "Change default directory to the canonicalized dirname of this buffer."
    ;; TODO: Fixes issue with export to pdf failing when it's not canonicalized.
    ;; Needs more investigation and might be a good idea to report upstream.
    (cd (file-name-directory (file-truename (buffer-file-name))))))

;;; init.el ends here
