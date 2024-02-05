;;; init.el --- my init.el -*- lexical-binding: t -*-
;; Copyright © 2013-2022 Phil Hagelberg and contributors
;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
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
;; REFERENCES:
;; Phil Hagelberg's better-defaults configuration
;; David Wilson's (davwil, System Crafters) videos

;; TODO: would like comment highlighting functionality; e.g. TODO, FIXME,
;; etc. being highlighted assigned colors.

;;; Code:

;;; Global configurations

;;;; User-defined variables

(defconst state-dir
  (concat (or (getenv "XDG_STATE_HOME") "~/.local/state") "/emacs/"))

;;;; User info

(setq user-full-name "aurtzy"
      user-mail-address "aurtzy@gmail.com")

;;;; Emacs-managed files

(use-package emacs
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  :config
  (load custom-file 'noerror 'nomessage))

(use-package emacs
  :custom
  (load-prefer-newer t))

(use-package emacs
  :custom
  (backup-directory-alist `(("." . ,state-dir)))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 4))

(use-package emacs
  :config
  (savehist-mode 1)
  (save-place-mode 1)
  (recentf-mode 1))

;;;; Emacs management files

(use-package emacs
  :custom
  (enable-remote-dir-locals t))

;;;; Visual interface

(use-package emacs
  :config
  (load-theme 'modus-vivendi))

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
  :custom
  (inhibit-startup-message t)
  (visible-bell t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (use-dialog-box nil)
  :config
  (add-to-list 'default-frame-alist '(font . "Hack-11"))
  ;; TODO menu-bar was funky with frame transparency at the time of
  ;; writing so menu-bar has been disabled; try re-enabling in future
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (global-visual-line-mode 1)
  (global-hl-line-mode 1))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward))

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
  :config
  (add-hook 'shell-mode-hook
            'remove-scroll-margin)
  (add-hook 'eshell-mode-hook
            'remove-scroll-margin)
  ;; Configure scrollbar/mouse scrolling
  ;;
  ;; TODO check out mwheel.el and experiment with
  ;; the nano/vim-like vscrolling that I like
  :custom
  (scroll-preserve-screen-position t)
  :config
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

;;;;; DEFAULT FILE FORMATTING

(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (require-final-newline t))

;;;;; DEFAULT TO REGEXP ISEARCH

(use-package emacs
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

;;;;; REMAP `kill-sexp'

(use-package emacs
  :preface
  (defun into-list (&optional arg interactive)
    "(into-list &optional ARG INTERACTIVE)

Move forward into one level of parentheses.
This command is identical to \"down-list\" as a
simple rename to fit the keybind it will be mapped to."
    (interactive "^p\nd")
    (down-list arg interactive))
  :bind (("C-M-d" . kill-sexp)
         ("C-M-i" . into-list)
         ("C-<tab>" . completion-at-point)
         :map emacs-lisp-mode-map
         ("C-M-i" . nil)))

;;;;; REMAP UP/DOWN CASE COMMANDS

(use-package emacs
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)))

;;;;; ENABLE ELECTRIC PARENS

(use-package emacs
  :config
  (electric-pair-mode 1))

;;;;; EDITING TWEAKS

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

(use-package emacs
  :init
  (setq-default fill-column 80)
  :config
  (global-display-fill-column-indicator-mode 1))

(use-package unfill)

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :custom
  (adaptive-wrap-extra-indent 1))

;; TODO this is cool
;; See: https://www.vernon-grant.com/Emacs/Discovering-Emacs/4-using-whitespace-mode.html
(use-package whitespace
  :custom
  (whitespace-style '(face
                      spaces
                      empty
                      tabs
                      trailing
                      space-mark
                      tab-mark))
  (whitespace-global-modes '(not shell-mode
                                 help-mode
                                 magit-mode
                                 magit-diff-mode
                                 ibuffer-mode
                                 dired-mode))
  :config
  ;; (global-whitespace-mode 1)
  )

;;;; Completion suite

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
         ("C-c m" . consult-man)
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
         ("C-x p b" . consult-project-buffer)
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

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  ;;:init
  ;; TODO new feature that is part of default config not out yet
  ;;
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Minor modes

(use-package emacs
  :custom
  (auto-insert-directory (concat user-emacs-directory "inserts"))
  :config
  (auto-insert-mode 1))

(use-package envrc
  :preface
  (declare-function envrc-global-mode "envrc")
  :config
  (envrc-global-mode t))

(use-package eglot
  :commands eglot)

(use-package flymake
  :hook prog-mode
  :custom
  (flymake-number-of-errors-to-display 4))

;;;; Miscellaneous

(use-package emacs
  :bind (("C-z" . nil)))

;; TEMP: properly set project root based on files found
;;
;; project.el does not have such a feature for manually selecting at
;; the moment; this will suffice for now. Comes from:
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
;;
;; Extra links for more info:
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41955#26
(use-package project
  :preface
  (defcustom project-root-markers
    '(".project-root"
      "manifest.scm"
      "main.py")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))
  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  :config
  (add-to-list 'project-find-functions #'project-find-root))

;;; Major modes

(use-package dashboard
  :preface
  (declare-function dashboard-setup-startup-hook "dashboard")
  :custom
  (dashboard-set-init-info nil)
  :config
  (dashboard-setup-startup-hook))

(use-package python
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (set-fill-column 79)))
  :custom
  (python-shell-dedicated 'project))

;; https://reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package ccls
  :after eglot
  :when (package-installed-p 'ccls)
  :defines ccls-sem-highlight-method
  :custom
  (ccls-initialization-options '(:index
                                 (:comments 2)
                                 :completion
                                 (:detailedLabel t)))
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  ;; TODO remove this comment when the above works; this does not work yet due
  ;; to unavailable feature; see: https://github.com/joaotavora/eglot/issues/615
  ;; depending on its implementation the above may need to be tweaked
  )

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package scheme
  :init
  (font-lock-add-keywords 'scheme-mode
                          '(("(\\(lambda\\*\\)"
                             (1 font-lock-keyword-face))))
  (put 'lambda* 'scheme-indent-function 1))

(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/git/guix"))

;; TODO: explore magit configurations
(use-package magit
  :commands magit)

(use-package org
  :preface
  (defun org-export-to-pdf-cd (&optional _ _ _ _ _)
    "Change default directory to the canonicalized dirname of this buffer.

Fixes issue with export to pdf failing when it's not
canonicalized.  Needs more investigation and might be a good idea
to report upstream.  TODO."
    (cd (file-name-directory (file-truename (buffer-file-name)))))
  :commands org-mode
  :bind (("C-c C-x <backtab>" . org-clock-out)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "PROG(p!)" "|" "DONE(d!)" "SKIP(s@/!)")))
  (org-cycle-inline-images-display t)
  (org-export-in-background t)
  ;; TODO add a cleaner function that deletes files after some time limit
  (org-preview-latex-image-directory "~/.cache/emacs/ltximg")
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-into-drawer t)
  :init
  (let ((export-dir "~/Documents/org"))
    (make-directory export-dir t)
    (define-auto-insert
      '(org-mode . "Org file")
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

(use-package org-agenda
  :after org
  :custom
  (org-agenda-span 10))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
