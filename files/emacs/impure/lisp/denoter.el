;;; denoter.el --- Transient menus for denote.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvin Hsu
;; Copyright (C) 2018-2025 Free Software Foundation, Inc.

;; Author: Alvin Hsu <aurtzy@gmail.com>
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

;; TODO

;;; Code:

(require 'denote)
(require 'eieio)
(require 'thingatpt)
(require 'transient)

;;;; TEMP: Migration stuff.

(declare-function my-emacs-denote-assets-directory "init.el")

(defvar my-emacs-denote-aliases-file)

(require 'consult)
(require 'scoped)


;;;; Classes.


;;;;; Definitions.


;;;; Transient groups.


;;;;; Definitions.




;;;; Commands.


;;;;; Auxiliary.

(defun denoter-context-id (&optional prompt?)
  "Extract and return denote ID from context.

Return nil if nothing found in context.  If PROMPT?, allow
prompting for denote ID as a fallback."
  (cl-flet ((extract-id-or-nil (string)
              (if string (denote-extract-id-from-string string))))
    (or (transient-scope)
        (extract-id-or-nil (buffer-file-name))
        (extract-id-or-nil (word-at-point))
        (extract-id-or-nil (ignore-errors (dired-get-filename)))
        (extract-id-or-nil default-directory)
        (if prompt?
            (denote-retrieve-filename-identifier (denote-file-prompt))))))

(defun denoter-context-file ()
  "Get the current context's file name.  May return nil."
  (if-let* ((id (denoter-context-id)))
      (cond
       ((denote-get-path-by-id id))
       ;; File may not necessarily exist if it's not saved, yet.
       ((let ((current-file (buffer-file-name)))
          (and (equal id (denote-extract-id-from-string current-file))
               current-file))))))

(defun denoter-with-context-apply (fun &rest args)
    "Apply FUN to ARGS in the with buffer set to the contextual denote file."
    (if-let* ((file (denoter-context-file)))
        (with-current-buffer (find-file-noselect file)
          (apply fun args))
      (error "Denote context is invalid")))


;;;;; Transient infixes.

;;;;;; Auxiliary.

;;;;;; Definitions.

(transient-define-infix denoter-exclude-regexp-infix ()
  :class transient-option
  :argument "exclude-regexp="
  :reader (lambda (&rest _) (denote-sort-exclude-files-prompt))
  :description "Exclude regexp")

(transient-define-infix denoter-reverse-sort-infix ()
  :class transient-switch
  :argument "reverse-sort"
  :description "Reverse sort")

(transient-define-infix denoter-sort-by-component-infix ()
  "Set the sort component in transient state.

The sort component is one of `denote-sort-components', stored as a
string.  Consumers must intern back to symbols for use with denote
functions."
  :class transient-option
  :argument "sort-by="
  :reader (lambda (&rest _) (symbol-name (denote-sort-component-prompt)))
  :always-read t
  :description "Sort by")


;;;;; Transient suffixes.

;;;;;; Definitions.

(transient-define-suffix denoter-dired-all
  (sort-by-component reverse exclude-regexp)
  "Execute `denoter-dired' with FILES-MATCHING-REGEXP set to nil."
  (interactive
   (let ((args (transient-args 'denoter-dired-menu)))
     (list (intern (transient-arg-value "sort-by=" args))
           (transient-arg-value "reverse-sort" args)
           (transient-arg-value "exclude-regexp=" args))))
  (denoter-dired nil sort-by-component reverse exclude-regexp))

(transient-define-suffix denoter-dired
  (files-matching-regexp sort-by-component reverse exclude-regexp)
  "Like `denote-sort-dired', but interactive calls use transient state."
  (interactive
   (let ((args (transient-args 'denoter-dired-menu)))
     (list (denote-files-matching-regexp-prompt)
           (intern (transient-arg-value "sort-by=" args))
           (transient-arg-value "reverse-sort" args)
           (transient-arg-value "exclude-regexp=" args))))
  (denote-sort-dired
   files-matching-regexp sort-by-component reverse exclude-regexp))

(transient-define-suffix denoter-dired-assets-directory (file)
  "Open `dired' in assets directory associated with denote FILE."
  (interactive (list (denote-file-prompt)))
  (dired (my-emacs-denote-assets-directory file t)))

(transient-define-suffix denoter-find-context-assets-directory ()
  "Run Dired in the assets directory from current denote context."
  (interactive)
  (dired (my-emacs-denote-assets-directory
          (denote-get-path-by-id (denoter-context-id t)) t)))

(transient-define-suffix denoter-find-aliases-file ()
  "Find `denoter-aliases-file' from variable `denote-directory'."
  (interactive)
  (find-file (file-name-concat denote-directory my-emacs-denote-aliases-file)))

(transient-define-suffix denoter-find-context-file ()
  "Open the contextual denote file from transient state."
  (interactive)
  (find-file (denote-get-path-by-id (denoter-context-id t))))

(transient-define-suffix denoter-find-regexp ()
  "Find regexp in variable `denote-directory' notes."
  (interactive)
  (let ((default-directory denote-directory))
    (consult-ripgrep (denote-directory-files))))

(transient-define-suffix denoter-rename-to-location (dir)
  "Move context's denote files (including assets) to another location."
  (interactive (list (denote-subdirectory-prompt)))
  (if-let* ((file (denoter-context-file)))
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

(transient-define-suffix denoter-switch-context-id ()
  "Switch context to another denote file."
  :transient t
  (interactive)
  ;; Ignore the current buffer so we can select the current denote file.
  (with-temp-buffer
    (oset (transient-prefix-object) scope
          (denote-retrieve-filename-identifier (denote-file-prompt)))))


;;;;; Transient prefixes.

;;;;;; Auxiliary.



;;;;;; Definitions.

;;;###autoload
(transient-define-prefix denoter-dired-menu ()
  [:description
   (lambda ()
     (concat (propertize "Denote directory: " 'face 'transient-heading)
             (propertize denote-directory 'face 'transient-value)))
   ""]
  ["Options"
   (",e" denoter-exclude-regexp-infix)
   (",s" denoter-sort-by-component-infix)
   (",r" denoter-reverse-sort-infix)]
  ["Commands"
   ("a" "Dired (all)" denoter-dired-all)
   ("d" "Dired (filtered)" denoter-dired)]
  (interactive)
  (transient-setup
   'denoter-dired-menu nil nil
   ;; Use user-customizable `denote' options to set defaults for menu.
   :value `(,(concat
              "sort-by=" (symbol-name denote-sort-dired-default-sort-component))
            ,@(if denote-sort-dired-default-reverse-sort '("reverse-sort")))))

;; TODO: Add commands from:
;; <https://protesilaos.com/emacs/denote#h:998ae528-9276-47ec-b642-3d7355a38f27>
;; TODO: Support denote silos.
;; TODO: `denote-directory' can be a list of directories.  We should handle
;; this case.
;;;###autoload
(transient-define-prefix denoter-menu ()
  :refresh-suffixes t
  ;; TODO: We could have a higher-level heading above denote context for
  ;; `denote-directory'.  This is where we could put "global" commands and
  ;; silo management commands.
  [:description
   (lambda ()
     (concat (propertize "Denote context: " 'face 'transient-heading)
             (if-let* ((file (denoter-context-file)))
                 (propertize (file-name-nondirectory file)
                             'face 'transient-value)
               (propertize "None detected" 'face 'transient-inapt-suffix))))
   ("n" "Switch denote context" denoter-switch-context-id)
   ("N" "New notes file" denote-subdirectory)]
  ["Contextual commands"
   :inapt-if-not denoter-context-id
   :pad-keys t
   [ :advice denoter-with-context-apply
     ;; TODO: Add command: Create static assets directory.
     ;; TODO: Display "create assets directory" if it doesn't exist.
     ("a" "Open assets directory" denoter-find-context-assets-directory)
     ("f" "Open file" denoter-find-context-file)]
   ["Rename"
    :advice denoter-with-context-apply
    ("r k" "keywords" denote-rename-file-keywords)
    ("r m" "to another location" denoter-rename-to-location)
    ("r t" "title" denote-rename-file-title)
    ("r r" "file" denote-rename-file)]]
  ["Find"
   ("A" "aliases file" denoter-find-aliases-file)
   ("d" "denote-sort-dired" denoter-dired-menu)
   ("R" "regexp in notes" denoter-find-regexp)]
  (interactive)
  (transient-setup
   'denoter-menu nil nil
   :scope (denoter-context-id)))

(provide 'denoter)
;;; denoter.el ends here
