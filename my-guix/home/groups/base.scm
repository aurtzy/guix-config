;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
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
;; This module defines groups for use with all systems; that is, services that I
;; deem required or useful everywhere.

(define-module (my-guix home groups base)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages tex)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services)
  #:use-module (my-guix packages git-annex-configure))

(define-group emacs
  (stow-service "emacs")
  (manifest-service (list emacs-next-pgtk
                          ;; meow modal editing
                          emacs-meow
                          ;; TODO remove this when I confirm it's not needed anymore
                          ;; needed by kaolin themes
                          ;; emacs-autothemer
                          ;; completion bundle
                          emacs-vertico
                          emacs-consult
                          emacs-marginalia
                          emacs-orderless
                          emacs-embark
                          ;; fill modes
                          emacs-visual-fill-column
                          emacs-adaptive-wrap
                          ;; git
                          emacs-magit
                          ;; orgmode latex
                          texlive-amsfonts
                          texlive-base
                          texlive-bin  ;; sets GUIX_TEXMF search path
                          texlive-capt-of
                          texlive-fonts-ec
                          texlive-hyperref
                          texlive-ulem
                          texlive-wrapfig))
  (simple-service home-environment-variables-service-type
                  '(;; Set editor for e.g. sudoedit
                    ("VISUAL" . "/usr/bin/env emacs")
                    ("EDITOR" . "/usr/bin/env emacs -nw"))))

(define-group flatpak
  (stow-service "flatpak")
  (flatpak-service 'flathub
                   (list "com.github.tchx84.Flatseal"))
  (simple-service home-activation-service-type
                  #~(unless #$(getenv "GUIX_DISABLE_FLATPAK")
                      (invoke "flatpak" "update" "--noninteractive")))
  (simple-service home-environment-variables-service-type
                  `(("XDG_DATA_DIRS" . ,(string-append
                                         "$XDG_DATA_DIRS"
                                         ":" $xdg-data-home "/flatpak/exports/share")))))

(define-group firefox
  (stow-service "firefox")
  (flatpak-service 'flathub
                   (list "org.mozilla.firefox")))

(define-group gnome-disks
  (manifest-service (list gnome-disk-utility)))

(define-group keepass
  (flatpak-service 'flathub
                   (list "org.keepassxc.KeePassXC")))

(define-group data-management
  (manifest-service (list git-annex
                          git-annex-configure
                          borg)))

;; Can't use define-group here since service procedure doesn't take a "service
;; name" argument
(define shell-group
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (environment-variables
              `(("GUIX_CONFIG_DIR" . ,$config-dir)
                ("GUIX_PACKAGE_PATH"
                 . ,(string-join $package-paths ":"))

                ;; Exclude certain commands from history
                ("HISTCONTROL" . "ignoreboth")
                ("HISTIGNORE" . "history*")
                
                ;; Explicitly set application data directory
                ("XDG_DATA_HOME" . ,$xdg-data-home)
                
                ;; Wayland-specific variables
                ("MOZ_ENABLE_WAYLAND" . "1")

                ;; Include more in PATH
                ("PATH" . "$HOME/.local/bin:$PATH")))
             (aliases `(("l." . "ls -d .*")
                        ("la" . "ls -a")
                        ("diff" . "diff --color=auto")))
             (bashrc
              ;; Import function definitions in bashrc file
              (list (local-file (files-ref "bash/bashrc")
                                "bashrc")))))))

(define-public base-groups
  (append emacs-group
          data-management-group
          flatpak-group
          firefox-group
          gnome-disks-group
          keepass-group
          shell-group))
