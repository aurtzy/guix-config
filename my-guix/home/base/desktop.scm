;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module provides a base desktop home environment.

(define-module (my-guix home base desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages ncurses)
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils))

(use-package-modules haskell-apps backup
                     package-management)

(define-public base-desktop-home-environment
  (home-environment
   (packages
    (list git-annex ;; data management packages
          git-annex-configure
          borg
          ;; fancy shell text
          ncurses))
   (services
    (list (service home-bash-service-type
                   (home-bash-configuration
                    (environment-variables
                     `(("GUIX_PACKAGE_PATH"
                        . ,(build-path-augmentation
                            "GUIX_PACKAGE_PATH"
                            $modules-dir))

                       ;; Exclude certain commands from history
                       ("HISTCONTROL" . "ignoreboth")
                       ("HISTIGNORE" . "history*")
                       
                       ;; Explicitly set application data directory
                       ("XDG_DATA_HOME" . ,$xdg-data-home)

                       ;; Add flatpak data directory
                       ("XDG_DATA_DIRS"
                        . ,(build-path-augmentation
                            "XDG_DATA_DIRS"
                            (string-append
                             $xdg-data-home "/flatpak/exports/share")
                            ;; This won't actually be used since we always do
                            ;; user installation, but it should make flatpak
                            ;; stop complaining
                            "/var/lib/flatpak/exports/share"))
                       ;; Include more in PATH
                       ("PATH"
                        . ,(build-path-augmentation
                            "PATH"
                            "$HOME/.local/bin"))))
                    ;; TODO some of these might be redundant and can be
                    ;; removed
                    (aliases `(("l." . "ls -d .*")
                               ("la" . "ls -a")
                               ("diff" . "diff --color=auto")))
                    (bashrc
                     ;; Import function definitions in bashrc file
                     (list (local-file
                            (search-files-path "bash/bashrc")
                            "bashrc")))))
           ;; Flatpak management
           (service home-flatpak-service-type
                    (home-flatpak-configuration
                     (remotes
                      '((flathub
                         . "https://flathub.org/repo/flathub.flatpakrepo")))
                     (profile '(("com.github.tchx84.Flatseal" . flathub)))))
           (simple-service 'stow-flatpak
                           home-stow-service-type
                           (list "flatpak"))))))
