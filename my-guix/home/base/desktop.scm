;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
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
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils))

(use-package-modules haskell-apps backup
                     package-management xdisorg)

(define-public base-desktop-home-environment
  (home-environment
   (packages
    (list git-annex ;; data management packages
          git-annex-configure
          borg
          ;; fancy shell text
          ncurses
          ;; for checking if programs are using wayland
          xeyes))
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
                            ;; user installation, but it make should make
                            ;; flatpak stop complaining
                            "/var/lib/flatpak/exports/share"))
                       ;; Include more in PATH
                       ("PATH"
                        . ,(build-path-augmentation
                            "PATH"
                            "$HOME/.local/bin"))))
                    (aliases `(("l." . "ls -d .*")
                               ("la" . "ls -a")
                               ("diff" . "diff --color=auto")

                               ("guix-without-flatpak"
                                . "GUIX_FLATPAK_DISABLE=1 guix")))
                    (bashrc
                     ;; Import function definitions in bashrc file
                     (list (local-file
                            (search-files-path "bash/bashrc")
                            "bashrc")))))
          (service home-impure-symlinks-service-type)
          ;; Flatpak management
          (service home-flatpak-service-type
                   (home-flatpak-configuration
                    (remotes
                     '((flathub
                        . "https://flathub.org/repo/flathub.flatpakrepo")))
                    (profile '((flathub "com.github.tchx84.Flatseal")))))
          (simple-service 'home-impure-symlinks-flatpak
                          home-impure-symlinks-service-type
                          `((".local/share/flatpak/overrides/global"
                             ,(search-files-path
                               "impure/flatpak/global"))
                            (".local/share/flatpak/overrides/com.github.tchx84.Flatseal"
                             ,(search-files-path
                               "impure/flatpak/com.github.tchx84.Flatseal"))))))))
