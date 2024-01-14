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
;;; This module provides a base desktop home environment.

(define-module (my-guix home base desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils))

(use-package-modules ncurses package-management xdisorg ssh)

(define-public base-desktop-home-environment
  (home-environment
   (packages
    (list ncurses ;; fancy shell text
          xeyes   ;; check if programs are using wayland
          openssh ;; ssh
          ))
   (services
    (list (service home-bash-service-type
                   (home-bash-configuration
                    (environment-variables
                     `(("GUIX_PACKAGE_PATH"
                        . ,(build-path-augmentation
                            "GUIX_PACKAGE_PATH"
                            $my-modules-dir))

                       ;; Exclude certain commands from history
                       ("HISTCONTROL" . "ignoreboth")
                       ("HISTIGNORE" . "history:history *:exit:exit *")

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
                    (aliases `((",home-reconfigure"
                                . ,(format
                                    #f
                                    "guix home reconfigure ~a/~a"
                                    $my-guix-config
                                    "home.scm"))
                               (",home-reconfigure-without-flatpak"
                                . "GUIX_FLATPAK_DISABLE=1 ,home-reconfigure")
                               (",system-reconfigure"
                                . ,(format
                                    #f
                                    "sudo guix system -L ~a reconfigure ~a/~a"
                                    $my-modules-dir
                                    $my-guix-config
                                    "system.scm"))

                               ("l." . "ls -d .*")
                               ("la" . "ls -a")
                               ("diff" . "diff --color=auto")))
                    (bashrc
                     ;; Import function definitions in bashrc file
                     (list (local-file
                            (path-append-my-files "bash/bashrc")
                            "bashrc")))))
          (simple-service 'home-files
                          home-files-service-type
                          `((".inputrc"
                             ,(plain-file
                               "inputrc"
                               "set revert-all-at-newline on\n"))))
          (service home-flatpak-service-type)
          (service home-impure-symlinks-service-type)))))
