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
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils))

(use-package-modules ncurses package-management xdisorg ssh)

(define guix-path
  (let ((local-guix (path-append (getenv "HOME")
                                 ".config/guix/current/bin/guix"))
        (global-guix "/run/current-system/profile/bin/guix"))
    (if (file-exists? local-guix)
        local-guix
        global-guix)))

(define wrapped-guix-script
  #~(begin
      (setenv "GUIX_PACKAGE_PATH" #$GUIX_CONFIG_MODULES_DIR)
      (apply execl #$guix-path (command-line))))


(define-public base-desktop-home-environment
  (home-environment
   (packages
    (list ncurses ;; fancy shell text
          xeyes   ;; check if programs are using wayland
          openssh ;; ssh
          ))
   (services
    (list (service home-dbus-service-type)
          (service home-bash-service-type
                   (home-bash-configuration
                    (environment-variables
                     `(("GUIX_PACKAGE_PATH"
                        . ,(build-path-augmentation
                            "GUIX_PACKAGE_PATH"
                            GUIX_CONFIG_MODULES_DIR))

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
                    (aliases
                     `((",guix-without-flatpak"
                        . "GUIX_FLATPAK_DISABLE=1 guix")
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
                          `((".local/bin/guix"
                             ,(program-file
                               "wrapped-guix-script"
                               wrapped-guix-script))
                            (".inputrc"
                             ,(plain-file
                               "inputrc"
                               "set revert-all-at-newline on\n"))))
          (service home-flatpak-service-type)
          (service home-impure-symlinks-service-type)))))
