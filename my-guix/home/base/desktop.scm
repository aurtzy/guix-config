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
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home services)
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
          ;; TODO use of flatpak/stow on a first-time run will fail due to
          ;; paths not being updated until a restart or .bash_profile is
          ;; sourced. I should be able to fix this by referencing packages
          ;; with gexp magic in the service definitions, which should be
          ;; feasible to do.
          flatpak
          stow))
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
                             $xdg-data-home "/flatpak/exports/share")))
                       
                       ;; TODO is this necessary, still? if it is, might be
                       ;; better to put it elsewhere (perhaps even a dedicated
                       ;; wayland-extension which could be pulled in by
                       ;; wayland desktop env extensions)
                       ;;
                       ;; Wayland-specific variables
                       ;; ("MOZ_ENABLE_WAYLAND" . "1")

                       ;; Include more in PATH
                       ("PATH"
                        . ,(build-path-augmentation
                            "PATH"
                            "$HOME/.local/bin"))))
                    ;; TODO some of these might be redundant and can be
                    ;; removed
                    (aliases `(("grep" . "grep --color=auto")
                               ("ls" . "ls --color=auto")
                               ("l." . "ls -d .*")
                               ("la" . "ls -a")
                               ("diff" . "diff --color=auto")))
                    (bashrc
                     ;; Import function definitions in bashrc file
                     (list (local-file
                            (search-files-path "bash/bashrc")
                            "bashrc")))))
          ;; TODO not needed once flatpaks are completely managed by Guix
          ;;
          ;; Update all flatpaks, including those not managed by Guix
          (simple-service 'flatpak-update-all
                          home-activation-service-type
                          #~(unless #$(getenv "GUIX_DISABLE_FLATPAK")
                              (invoke "flatpak"
                                      "update"
                                      "--noninteractive")))
          ;; Flatpak management
          (flatpak-service 'flatpak-flatseal
                           'flathub
                           (list "com.github.tchx84.Flatseal"))
          (stow-service 'stow-flatseal "flatpak")))))
