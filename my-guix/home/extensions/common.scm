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
;; This module provides extensions that will commonly be pulled in by base
;; home environment definitions.

(define-module (my-guix home extensions common)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions misc)
  #:use-module (my-guix home services))

(use-package-modules emacs emacs-xyz
                     kde-plasma kde-frameworks
                     video music)

(define-public emacs-base-extension
  (extension
    (name "emacs-base")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* emacs-next-pgtk  ;wayland support
               ;; meow modal editing
               emacs-meow
               ;; completion bundle
               emacs-vertico
               emacs-consult
               emacs-marginalia
               emacs-orderless
               emacs-embark
               ;; fill column display tweaks
               emacs-visual-fill-column
               emacs-adaptive-wrap
               ;; git
               emacs-magit
               (home-environment-packages env)))
       (services
        (cons* (simple-service 'emacs-environment
                               home-environment-variables-service-type
                               '(;; Set editor for e.g. sudoedit
                                 ("VISUAL"
                                  . "/usr/bin/env emacs")
                                 ("EDITOR"
                                  . "/usr/bin/env emacs -nw")))
               (home-environment-user-services env)))))))

(define-public emacs-org-extension
  (extension
    (name "emacs-org")
    (dependencies
     (list emacs-base-extension
           tex-extension))))

(define-public emacs-extension
  (extension
    (name "emacs")
    (dependencies
     (list emacs-base-extension
           emacs-org-extension))))

(define-public browsers-extension
  (extension
    (name "browsers")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (stow-service 'stow-firefox "firefox")
               (stow-service 'stow-brave "brave")
               (flatpak-service 'flatpak-browsers
                                'flathub
                                '("org.mozilla.firefox"
                                  "com.github.micahflee.torbrowser-launcher"
                                  "com.brave.Browser"))

               (home-environment-user-services env)))))))

(define-public password-management-extension
  (extension
    (name "password-management")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (flatpak-service 'flatpak-keepassxc
                                'flathub
                                '("org.keepassxc.KeePassXC"))
               (home-environment-user-services env)))))))

;; TODO do I actually need this?
(define-public breeze-theme-extension
  (extension
    (name "breeze-theme")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* breeze
               breeze-icons
               (home-environment-packages env)))))))

(define-public media-players-extension
  (extension
    (name "media-players")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* mpv
               strawberry
               (home-environment-packages env)))))))

(define-public yt-dlp-extension
  (extension
    (name "yt-dlp")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* yt-dlp
               (home-environment-packages env)))))))

(define-public common-extension
  (extension
    (name "common")
    (dependencies
     (list emacs-extension
           browsers-extension
           password-management-extension
           ;; TODO enable this if it's needed; otherwise remove extension
           ;; breeze-theme-extension
           media-players-extension
           yt-dlp-extension))))
