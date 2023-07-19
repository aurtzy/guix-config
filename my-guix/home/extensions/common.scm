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
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions misc)
  #:use-module (my-guix home services package-management)
  #:export (emacs-base-extension
            emacs-org-extension
            emacs-extension
            browsers-extension
            password-management-extension
            breeze-theme-extension
            media-extension

            common-extensions))

(use-package-modules emacs emacs-xyz guile
                     kde-plasma kde-frameworks
                     video music)

(define emacs-base-extension
  (extension
    (name 'emacs-base-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list emacs-next-pgtk          ;emacs with wayland support
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
               ;; guix/guile/scheme hacking
               guile-3.0
               emacs-guix
               emacs-geiser
               emacs-geiser-guile
               emacs-paredit)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-environment-variables-service-type
                               '( ;; Set editor for e.g. sudoedit
                                 ("VISUAL"
                                  . "/usr/bin/env emacs")
                                 ("EDITOR"
                                  . "/usr/bin/env emacs -nw"))))))))))

(define emacs-org-extension
  (extension
    (name 'emacs-org-extension)
    (dependencies
     (list emacs-base-extension
           tex-extension))))

(define emacs-extension
  (extension
    (name 'emacs-extension)
    (dependencies
     (list emacs-base-extension
           emacs-org-extension))))

(define browsers-extension
  (extension
    (name 'browsers-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-stow-service-type
                               (list "firefox"
                                     "brave"))
               (simple-service name
                               home-flatpak-profile-service-type
                               '(("org.mozilla.firefox" . flathub)
                                 ("com.github.micahflee.torbrowser-launcher"
                                  . flathub)
                                 ("com.brave.Browser" . flathub))))))))))

(define password-management-extension
  (extension
    (name 'password-management-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-flatpak-profile-service-type
                               '(("org.keepassxc.KeePassXC"
                                  . flathub))))))))))

;; TODO do I actually need this?
(define breeze-theme-extension
  (extension
    (name 'breeze-theme-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list breeze
               breeze-icons)))))))

(define media-extension
  (extension
    (name 'media-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list yt-dlp
               mpv
               strawberry)))))))

(define common-extensions
  (list emacs-extension
        browsers-extension
        password-management-extension
        breeze-theme-extension
        media-extension))
