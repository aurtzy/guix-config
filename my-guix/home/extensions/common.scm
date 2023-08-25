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
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions misc)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
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
               ;; editing tweaks
               emacs-unfill
               emacs-visual-fill-column ; TODO DEPRECATED
               emacs-adaptive-wrap
               ;; git
               emacs-magit
               ;; guix/guile/scheme hacking
               guile-3.0
               emacs-guix
               emacs-geiser
               emacs-geiser-guile
               emacs-paredit
               ;; dashboard on init
               emacs-dashboard)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-impure-symlinks-service-type
                               `((".config/emacs/init.el"
                                  ,(search-files-path
                                    "impure/emacs/init.el"))))
               (simple-service name
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
                               home-impure-symlinks-service-type
                               `(("Downloads/_tab-session-manager-backups"
                                  ,(string-append
                                    (getenv "HOME")
                                    "/areas/firefox/extension-backups"
                                    "/tab-session-manager-backups"))
                                 (".local/share/flatpak/overrides/com.brave.Browser"
                                  ,(search-files-path
                                    "impure/brave/com.brave.Browser"))
                                 (".local/share/flatpak/overrides/org.mozilla.firefox"
                                  ,(search-files-path
                                    "impure/firefox/org.mozilla.firefox"))))
               (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "org.mozilla.firefox")
                                 (flathub
                                  "com.github.micahflee.torbrowser-launcher")
                                 (flathub "com.brave.Browser"))))))))))

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
                               '((flathub "org.keepassxc.KeePassXC"))))))))))

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
               quodlibet)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-bash-service-type
                               (home-bash-extension
                                (aliases
                                 '(("mpv-without-cache"
                                    . "mpv --cache-secs=5"))))))))))))

(define common-extensions
  (list emacs-extension
        browsers-extension
        password-management-extension
        breeze-theme-extension
        media-extension))
