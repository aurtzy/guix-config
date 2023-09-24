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
;;; This module provides mods that will commonly be pulled in by base home
;;; environment definitions.

(define-module (my-guix home mods common)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix home mods misc)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
  #:export (emacs-base-mod
            emacs-org-mod
            emacs-mod
            flatpak-mod
            browsers-mod
            password-management-mod
            breeze-theme-mod
            media-mod

            common-mods))

(use-package-modules emacs emacs-xyz guile
                     freedesktop
                     kde-plasma kde-frameworks
                     video music)

(define emacs-base-mod
  (mod
    (name 'emacs-base-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
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
              emacs-visual-fill-column ;TODO DEPRECATED
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
              emacs-dashboard
              ;; markdown
              emacs-markdown-mode))
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((".config/emacs"
                                 ,(search-files-path
                                   "impure/emacs")
                                 "init.el")))
              (simple-service name
                              home-environment-variables-service-type
                              '( ;; Set editor for e.g. sudoedit
                                ("VISUAL"
                                 . "/usr/bin/env emacs")
                                ("EDITOR"
                                 . "/usr/bin/env emacs -nw")))))))))

(define emacs-org-mod
  (mod
    (name 'emacs-org-mod)
    (dependencies
     (list emacs-base-mod
           tex-mod))))

(define emacs-mod
  (mod
    (name 'emacs-mod)
    (dependencies
     (list emacs-base-mod
           emacs-org-mod))))

(define flatpak-mod
  (mod
    (name 'flatpak-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list flatpak-xdg-utils))
       (services
        home-environment-user-services
        services =>
        (cons (simple-service 'home-impure-symlinks-flatpak
                              home-impure-symlinks-service-type
                              (append
                               ;; Flatpak doesn't like dangling symlinks, so
                               ;; only make symlink when icons directory
                               ;; exists (i.e. when on Guix System)
                               (if (file-exists?
                                    "/run/current-system/profile/share")
                                   '((".local/share"
                                      "/run/current-system/profile/share"
                                      "icons"))
                                   '())
                               `( ;; GDK_PIXBUF_MODULE_FILE causes CSD issues
                                 ;; on foreign distros, so we unset it for
                                 ;; all flatpaks; allow access to system
                                 ;; icons
                                 (".local/share/flatpak/overrides"
                                  ,(search-files-path
                                    "impure/flatpak")
                                  "global")
                                 (".local/share/flatpak/overrides"
                                  ,(search-files-path
                                    "impure/flatpak")
                                  "com.github.tchx84.Flatseal"))))
              (modify-services services
                (home-flatpak-service-type
                 config =>
                 (home-flatpak-configuration
                  (remotes
                   (acons 'flathub
                          "https://flathub.org/repo/flathub.flatpakrepo"
                          (home-flatpak-configuration-remotes config)))
                  (profile
                   (cons '(flathub "com.github.tchx84.Flatseal")
                         (home-flatpak-configuration-profile
                          config))))))))))))

(define browsers-mod
  (mod
    (name 'browsers-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `(("Downloads/_tab-session-manager-backups"
                                 ,(string-append
                                   (getenv "HOME")
                                   "/areas/firefox/extension-backups"
                                   "/tab-session-manager-backups"))
                                (".var/app/org.mozilla.firefox/.mozilla/firefox/profile.default"
                                 ,(string-append
                                   (getenv "HOME")
                                   "/areas/firefox/profile")
                                 "bookmarkbackups"
                                 "favicons.sqlite"
                                 "search.json.mozlz4")
                                (".local/share/flatpak/overrides"
                                 ,(search-files-path
                                   "impure/brave")
                                 "com.brave.Browser")
                                (".local/share/flatpak/overrides"
                                 ,(search-files-path
                                   "impure/firefox")
                                 "org.mozilla.firefox")))
              (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "org.mozilla.firefox")
                                (flathub
                                 "com.github.micahflee.torbrowser-launcher")
                                (flathub "com.brave.Browser")))))))))

(define password-management-mod
  (mod
    (name 'password-management-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "org.keepassxc.KeePassXC")))))))))

;; TODO do I actually need this?
(define breeze-theme-mod
  (mod
    (name 'breeze-theme-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list breeze
              breeze-icons))))))

(define media-mod
  (mod
    (name 'media-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list yt-dlp
              mpv
              quodlibet))
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-bash-service-type
                              (home-bash-extension
                               (aliases
                                '(("mpv-without-cache"
                                   . "mpv --cache-secs=5")))))))))))

(define common-mods
  (list emacs-mod
        flatpak-mod
        browsers-mod
        password-management-mod
        breeze-theme-mod
        media-mod))
