;;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
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
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (my-guix home mods misc)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils)
  #:export (build-data-mod
            emacs-base-mod
            emacs-org-mod
            emacs-mod
            common-fonts-mod
            flatpak-mod
            audio-mod
            browsers-mod
            password-management-mod
            breeze-theme-mod
            media-mod

            common-mods))

(use-package-modules haskell-apps backup
                     emacs emacs-xyz tree-sitter guile
                     package-management
                     pulseaudio
                     fonts freedesktop
                     kde-plasma kde-frameworks
                     video music)

(define (build-assist-data-script data-dirs)
  (with-imported-modules
      '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((data-dirs '#$data-dirs)
               (orig-dir (getcwd))
               (with-chdir
                (lambda (dir proc)
                  (chdir
                   (if (string-prefix? "/" dir)
                       dir
                       (format #f "~a/~a"
                               (getenv "HOME")
                               dir)))
                  (proc)
                  (chdir orig-dir))))
          ;; Always flush buffers regardless of fails to minimize chance that
          ;; changes leave device in invalid state (e.g. via power failure)
          (with-exception-handler
              (lambda (exn) (sync) (exit #f))
            (lambda ()
              (for-each
               (lambda (data-dir)
                 (format #t "SYNCING: ~s\n"
                         data-dir)
                 (with-chdir
                  data-dir
                  (lambda ()
                    (invoke #$(file-append git-annex "/bin/git-annex")
                            "assist"))))
               data-dirs)
              (sync)))))))

(define (build-data-mod data-specs)
  "Build a mod that includes packages and services for my data setup.

DATA-SPECS is a list of symlink specifications.  A specification must have the
path to the data repository, followed by the names of its store items to
symlink from $HOME.

Note that paths should not have whitespaces to prevent issues with building
the shell alias."
  (mod
    (name 'data-mod)
    (apply
     (mod-home-environment
       (packages
        (list git-annex
              borg
              git-annex-configure))
       (services
        (list (simple-service name
                              home-impure-symlinks-service-type
                              (map
                               (lambda (symlinks-spec)
                                 (let* ((data-dir (car symlinks-spec))
                                        (item-names (cdr symlinks-spec))
                                        (store-dir (string-append data-dir
                                                                  "/store")))
                                   (cons* "" store-dir item-names)))
                               data-specs))
              (simple-service name
                              home-files-service-type
                              `((".local/bin/,annex-assist-all"
                                 ,(program-file
                                   "assist-data"
                                   (build-assist-data-script
                                    (map car data-specs))))))))))))

(define emacs-base-mod
  (mod
    (name 'emacs-base-mod)
    (apply
     (mod-home-environment
       (packages
        (list emacs-pgtk
              font-hack
              ;; tree-sitter
              tree-sitter-bash
              tree-sitter-c
              tree-sitter-go
              tree-sitter-gomod
              tree-sitter-javascript
              tree-sitter-python
              tree-sitter-rust
              ;; project-specific files
              emacs-envrc
              emacs-editorconfig
              ;; completion bundle
              emacs-vertico
              emacs-consult
              emacs-marginalia
              emacs-orderless
              emacs-embark
              ;; editing tweaks
              emacs-unfill
              emacs-adaptive-wrap
              ;; git
              emacs-magit
              emacs-magit-annex
              emacs-git-annex
              emacs-forge
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
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((".config/emacs"
                                 ,(path-append-my-files "impure/emacs")
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

(define common-fonts-mod
  (mod
    (name 'common-fonts-mod)
    (apply
     (mod-home-environment
       (packages
        (list font-google-noto
              font-wqy-zenhei))
       (services
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((".local/share"
                                 ,(path-append-my-home
                                   ".guix-home/profile/share")
                                 "fonts")))))))))

(define flatpak-mod
  (mod
    (name 'flatpak-mod)
    (apply
     (mod-home-environment
       (packages
        (list flatpak-xdg-utils
              xdg-utils))
       (services
        (list (simple-service name
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
                                  ,(path-append-my-files "impure/flatpak")
                                  "global")
                                 (".local/share/flatpak/overrides"
                                  ,(path-append-my-files "impure/flatpak")
                                  "com.github.tchx84.Flatseal"))))))
       (apply
        (lambda (he)
          (home-environment
           (inherit he)
           (services
            (modify-services (home-environment-user-services he)
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
                        config))))))))))))))

(define audio-mod
  (mod
    (name 'pipewire-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (mod-home-environment
       (packages
        (list pavucontrol))
       (services
        (list (service home-pipewire-service-type)
              (simple-service name
                              home-impure-symlinks-service-type
                              `((".config/easyeffects/input"
                                 ,(path-append-my-files "impure/pipewire")
                                 "main-mic.json")))
              (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "com.github.wwmm.easyeffects")))
              (simple-service name
                              home-shepherd-service-type
                              (list
                               (shepherd-service
                                (documentation
                                 "Run Easy Effects (easyeffects).")
                                (provision
                                 '(easyeffects))
                                (requirement
                                 '(pipewire))
                                (start
                                 #~(make-forkexec-constructor
                                    (list #$(file-append flatpak
                                                         "/bin/flatpak")
                                          "run"
                                          "--user"
                                          "com.github.wwmm.easyeffects"
                                          "--gapplication-service")))
                                (stop
                                 #~(make-kill-destructor)))))))))))

(define browsers-mod
  (mod
    (name 'browsers-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (mod-home-environment
       (services
        (let ((firefox-profile
               ".var/app/org.mozilla.firefox/.mozilla/firefox/profile.default"))
          (list (simple-service name
                                home-impure-symlinks-service-type
                                `((,firefox-profile
                                   ,(path-append-my-home
                                     "/areas/firefox/profile")
                                   "bookmarkbackups")
                                  (".local/share/flatpak/overrides"
                                   ,(path-append-my-files "impure/brave")
                                   "com.brave.Browser")
                                  (".local/share/flatpak/overrides"
                                   ,(path-append-my-files "impure/firefox")
                                   "org.mozilla.firefox")))
                (simple-service name
                                home-activation-service-type
                                ;; This file isn't /that/ important, and
                                ;; Firefox keeps overwriting it, so we just
                                ;; copy it without a backup instead
                                #~(copy-file #$(path-append-my-home
                                                "areas/firefox/profile"
                                                "search.json.mozlz4")
                                             #$(path-append-my-home
                                                firefox-profile
                                                "search.json.mozlz4")))
                (simple-service name
                                home-flatpak-profile-service-type
                                '((flathub "org.mozilla.firefox")
                                  (flathub
                                   "com.github.micahflee.torbrowser-launcher")
                                  (flathub "com.brave.Browser"))))))))))

(define password-management-mod
  (mod
    (name 'password-management-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (mod-home-environment
       (services
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "org.keepassxc.KeePassXC")))))))))

;; TODO do I actually need this?
(define breeze-theme-mod
  (mod
    (name 'breeze-theme-mod)
    (apply
     (mod-home-environment
       (packages
        (list breeze
              breeze-icons))))))

(define media-mod
  (mod
    (name 'media-mod)
    (apply
     (mod-home-environment
       (packages
        (list yt-dlp
              mpv
              quodlibet))
       (services
        (list (simple-service name
                              home-bash-service-type
                              (home-bash-extension
                               (aliases
                                '(("mpv-without-cache"
                                   . "mpv --cache-secs=5")))))))))))

(define common-mods
  (list emacs-mod
        common-fonts-mod
        flatpak-mod
        audio-mod
        browsers-mod
        password-management-mod
        breeze-theme-mod
        media-mod))
