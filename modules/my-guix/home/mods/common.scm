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
  #:use-module (guix transformations)
  #:use-module (my-guix home mods misc)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils)
  #:export (build-data-mod
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
                     protobuf
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
    (name 'data)
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

(define emacs-mod
  (mod
    (name 'emacs)
    (description
     "Configures Emacs.")
    (dependencies
     (list tex-mod))
    (apply
     (compose (mod-he-packages
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
                     emacs-vundo
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
                     ;; data science stuff
                     emacs-jupyter
                     emacs-code-cells
                     emacs-csv-mode
                     ;; misc
                     emacs-eat
                     emacs-markdown-mode
                     emacs-protobuf-mode
                     ;; packages being tried out go below here
                     ;; emacs-dape                ;TODO package this?
                     emacs-org-noter
                     emacs-pdf-tools))
              (mod-he-services
               (list (simple-service name
                                     home-impure-symlinks-service-type
                                     `((".config/emacs"
                                        ,(path-append-my-files "emacs/impure")
                                        "init.el")))
                     (simple-service name
                                     home-environment-variables-service-type
                                     '( ;; Set editor for e.g. sudoedit
                                       ("VISUAL"
                                        . "/usr/bin/env emacs")
                                       ("EDITOR"
                                        . "/usr/bin/env emacs -nw")))))))))

(define common-fonts-mod
  (mod
    (name 'common-fonts)
    (description
     "Adds common fonts that provide support for other languages.")
    (apply
     (compose (mod-he-packages
               (list font-google-noto
                     font-wqy-zenhei))
              (mod-he-services
               (list (simple-service name
                                     home-impure-symlinks-service-type
                                     `((".local/share"
                                        ,(path-append-my-home
                                          ".guix-home/profile/share")
                                        "fonts")))))))))

(define flatpak-mod
  (mod
    (name 'flatpak)
    (description
     "Configures flatpak for the home environment.  This mod adds Flathub as a
remote.")
    (apply
     (compose-lambda (he)
       (list (mod-he-packages
              (list flatpak-xdg-utils
                    xdg-utils))
             (mod-he-services
              (list
               (simple-service name
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
             ;; TODO Use a simple-service for home-flatpak-service-type (or
             ;; some descendant supporting remote extensions) when it is
             ;; available
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
    (name 'audio)
    (description
     "Configures audio packages and services for this environment.  This mod
enables the use of Pipewire.")
    (dependencies
     (list flatpak-mod))
    (apply
     (compose
      (mod-he-packages
       (list pavucontrol))
      (mod-he-services
       (list (service home-pipewire-service-type)
             (simple-service name
                             home-impure-symlinks-service-type
                             `((".local/share/flatpak/overrides"
                                ,(path-append-my-files "easyeffects/impure")
                                "com.github.wwmm.easyeffects")
                               (".var/app/com.github.wwmm.easyeffects/config/easyeffects"
                                ,(path-append-my-files
                                  "easyeffects/impure/config"))))
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
    (name 'browsers)
    (description
     "This mod adds web browsers to the environment for browsing the
Internet.")
    (dependencies
     (list flatpak-mod))
    (apply
     (compose
      (mod-he-services
       (let ((firefox-profile
              ".var/app/org.mozilla.firefox/.mozilla/firefox/profile.default"))
         (list
          (simple-service name
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
                            (flathub "org.torproject.torbrowser-launcher")
                            (flathub "com.brave.Browser"))))))))))

(define password-management-mod
  (mod
    (name 'password-management)
    (description
     "Configures password management applications.")
    (dependencies
     (list flatpak-mod))
    (apply
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "org.keepassxc.KeePassXC")))))))))

;; TODO do I actually need this?
(define breeze-theme-mod
  (mod
    (name 'breeze-theme)
    (apply
     (mod-home-environment
       (packages
        (list breeze
              breeze-icons))))))

(define media-mod
  (mod
    (name 'media)
    (description
     "Configures applications for the consumption of media.")
    (apply
     (compose
      (mod-he-packages
       (list yt-dlp
             mpv
             quodlibet))
      (mod-he-services
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
