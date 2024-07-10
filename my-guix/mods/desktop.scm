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
;;; This module defines mods for desktop systems.

(define-module (my-guix mods desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix utils)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:use-module (srfi srfi-1)
  #:export (replace-mesa
            <swapfile-configuration>
            swapfile-configuration
            swapfile-configuration?
            swapfile-configuration-file
            swapfile-configuration-device
            swapfile-configuration-offset
            swapfile

            audio-mod
            bottles-mod
            breeze-theme-mod
            browsers-mod
            common-fonts-mod
            desktop-services-mod
            emacs-mod
            esync-mod
            file-system-management-mod
            flatpak-mod
            media-mod
            password-management-mod
            printers-mod
            swapfile-mod
            tor-mod
            virtualization-mod

            desktop-mods))

(use-package-modules avahi backup compression cryptsetup disk emacs emacs-xyz
                     fonts freedesktop gl gnome gnome-xyz guile haskell-apps
                     kde-frameworks kde-plasma linux music package-management
                     protobuf pulseaudio qt tex tor tree-sitter video
                     virtualization)

(use-service-modules cups desktop networking pm virtualization xorg)

(define-record-type* <swapfile-configuration>
  swapfile-configuration make-swapfile-configuration
  swapfile-configuration?
  ;; Path to swapfile.
  (file swapfile-configuration-file)
  ;; Device that swapfile is present on.
  (device swapfile-configuration-device)
  ;; Offset of swapfile.
  (offset swapfile-configuration-offset))

;; swapfile: Parameter representing swapfile configuration of the system.  By
;; default it is false; otherwise, this parameter must be of type
;; <swapfile-configuration>.
(define swapfile
  (make-parameter #f
                  (lambda (val)
                    (rnrs:assert (or (not val) (swapfile-configuration? val)))
                    val)))

;; replace-mesa: Parameter storing a procedure that consumes a package and
;; replaces its mesa inputs with another input.  By default, it is the
;; identity function (i.e. returns the same package).
;;
;; This parameter can be directly set to the replacement procedure, but can
;; also accept a package, in which case the converter will turn it into a
;; procedure that grafts mesa with that package.
(define replace-mesa
  (make-parameter identity
                  (lambda (val)
                    (cond
                     ((procedure? val)
                      val)
                     ((package? val)
                      (package-input-rewriting `((,mesa . ,val))))
                     (else
                      (raise-exception
                       (make-exception
                        (make-exception-with-message
                         "Not a procedure or package")
                        (make-exception-with-irritants
                         (list val)))))))))

(define audio-mod
  (mod
    (name 'audio)
    (description
     "Configures audio packages and services for this environment.  This mod
enables the use of Pipewire.")
    (he-extension
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

(define bottles-mod
  (mod
    (name 'bottles)
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "com.usebottles.bottles")))
             (simple-service name
                             home-impure-symlinks-service-type
                             `((".local/share/flatpak/overrides"
                                ,(path-append-my-files "bottles/impure")
                                "com.usebottles.bottles")))))))))

;; TODO: do I actually need this?
(define breeze-theme-mod
  (mod
    (name 'breeze-theme)
    (he-extension
     (compose (mod-he-packages
               (list breeze
                     breeze-icons))))))

(define browsers-mod
  (mod
    (name 'browsers)
    (description
     "This mod adds web browsers to the environment for browsing the
Internet.")
    (he-extension
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
                             ,(path-append-my-files "brave/impure")
                             "com.brave.Browser")
                            (".local/share/flatpak/overrides"
                             ,(path-append-my-files "firefox/impure")
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

(define common-fonts-mod
  (mod
    (name 'common-fonts)
    (description
     "Adds common fonts that provide support for other languages.")
    (he-extension
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

(define desktop-services-mod
  (mod
    (name 'desktop-services)
    (description
     "Configures desktop services defined by Guix.

Some services are explicitly removed for modularity purposes (i.e. to be added
elsewhere in possibly different forms).")
    (os-extension
     (let ((replace-mesa (replace-mesa)))
       (compose
        (mod-os-packages
         (list (replace-mesa network-manager-applet)))
        (mod-os-services
         (delete 'network-manager-applet
                 (modify-services %desktop-services
                   (delete gdm-service-type))
                 (lambda (name serv)
                   (eq? name (service-type-name (service-kind serv)))))))))))

(define emacs-mod
  (mod
    (name 'emacs)
    (description
     "Configures Emacs.")
    (he-extension
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
                     ;; latex with org-mode
                     texlive-beamer
                     texlive-capt-of
                     texlive-libertine
                     texlive-scheme-basic
                     texlive-ulem
                     texlive-wrapfig
                     ;; project-specific files
                     emacs-envrc
                     emacs-editorconfig
                     ;; completion bundle
                     emacs-vertico
                     emacs-consult
                     emacs-marginalia
                     emacs-orderless
                     emacs-embark
                     emacs-corfu
                     ;; editing tweaks
                     emacs-adaptive-wrap
                     emacs-avy
                     emacs-unfill
                     emacs-vundo
                     ;; git
                     emacs-magit
                     emacs-magit-todos ; XXX: Strangely only works with ripgrep?
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
                     emacs-hl-todo
                     emacs-markdown-mode
                     emacs-protobuf-mode
                     emacs-ripgrep
                     emacs-wgrep
                     ;; packages being tried out go below here
                     ;; emacs-dape                ;TODO: package this?
                     emacs-org-noter
                     emacs-pdf-tools
                     ;; Enable viewing .zip files
                     unzip))
              (mod-he-services
               (list
                (simple-service name
                                home-impure-symlinks-service-type
                                `((".config/emacs"
                                   ,(path-append-my-files "emacs/impure")
                                   "init.el"
                                   "lisp")))
                (simple-service name
                                home-environment-variables-service-type
                                '( ;; Set editor for e.g. sudoedit
                                  ("VISUAL"
                                   .
                                   "/usr/bin/env emacsclient -a emacs --no-wait")
                                  ("EDITOR"
                                   .
                                   "/usr/bin/env emacs -nw")
                                  ("SUDO_EDITOR"
                                   .
                                   "/usr/bin/env emacs")))))))))

(define esync-mod
  (mod
    (name 'esync)
    (description
     "Makes the system Esync-compatible.")
    (os-extension
     (compose (mod-os-services
               (list
                (service pam-limits-service-type
                         (list
                          (pam-limits-entry "*" 'hard 'nofile 524288)))))))))

(define flatpak-mod
  (mod
    (name 'flatpak)
    (description
     "Configures flatpak for the home environment.  This mod adds Flathub as a
remote.")
    (he-extension
     (compose
      (mod-he-packages
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
                            ,(path-append-my-files "flatpak/impure")
                            "global")
                           (".local/share/flatpak/overrides"
                            ,(path-append-my-files "flatpak/impure")
                            "com.github.tchx84.Flatseal"))))))
      ;; TODO: Use a simple-service for home-flatpak-service-type (or
      ;; some descendant supporting remote extensions) when it is
      ;; available
      (mod-he-service
       home-flatpak-service-type
       (lambda (config)
         (home-flatpak-configuration
          (inherit config)
          (remotes
           (acons 'flathub
                  "https://flathub.org/repo/flathub.flatpakrepo"
                  (home-flatpak-configuration-remotes config)))
          (profile
           (cons '(flathub "com.github.tchx84.Flatseal")
                 (home-flatpak-configuration-profile
                  config))))))))))

(define file-system-management-mod
  (mod
    (name 'file-system-management)
    (description
     "Provides software to support various file system operations and disk
management/maintenance.")
    (os-extension
     (compose (mod-os-packages
               (list btrfs-progs
                     cryptsetup
                     gparted
                     gptfdisk
                     lvm2
                     ntfs-3g))))))

(define media-mod
  (mod
    (name 'media)
    (description
     "Configures applications for the consumption of media.")
    (he-extension
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
                                  . "mpv --cache-secs=5")))))
             (simple-service name
                             home-impure-symlinks-service-type
                             `((".config/mpv"
                                ,(path-append-my-files "mpv/impure/config"))))))))))

(define password-management-mod
  (mod
    (name 'password-management)
    (description
     "Configures password management applications.")
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "org.keepassxc.KeePassXC")))))))))

(define printers-mod
  (mod
    (name 'printers)
    (description
     "Provides printing support via CUPS.")
    (os-extension
     (compose (mod-os-packages
               (list nss-mdns))
              (mod-os-services
               (list (service cups-service-type)))))))

(define swapfile-mod
  (mod
    (name 'swapfile)
    (description
     "Configures swapfile for system defined by the SWAPFILE parameter.  See
Guix documentation on swapfiles for more information.  If the setup script in
this repository is used to set up the swapfile, it should output all the
swapfile configuration information needed.")
    (os-extension
     (compose-lambda (os)
       (define config (swapfile))

       (rnrs:assert (swapfile-configuration? config))
       (let ((file (swapfile-configuration-file config))
             (device (swapfile-configuration-device config))
             (offset (swapfile-configuration-offset config)))
         (list
          (mod-os-swap-devices
           (list (swap-space (target file)
                             (dependencies
                              (filter
                               (file-system-mount-point-predicate "/")
                               (operating-system-file-systems os))))))
          (mod-os-kernel-arguments
           (list (string-append "resume=" device)
                 (string-append "resume_offset=" offset)))))))))

(define tor-mod
  (mod
    (name 'tor)
    (description
     "Configures tor.")
    (os-extension
     (compose (mod-os-packages
               (list torsocks))
              (mod-os-services
               (list (service tor-service-type)))))))

(define virtualization-mod
  (mod
    (name 'virtualization)
    (description
     "Adds virtualization packages and services to the system environment.")
    (os-extension
     (compose (mod-os-packages
               (list virt-manager
                     gnome-boxes))
              (mod-os-services
               (list (service libvirt-service-type
                              (libvirt-configuration
                               (unix-sock-group "libvirt")))
                     (service virtlog-service-type)
                     (service qemu-binfmt-service-type
                              (qemu-binfmt-configuration
                               (platforms (lookup-qemu-platforms
                                           "arm"
                                           "aarch64"))))))))))

(define desktop-mods
  (append base-mods
          (list audio-mod
                bottles-mod
                breeze-theme-mod
                browsers-mod
                common-fonts-mod
                desktop-services-mod
                emacs-mod
                esync-mod
                file-system-management-mod
                flatpak-mod
                media-mod
                password-management-mod
                printers-mod
                swapfile-mod
                tor-mod
                virtualization-mod)))
