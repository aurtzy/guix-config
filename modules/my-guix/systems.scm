;;; Copyright © 2023-2025 aurtzy <aurtzy@gmail.com>
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
;;; This module defines base system configuration records.

(define-module (my-guix systems)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu services base)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system nss)
  #:use-module (guix gexp)
  #:use-module (my-guix config)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix utils)
  #:export (base-desktop-operating-system
            base-desktop-home-environment
            base-foreign-desktop-home-environment))

(use-package-modules base ncurses nss package-management xdisorg ssh)

(define base-desktop-operating-system
  (operating-system
    (host-name "a-guix-system")
    (timezone "America/New_York")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us"))
    ;; Support '.local' host name lookups (mainly for printing)
    (name-service-switch %mdns-host-lookup-nss)
    (packages (list))
    (services (list))
    (sudoers-file
     (plain-file "sudoers"
                 (string-join
                  (list (plain-file-content %sudoers-specification)
                        "Defaults pwfeedback")
                  "\n"
                  'suffix)))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))
                 (timeout 3)
                 (keyboard-layout keyboard-layout)))
    (file-systems
     (cons* (file-system
              (mount-point "/media/usb-backup")
              (device (uuid "207d03a2-8838-4578-baba-dfb1af375da1"))
              (type "f2fs")
              (mount? #f))
            %base-file-systems))))

(define wrapped-guix-script
  #~(begin
      ;; Only set GUIX_PACKAGE_PATH when it is unset to allow for overrides
      (if (getenv "GUIX_PACKAGE_PATH")
          (display "override detected: GUIX_PACKAGE_PATH is already set\n")
          (setenv "GUIX_PACKAGE_PATH" #$GUIX_CONFIG_MODULES_DIR))
      (let ((local-guix #$(string-append (getenv "HOME")
                                         "/.config/guix/current/bin/guix"))
            (global-guix "/run/current-system/profile/bin/guix"))
        (apply execl
               (if (file-exists? local-guix)
                   local-guix
                   global-guix)
               (command-line)))))

(define-public base-desktop-home-environment
  (home-environment
   (packages
    (list ncurses ;; fancy shell text
          xeyes   ;; check if programs are using wayland
          openssh ;; ssh
          ))
   (services
    (cons* (service home-dbus-service-type)
           (service home-bash-service-type
                    (home-bash-configuration
                     (environment-variables
                      `( ;; Exclude certain commands from history
                        ("HISTCONTROL" . "ignoreboth")
                        ("HISTIGNORE" . "history:history *:exit:exit *")

                        ;; Explicitly set application data directory
                        ("XDG_DATA_HOME" . ,XDG_DATA_HOME)

                        ;; Add flatpak data directory
                        ("XDG_DATA_DIRS"
                         . ,(build-path-augmentation
                             "XDG_DATA_DIRS"
                             (string-append
                              XDG_DATA_HOME "/flatpak/exports/share")
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
           (service home-flatpak-service-type
                    (home-flatpak-configuration
                     (remotes '(("flathub"
                                 "https://flathub.org/repo/flathub.flatpakrepo")))
                     (global-overrides
                      ;; GDK_PIXBUF_MODULE_FILE causes CSD issues on foreign
                      ;; distros, so unset it for all flatpaks.
                      (flatpak-overrides-configuration
                       (filesystems
                        (list "/gnu/store:ro"
                              ;; Allow access to system icons.
                              "/run/current-system/profile/share/icons:ro"))
                       (unset-environment (list "GDK_PIXBUF_MODULE_FILE"))))))
           %base-home-services))))

(define base-foreign-desktop-home-environment
  (let ((env base-desktop-home-environment))
    (home-environment
     (inherit env)
     (packages
      (cons* nss-certs
             glibc-locales
             ;; Use host's ssh
             (delq openssh
                   (home-environment-packages env))))
     (services
      (cons*
       (simple-service 'base-foreign-desktop-home-environment-variables
                       home-environment-variables-service-type
                       '(("SSL_CERT_DIR"
                          . "$HOME/.guix-home/profile/etc/ssl/certs")
                         ("SSL_CERT_FILE"
                          . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
                         ;; TODO: figure out how this hack with XCURSOR_PATH
                         ;; works; apps can find adwaita cursors but not
                         ;; others (e.g. breeze_cursors)
                         ("XCURSOR_PATH"
                          . "/usr/share/icons")))
       (home-environment-user-services env))))))
