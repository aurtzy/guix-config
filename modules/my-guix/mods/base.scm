;;; Copyright © 2023-2025 Alvin Hsu <aurtzy@gmail.com>
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
;;; This module defines mods commonly used on all systems.

(define-module (my-guix mods base)
  #:use-module (gnu)
  #:use-module (gnu services configuration)
  #:use-module (guix channels)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix mods)
  #:use-module (my-guix utils)
  #:use-module (nonguix utils)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (base-packages-mod
            nonguix-channel-mod
            meta-base-mod

            replace-mesa-argument
            desktop-services-mod
            file-system-management-mod
            performance-mod
            printers-mod
            swapfile-configuration
            swapfile-configuration?
            swapfile-configuration-file
            swapfile-configuration-device
            swapfile-configuration-offset
            swapfile-argument
            swapfile-mod
            tor-mod
            virtualization-mod
            meta-desktop-mod))

(use-package-modules aspell audio avahi backup compression cryptsetup disk
                     emacs emacs-build emacs-xyz fonts freedesktop gl gnome
                     gnome-xyz guile haskell-apps kde-frameworks kde-plasma
                     linux music package-management protobuf pulseaudio qt tex
                     tor tree-sitter version-control video virtualization)

(use-service-modules cups desktop linux networking pm virtualization xorg)


;;; Base mods.

(define base-packages-mod
  (operating-system-mod
    (name 'base-packages)
    (description
     "Provides the base set of packages defined by Guix as well as additional
packages deemed essential.")
    (packages (cons* git git-annex %base-packages))))

(define guix-channels-mod
  (operating-system-mod
    (name 'guix-channels)
    (description
     "Sets up the Nonguix channel on this system.  The channel must still be
enabled in the home environment.")
    (services
     (list (service guix-service-type
                    (guix-configuration
                      (channels
                       (cons*
                        (channel
                          (name 'nonguix)
                          (url "https://gitlab.com/nonguix/nonguix")
                          (branch "master")
                          (introduction
                           (make-channel-introduction
                            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                            (openpgp-fingerprint
                             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                        %default-channels))))
           (simple-service name
                           guix-service-type
                           (guix-extension
                             (authorized-keys
                              (list (local-file (path-append-my-files
                                                 "guix/nonguix.pub"))))
                             (substitute-urls
                              '("https://substitutes.nonguix.org"))))))))

(define meta-base-mod
  (operating-system-mod
    (name 'meta-base)
    (addons (list base-packages-mod guix-channels-mod))))


;;; Desktop mods.

(define replace-mesa-argument
  (mod-argument
    (keyword #:replace-mesa)
    (description
     "Procedure that consumes a package and replaces its mesa inputs.  By
default, this just returns an identical package.

This parameter can be directly set to the replacement procedure, but can also
accept a package, in which it will be turned into a procedure that grafts mesa
with that package.")
    (default-value identity)
    (sanitizer (lambda (val)
                 (cond ((procedure? val)
                        val)
                       ((package? val)
                        (package-input-rewriting `((,mesa . ,val))))
                       (else
                        (raise-exception
                         (make-exception
                          (make-exception-with-message
                           (format #f "not a procedure or package: ~s" val))))))))))

(define desktop-services-mod
  (operating-system-mod
    (name 'desktop-services)
    (description
     "Configures desktop services defined by Guix.

Some services are explicitly removed for modularity purposes (i.e. to be added
elsewhere in possibly different forms).")
    (services
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((replace-mesa replace-mesa-argument))
       (with-transformation replace-mesa
                            (modify-services %desktop-services
                              (delete guix-service-type)
                              (delete gdm-service-type)))))))

(define file-system-management-mod
  (operating-system-mod
    (name 'file-system-management)
    (description
     "Provides software to support various file system operations and disk
management/maintenance.")
    (packages (let-mod-arguments (this-operating-system-mod-arguments)
                  ((replace-mesa replace-mesa-argument))
                (list btrfs-progs
                      cryptsetup
                      (replace-mesa gnome-disk-utility)
                      gparted
                      gptfdisk
                      lvm2
                      ntfs-3g)))
    (services (list (service fstrim-service-type)))))

(define performance-mod
  (operating-system-mod
    (name 'performance)
    (description
     "Apply configurations for general improvements in system performance.")
    (services
     (list (service pam-limits-service-type)
           ;; Make system Esync-compatible.
           (simple-service name pam-limits-service-type
                           (list (pam-limits-entry "*" 'hard 'nofile 524288)))
           (simple-service name kernel-module-loader-service-type
                           '("ntsync"))))))

(define printers-mod
  (operating-system-mod
    (name 'printers)
    (description
     "Provides printing support via CUPS.")
    (packages (list nss-mdns))
    (services (list (service cups-service-type)))))

(define-configuration/no-serialization swapfile-configuration
  (file string "Path to swapfile.")
  (device string "Device that swapfile is present on.")
  (offset string "Offset of swapfile."))

(define swapfile-argument
  (mod-argument
    (keyword #:swapfile)
    (description "Swapfile configuration.")))

(define swapfile-mod
  (operating-system-mod
    (name 'swapfile)
    (description
     "Configures swapfile for the system.  See <info:guix#Swap Space> for more
information.  If the setup script in this repository is used to set up the
swapfile, it should output all the swapfile configuration information needed.

The base operating system must include the swapfile file system
configuration.")
    (swap-devices
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((base-configuration base-configuration-argument)
          (swapfile swapfile-argument))
       (match-record swapfile <swapfile-configuration> (file)
         (list (swap-space
                 (target file)
                 (dependencies
                  (filter (file-system-mount-point-predicate "/")
                          (operating-system-file-systems base-configuration))))))))
    (kernel-arguments
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((swapfile swapfile-argument))
       (match-record swapfile <swapfile-configuration> (device offset)
         (list (string-append "resume=" device)
               (string-append "resume_offset=" offset)))))))

(define tor-mod
  (operating-system-mod
    (name 'tor)
    (description
     "Configures tor.")
    (packages (list torsocks))
    (services (list (service tor-service-type)))))

(define virtualization-mod
  (operating-system-mod
    (name 'virtualization)
    (description
     "Adds virtualization packages and services to the system environment.

Authorized users should be part of the \"libvirt\" user group.")
    (packages (let-mod-arguments (this-operating-system-mod-arguments)
                  ((replace-mesa replace-mesa-argument))
                (list virt-manager
                      (replace-mesa gnome-boxes))))
    (services
     (list (service libvirt-service-type
                    (libvirt-configuration
                      (unix-sock-group "libvirt")))
           (service virtlog-service-type)
           (service qemu-binfmt-service-type
                    (qemu-binfmt-configuration
                      (platforms (lookup-qemu-platforms
                                  "arm"
                                  "aarch64"))))))))

(define meta-desktop-mod
  (operating-system-mod
    (name 'meta-desktop)
    (addons (list meta-base-mod
                  desktop-services-mod
                  performance-mod
                  file-system-management-mod
                  printers-mod
                  swapfile-mod
                  tor-mod
                  virtualization-mod))))
