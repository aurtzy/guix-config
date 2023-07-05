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
;; This module defines base operating systems for desktop usage.

(define-module (my-guix system base desktop)
  #:use-module (gnu)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system pam)
  #:use-module (my-guix config)
  #:use-module (my-guix utils))

(use-package-modules linux certs 
                     disk xdisorg)

(use-service-modules cups desktop)

;; Base desktop operating system. This configuration is missing proper
;; filesystem configurations that must be configured per-system.
(define-public base-desktop-operating-system
  (operating-system
    (host-name "a-guix-system")
    (timezone "America/New_York")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us"))
    (users
     (cons* (user-account
             (name "alvin")
             (group "users")
             (home-directory "/home/alvin")
             (supplementary-groups '("wheel"
                                     "netdev"
                                     "audio"
                                     "video")))
            %base-user-accounts))
    (packages
     (cons* nss-certs  ;certifications required for https
            ;; useful disk management utilities
            gparted
            btrfs-progs
            ;; for checking if programs are using wayland
            xeyes
            
            %base-packages))
    (services
     (cons* (simple-service 'add-guix-config-path
                            session-environment-service-type
                            '(("GUIX_PACKAGE_PATH"
                               . ,(build-path-augmentation
                                   "GUIX_PACKAGE_PATH"
                                   $modules-dir))))
            (service cups-service-type)
            %desktop-services))
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
              (mount-point "/mnt/backup-usb")
              (device (uuid "207d03a2-8838-4578-baba-dfb1af375da1"))
              (type "f2fs")
              (options
               (alist->file-system-options
                '("x-gvfs-show")))
              (mount? #f))
            %base-file-systems))))
