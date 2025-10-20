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
;;; This module defines configurations for the "al-laptop" machine.

(define-module (my-guix systems al-laptop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services networking)
  #:use-module (gnu system file-systems)
  #:use-module (guix packages)
  #:use-module (my-guix config)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods data)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix mods desktop-environment)
  #:use-module (my-guix mods desktop-extra)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix mods server)
  #:use-module ((my-guix systems)
                #:select ((base-desktop-operating-system . base-os)))
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (al-laptop-operating-system))

(use-package-modules radio)

(define al-laptop-operating-system
  (operating-system
    (inherit base-os)
    (kernel linux)
    (initrd microcode-initrd)
    (kernel-arguments
     ;; Fix keyboard not working when resuming from suspend
     (cons* "i8042.dumbkbd"
            "modprobe.blacklist=dvb_usb_rtl28xxu"
            (operating-system-user-kernel-arguments base-os)))
    (host-name "al-laptop")
    (users
     (cons* (user-account
              (name "alvin")
              (comment "Alvin")
              (group "users")
              (home-directory "/home/alvin")
              (supplementary-groups '("wheel"
                                      "netdev"
                                      "audio"
                                      "video"
                                      "kvm"
                                      "libvirt"
                                      ;; rtl-sdr
                                      "dialout")))
            (operating-system-users base-os)))
    (mapped-devices
     (list (mapped-device
             (source
              (uuid "bf8c2749-f357-4c6e-be6b-f1009a58aa5f"))
             (target "cryptroot")
             (type luks-device-mapping))))
    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device "/dev/mapper/cryptroot")
              (type "btrfs")
              (flags
               (base-file-system-flags-ref 'btrfs 'ssd))
              (options
               (alist->file-system-options
                (base-file-system-options-ref 'btrfs 'ssd)))
              (dependencies mapped-devices))
            (file-system
              (mount-point "/boot/efi")
              (device (uuid "DC21-DB63"
                            'fat32))
              (type "vfat"))
            (operating-system-file-systems base-os)))
    (packages
     (cons* rtl-sdr
            (operating-system-packages base-os)))
    (services
     (cons*
      (udev-rules-service 'rtl-sdr rtl-sdr)
      (operating-system-user-services base-os)))))

