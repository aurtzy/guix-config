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
;;; This module defines configurations for the "al-laptop" machine.

(define-module (my-guix systems al-laptop)
  #:use-module (gnu)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services networking)
  #:use-module (gnu system file-systems)
  #:use-module (guix packages)
  #:use-module (my-guix config)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix mods desktop-environment)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix mods server)
  #:use-module ((my-guix systems)
                #:select ((initial-desktop-operating-system . initial-os)))
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (modded-operating-system))

(use-package-modules radio)

(define base-operating-system
  (operating-system
    (inherit initial-os)
    (kernel linux)
    (firmware (cons* atheros-firmware
                     (operating-system-firmware initial-os)))
    (initrd microcode-initrd)
    (kernel-arguments
     (cons* "modprobe.blacklist=dvb_usb_rtl28xxu"
            (operating-system-user-kernel-arguments initial-os)))
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
            (operating-system-users initial-os)))
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
            (operating-system-file-systems initial-os)))
    (packages
     (cons* rtl-sdr
            (operating-system-packages initial-os)))
    (services
     (cons*
      (udev-rules-service 'rtl-sdr rtl-sdr)
      (operating-system-user-services initial-os)))))

(define modded-operating-system
  (modded-configuration
    (arguments (list
                #:swapfile (swapfile-configuration
                            (file "/swapfile")
                            (device "/dev/mapper/cryptroot")
                            (offset "269568"))))
    (base base-operating-system)
    (mods (list meta-desktop-mod
                battery-mod
                bluetooth-mod
                gnome-mod
                ssh-server-mod
                (operating-system-mod
                  (name 'vpn)
                  (modifier
                   (lambda (os)
                     (operating-system
                       (inherit os)
                       (services
                        (modify-services (operating-system-user-services os)
                          (network-manager-service-type
                           config => (network-manager-configuration
                                       (inherit config)
                                       (vpn-plugins
                                        (list network-manager-openconnect))))))))))))))
