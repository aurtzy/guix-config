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
;;; This module defines configurations for the "al-pc" machine.

(define-module (my-guix systems al-pc)
  #:use-module (gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu services linux)
  #:use-module (gnu services sddm)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system privilege)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (my-guix config)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix mods desktop-environment)
  #:use-module (my-guix mods entertainment)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix mods server)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix packages keyboard-center)
  #:use-module (my-guix packages redlib)
  #:use-module (my-guix services hardware)
  #:use-module ((my-guix systems)
                #:select ((initial-desktop-operating-system . initial-os)))
  #:use-module (my-guix utils)
  #:use-module (nongnu packages linux)
  #:use-module ((nongnu packages nvidia) #:prefix nvidia:)
  #:use-module ((nongnu services nvidia) #:prefix nvidia:)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nonguix utils)
  #:export (modded-operating-system))

(define base-operating-system
  (operating-system
    (inherit initial-os)
    (host-name "al-pc")
    (kernel linux)
    (label (format #f "GNU with ~a ~a (Nouveau)"
                   (string-titlecase (package-name kernel))
                   (package-version kernel)))
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (kernel-arguments
     (cons* "preempt=full"   ; Mitigate stuttering and audio crackling issues.
            (operating-system-user-kernel-arguments initial-os)))
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
                                      "libvirt")))
            (operating-system-users initial-os)))
    (mapped-devices
     (list (mapped-device
             (source
              (uuid "7ccff0a3-b181-4788-9892-e68306566325"))
             (target "cryptroot")
             ;; TODO: Get key file working with root drive
             (type luks-device-mapping))
           (mapped-device
             (source
              (uuid "d50f62c1-e312-4c83-8b55-b0af00a4de2a"))
             (target "cryptstorage")
             (type luks-device-mapping)
             (arguments `(#:key-file ,(string-append
                                       "/root/keys/" (uuid->string source)))))))
    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device "/dev/mapper/cryptroot")
              (flags
               (base-file-system-flags-ref 'btrfs 'ssd))
              (options
               (alist->file-system-options
                (base-file-system-options-ref 'btrfs 'ssd)))
              (type "btrfs")
              (dependencies (filter
                             (lambda (dev)
                               (member "cryptroot"
                                       (mapped-device-targets dev)))
                             mapped-devices)))
            ;; XXX: This file-system requires cryptstorage map to exist,
            ;; otherwise reconfiguration fails.  Can be a problem in cases
            ;; where storage may not be available (e.g. from configuration
            ;; messups/testing that exclude cryptstorage mapping).  Current
            ;; process is to comment out this file-system, reconfigure, then
            ;; uncomment and reconfigure again once cryptstorage mapping
            ;; exists.
            (file-system
              (mount-point "/home/alvin/storage")
              (device "/dev/mapper/cryptstorage")
              (flags
               (base-file-system-flags-ref 'btrfs 'hdd))
              (options
               (alist->file-system-options
                (base-file-system-options-ref 'btrfs 'hdd)))
              (type "btrfs")
              (create-mount-point? #t)
              (mount-may-fail? #t)
              (dependencies mapped-devices))
            (file-system
              (mount-point "/boot/efi")
              (device (uuid "DC21-DB63" 'fat32))
              (type "vfat"))
            (file-system
              (mount-point "/media/backup")
              (device "/dev/mapper/luks-f42810d8-c723-4521-9646-da12f6103b59")
              (flags
               (base-file-system-flags-ref 'btrfs 'hdd))
              (options
               (alist->file-system-options
                '(("compress-force" . "zstd:10"))))
              (type "btrfs")
              (mount? #f))
            (operating-system-file-systems initial-os)))
    (services
     (cons* (service keyboard-center-service-type)
            (simple-service 'load-ntsync kernel-module-loader-service-type
                            '("ntsync"))
            (operating-system-user-services initial-os)))))

(define modded-operating-system
  (modded-configuration
    (arguments (list
                #:swapfile (swapfile-configuration
                            (file "/swapfile")
                            (device "/dev/mapper/cryptroot")
                            (offset "6036736"))))
    (base base-operating-system)
    (mods (list meta-desktop-mod
                meta-entertainment-mod
                gnome-mod
                nvidia-mod
                ssh-server-mod))))
