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
;;; This module defines mods for desktop operating systems.

(define-module (my-guix mods desktop)
  #:use-module (gnu)
  #:use-module (guix records)
  #:use-module (my-guix mods)
  #:use-module (srfi srfi-1)
  #:export (<swapfile-configuration>
            swapfile-configuration
            swapfile-configuration?
            swapfile-configuration-file
            swapfile-configuration-device
            swapfile-configuration-offset

            build-swapfile-mod
            gnome-mod
            battery-mod
            virtualization-mod))

(use-package-modules linux freedesktop
                     gnome gnome-xyz
                     qt kde-plasma kde-frameworks
                     virtualization)

(use-service-modules xorg desktop pm virtualization)

(define-record-type* <swapfile-configuration>
  swapfile-configuration make-swapfile-configuration
  swapfile-configuration?
  ;; Path to swapfile.
  (file swapfile-configuration-file)
  ;; Device that swapfile is present on.
  (device swapfile-configuration-device)
  ;; Offset of swapfile.
  (offset swapfile-configuration-offset))

(define (build-swapfile-mod config)
  "Builds swapfile mod, given a swapfile configuration CONFIG. See Guix
documentation on swapfiles for more information. If the setup script in this
repository is used to set up the swapfile, it should output this information
automatically."
  (let ((file (swapfile-configuration-file config))
        (device (swapfile-configuration-device config))
        (offset (swapfile-configuration-offset config)))
    (mod
      (name 'swapfile-mod)
      (apply
       (apply-mod operating-system
         os =>
         (swap-devices
          operating-system-swap-devices
          append=>
          (list (swap-space (target file)
                            (dependencies
                             (filter
                              (file-system-mount-point-predicate "/")
                              (operating-system-file-systems os))))))
         (kernel-arguments
          operating-system-user-kernel-arguments
          append=>
          (list (string-append "resume=" device)
                (string-append "resume_offset=" offset))))))))

(define wayland-mod
  (mod
    (name 'wayland-mod)
    (apply
     (apply-mod operating-system
       (packages
        operating-system-packages
        append=>
        (list xdg-desktop-portal
              ;; qtwayland
              ))))))

(define gnome-mod
  (mod
    (name 'gnome-mod)
    (dependencies
     (list wayland-mod))
    (apply
     (apply-mod operating-system
       os =>
       (packages
        operating-system-packages
        append=>
        (list xdg-desktop-portal-gtk
              gvfs
              gnome-tweaks
              gnome-shell-extensions
              gnome-shell-extension-sound-output-device-chooser
              gnome-shell-extension-gsconnect
              gnome-shell-extension-clipboard-indicator))
       (services
        operating-system-user-services
        append=>
        (list
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout (operating-system-keyboard-layout os))))
         (service gnome-desktop-service-type)
         (service gdm-service-type
                  (gdm-configuration
                   (wayland? #t)))))))))

(define battery-mod
  (mod
    (name 'battery-mod)
    (apply
     (apply-mod operating-system
       (packages
        operating-system-packages
        append=>
        (list tlp))
       (services
        operating-system-user-services
        append=>
        (list (service tlp-service-type
                       (tlp-configuration
                        (cpu-boost-on-ac? #t)))))))))

(define virtualization-mod
  (mod
    (name 'virtualization-mod)
    (apply
     (apply-mod operating-system
       (packages
        operating-system-packages
        append=>
        (list virt-manager
              gnome-boxes))
       (services
        operating-system-user-services
        append=>
        (list (service libvirt-service-type
                       (libvirt-configuration
                        (unix-sock-group "libvirt")))
              (service virtlog-service-type)))))))
