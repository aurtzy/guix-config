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
;; This module defines extensions for desktop operating systems.

(define-module (my-guix system extensions desktop)
  #:use-module (gnu)
  #:use-module (my-guix extensions))

(use-package-modules linux gnome gnome-xyz)

(use-service-modules xorg desktop pm)

(define-public (build-swapfile-extension file device offset)
  "Builds swapfile extension located at FILE. The DEVICE the swapfile is on as
well as its OFFSET is needed. See Guix documentation on swapfiles for more
information. If the setup script in this repository is used to set up the
swapfile, it outputs this information automatically."
  (extension
    (name "swapfile")
    (configuration
     (extender operating-system
         os =>
       (swap-devices
        (cons (swap-space
               (target file)
               (dependencies (filter
                              (file-system-mount-point-predicate "/")
                              (operating-system-file-systems os))))
              (operating-system-swap-devices os)))
       (kernel-arguments
        (cons* (string-append "resume="device)
               (string-append "resume_offset="offset)
               (operating-system-user-kernel-arguments os)))))))

(define-public gnome-extension
  (extension
    (name "gnome")
    (configuration
     (extender operating-system
         os =>
       (packages
        (cons* gvfs
               gnome-tweaks
               gnome-shell-extensions
               gnome-shell-extension-sound-output-device-chooser
               gnome-shell-extension-gsconnect
               (operating-system-packages os)))
       (services
        (cons* (set-xorg-configuration
                (xorg-configuration
                 (keyboard-layout (operating-system-keyboard-layout os))))
               (service gnome-desktop-service-type)
               (modify-services (operating-system-user-services os)
                 (gdm-service-type config =>
                                   (gdm-configuration
                                    (inherit config)
                                    (wayland? #t))))))))))

(define-public battery-extension
  (extension
    (name "battery")
    (configuration
     (extender operating-system
         os =>
       (packages
        (cons* tlp
               (operating-system-packages os)))
       (services
        (cons* (service tlp-service-type
                        (tlp-configuration
                         (cpu-boost-on-ac? #t)))
               (operating-system-user-services os)))))))
