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

(define-module (my-guix extensions desktop)
  #:use-module (gnu)
  #:use-module (guix records)
  #:use-module (my-guix extensions)
  #:use-module (srfi srfi-1)
  #:export (<swapfile-configuration>
            swapfile-configuration
            swapfile-configuration?
            swapfile-configuration-file
            swapfile-configuration-device
            swapfile-configuration-offset

            build-swapfile-extension
            gnome-extension
            battery-extension))

(use-package-modules linux gnome gnome-xyz)

(use-service-modules xorg desktop pm)

(define-record-type* <swapfile-configuration>
  swapfile-configuration make-swapfile-configuration
  swapfile-configuration?
  ;; Path to swapfile.
  (file swapfile-configuration-file)
  ;; Device that swapfile is present on.
  (device swapfile-configuration-device)
  ;; Offset of swapfile.
  (offset swapfile-configuration-offset))

;;; Not really a service, but it fits in fairly fine.
(define (build-swapfile-extension config)
  "Builds swapfile extension, given a swapfile configuration CONFIG. See Guix
documentation on swapfiles for more information. If the setup script in this
repository is used to set up the swapfile, it should output this information
automatically."
  (let ((file (swapfile-configuration-file config))
        (device (swapfile-configuration-device config))
        (offset (swapfile-configuration-offset config)))
    (extension
      (name 'swapfile-extension)
      (apply
       (extender operating-system
         os =>
         (swap-devices
          (modify-list
           operating-system-swap-devices
           (list (swap-space (target file)
                             (dependencies
                              (filter
                               (file-system-mount-point-predicate "/")
                               (operating-system-file-systems os)))))))
         (kernel-arguments
          operating-system-user-kernel-arguments
          (list (string-append "resume=" device)
                (string-append "resume_offset=" offset))))))))

(define gnome-extension
  (extension
    (name 'gnome-extension)
    (apply
     (extender operating-system
       os =>
       (packages
        (modify-list
         operating-system-packages
         (list gvfs
               gnome-tweaks
               gnome-shell-extensions
               gnome-shell-extension-sound-output-device-chooser
               gnome-shell-extension-gsconnect)))
       (services
        (modify-list
         operating-system-user-services
         (cons* (set-xorg-configuration
                 (xorg-configuration
                  (keyboard-layout (operating-system-keyboard-layout os))))
                (service gnome-desktop-service-type)
                (modify-services
                    (gdm-service-type
                     config => (gdm-configuration
                                (inherit config)
                                (wayland? #t)))))))))))

(define battery-extension
  (extension
    (name 'battery-extension)
    (apply
     (extender operating-system
       (packages
        (modify-list
         operating-system-packages
         (list tlp)))
       (services
        (modify-list
         operating-system-user-services
         (list (service tlp-service-type
                        (tlp-configuration
                         (cpu-boost-on-ac? #t))))))))))
