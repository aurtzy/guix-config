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
;;; This module defines mods for desktop operating systems.

(define-module (my-guix mods desktop)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:export (replace-mesa

            <swapfile-configuration>
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
                     gl gnome gnome-xyz
                     qt kde-plasma kde-frameworks
                     virtualization)

(use-service-modules xorg desktop pm virtualization)

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
       (mod-operating-system
         os =>
         (swap-devices
          (list (swap-space (target file)
                            (dependencies
                             (filter
                              (file-system-mount-point-predicate "/")
                              (operating-system-file-systems os))))))
         (kernel-arguments
          (list (string-append "resume=" device)
                (string-append "resume_offset=" offset))))))))

(define gnome-mod
  (mod
    (name 'gnome-mod)
    (description
     "Provides configurations for the GNOME desktop environment.")
    (apply
     (compose-lambda (os)
       (let ((replace-mesa (replace-mesa)))
         (list
          (mod-os-packages
           (map replace-mesa
                (list gvfs
                      gnome-tweaks
                      gnome-shell-extensions
                      gnome-shell-extension-gsconnect
                      xdg-desktop-portal-kde)))
          (mod-os-services
           (list (set-xorg-configuration
                  (xorg-configuration
                   (keyboard-layout (operating-system-keyboard-layout os))))
                 (service gnome-desktop-service-type
                          (gnome-desktop-configuration
                           (core-services
                            (list (replace-mesa gnome-meta-core-services)))
                           (shell
                            (list (replace-mesa gnome-meta-core-shell)))
                           (utilities
                            (list (replace-mesa gnome-meta-core-utilities)))
                           (extra-packages
                            (list (replace-mesa gnome-essential-extras)))))
                 (service gdm-service-type)))))))))

(define battery-mod
  (mod
    (name 'battery-mod)
    (apply
     (compose (mod-os-packages
               (list tlp))
              (mod-os-services
               (list (service tlp-service-type
                              (tlp-configuration
                               (cpu-boost-on-ac? #t)))))))))

(define virtualization-mod
  (mod
    (name 'virtualization-mod)
    (apply
     (mod-operating-system
       (packages
        (list virt-manager
              gnome-boxes))
       (services
        (list (service libvirt-service-type
                       (libvirt-configuration
                        (unix-sock-group "libvirt")))
              (service virtlog-service-type)))))))
