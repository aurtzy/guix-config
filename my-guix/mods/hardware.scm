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
;;; This module defines mods for hardware-related configurations.

(define-module (my-guix mods hardware)
  #:use-module (gnu)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix utils)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (nvidia-proprietary?

            battery-mod
            nvidia-mod))

(use-package-modules linux)

(use-service-modules pm)

;; nvidia-proprietary?: Parameter that dictates whether the NVIDIA proprietary
;; driver will be used.
(define nvidia-proprietary?
  (make-parameter #f (lambda (val)
                       (rnrs:assert (boolean? val))
                       val)))

(define battery-mod
  (mod
    (name 'battery)
    (description
     "Configures system for use on a battery.  Ideal for laptop
configurations.")
    (os-extension
     (compose (mod-os-packages
               (list tlp))
              (mod-os-services
               (list (service tlp-service-type
                              (tlp-configuration
                               (cpu-boost-on-ac? #t)))))))))

(define nvidia-mod
  (mod
    (name 'nvidia)
    (description
     "Configures the system for an NVIDIA GPU.")
    (os-extension
     (compose-lambda _
       (let ((nvidia-proprietary? (nvidia-proprietary?)))
         (list (mod-os-services
                (if nvidia-proprietary?
                    (list (service (module-ref (resolve-interface
                                                '(nongnu services nvidia))
                                               'nvidia-service-type)))
                    '()))
               (mod-os-kernel-arguments
                (if nvidia-proprietary?
                    (list "modprobe.blacklist=nouveau"
                          "nvidia_drm.modeset=1")
                    (list "nouveau.config=NvGspRm=1")))))))))
