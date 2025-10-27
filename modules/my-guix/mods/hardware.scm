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
;;; This module defines mods for hardware-related configurations.

(define-module (my-guix mods hardware)
  #:use-module (gnu)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix mods)
  #:use-module (my-guix utils)
  #:use-module (nonguix transformations)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (battery-mod
            nvidia-proprietary?-argument
            nvidia-mod))

(use-package-modules linux)

(use-service-modules pm)

(define battery-mod
  (operating-system-mod
    (name 'battery)
    (description
     "Configures system for use on a battery.  Ideal for laptop
configurations.")
    (packages (list tlp))
    (services
     (list (service tlp-service-type
                    (tlp-configuration
                      (cpu-boost-on-ac? #t)))))))

(define nvidia-proprietary?-argument
  (mod-argument
    (keyword #:nvidia-proprietary?)
    (description
     "Whether or not the proprietary NVIDIA driver is being used.")
    (default-value #f)))

(define nvidia-mod
  (operating-system-mod
    (name 'nvidia)
    (description
     "Configures the system for an NVIDIA GPU.")
    (kernel-arguments
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((nvidia-proprietary? nvidia-proprietary?-argument))
       (if nvidia-proprietary?
           '()
           ;; Enable GSP firmware for Nouveau/NVK.
           (list "nouveau.config=NvGspRm=1"))))
    (modifier
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((nvidia-proprietary? nvidia-proprietary?-argument))
       (if nvidia-proprietary?
           (nonguix-transformation-nvidia #:open-source-kernel-module? #t)
           identity)))))
