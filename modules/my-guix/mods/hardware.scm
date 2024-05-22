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
  #:export (battery-mod))

(use-package-modules linux)

(use-service-modules pm)

(define battery-mod
  (mod
    (name 'battery)
    (description
     "Configures system for use on a battery.  Ideal for laptop
configurations.")
    (apply
     (compose (mod-os-packages
               (list tlp))
              (mod-os-services
               (list (service tlp-service-type
                              (tlp-configuration
                               (cpu-boost-on-ac? #t)))))))))
