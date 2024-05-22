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
;;; This module defines mods commonly used on all systems.

(define-module (my-guix mods base)
  #:use-module (gnu)
  #:use-module (my-guix mods)
  #:use-module (my-guix utils)
  #:export (base-packages-mod
            nonguix-channel-mod

            base-mods))

(use-package-modules version-control)

(define base-packages-mod
  (mod
    (name 'base-packages)
    (description
     "Provides the base set of packages defined by Guix as well as additional
packages deemed essential.")
    (apply
     (mod-os-packages (cons* git
                             %base-packages)))))

(define nonguix-channel-mod
  (mod
    (name 'nonguix-channel)
    (description
     "Sets up the Nonguix channel on this system.  The channel must still be
enabled in the home environment.")
    (apply (compose
            (mod-os-services
             (list
              (simple-service name
                              guix-service-type
                              (guix-extension
                               (authorized-keys
                                (list (local-file (path-append-my-files
                                                   "guix/nonguix.pub"))))
                               (substitute-urls
                                '("https://substitutes.nonguix.org"))))))))))

(define base-mods (list base-packages-mod
                        nonguix-channel-mod))
