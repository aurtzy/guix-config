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
  #:use-module (guix channels)
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
    (os-extension
     (mod-os-packages (cons* git
                             %base-packages)))))

(define nonguix-channel-mod
  (mod
    (name 'nonguix-channel)
    (description
     "Sets up the Nonguix channel on this system.  The channel must still be
enabled in the home environment.")
    (os-extension
     (lambda (os)
       (operating-system
        (inherit os)
        (services
         (cons*
          (simple-service name
                          guix-service-type
                          (guix-extension
                           (authorized-keys
                            (list (local-file (path-append-my-files
                                               "guix/nonguix.pub"))))
                           (substitute-urls
                            '("https://substitutes.nonguix.org"))))
          (modify-services (operating-system-user-services os)
            (guix-service-type
             config
             => (guix-configuration
                 (inherit config)
                 (channels
                  (cons*
                   (channel
                    (name 'nonguix)
                    (url "https://gitlab.com/nonguix/nonguix")
                    (branch "master")
                    (introduction
                     (make-channel-introduction
                      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                      (openpgp-fingerprint
                       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                   %default-channels))))))))))))

(define base-mods (list base-packages-mod
                        nonguix-channel-mod))
