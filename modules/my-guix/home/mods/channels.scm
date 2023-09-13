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
;;; This module provides channels mods.

(define-module (my-guix home mods channels)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services guix)
  #:use-module (guix channels)
  #:use-module (my-guix mods))

(define-public nonguix-channel-mod
  (mod
    (name 'nonguix-channel-mod)
    (apply
     (apply-mod home-environment
       (services
        (modify-list
         home-environment-user-services
         (list
          (simple-service name
                          home-channels-service-type
                          (list
                           (channel
                            (name 'nonguix)
                            (url "https://gitlab.com/nonguix/nonguix")
                            (introduction
                             (make-channel-introduction
                              "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                              (openpgp-fingerprint
                               "2A39 3FFF 68F4 EF7A 3D29 12AF 6F51 20A0 22FB B2D5")))))))))))))
