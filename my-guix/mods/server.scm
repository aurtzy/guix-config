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
;;; This module provides mods geared towards server use.

(define-module (my-guix mods server)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:export (ssh-server-mod
            web-server-mod))

(use-package-modules web)

(use-service-modules ssh)

(define ssh-server-mod
  (mod
    (name 'ssh-server)
    (description
     "Provides an ssh service that runs in the background.")
    (os-extension
     (mod-os-services
      (list (service openssh-service-type
                     (openssh-configuration
                      (port-number 12122)
                      (password-authentication? #f))))))))

(define web-server-mod
  (mod
    (name 'web-server)
    (he-extension
     (compose (mod-he-packages
               (list darkhttpd))))))
