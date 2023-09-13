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
;;; This module defines mods that add channels.

(define-module (my-guix mods channels)
  #:use-module (gnu)
  #:use-module (my-guix utils)
  #:use-module (my-guix mods))

(define-public nonguix-channel-mod
  (mod
    (name 'nonguix-channel-mod)
    (apply
     (apply-mod operating-system
       (services
        operating-system-user-services
        append=>
        (list (simple-service name
                              guix-service-type
                              (guix-extension
                               (authorized-keys
                                (list (local-file (search-files-path
                                                   "guix/nonguix.pub"))))
                               (substitute-urls
                                '("https://substitutes.nonguix.org"))))))))))
