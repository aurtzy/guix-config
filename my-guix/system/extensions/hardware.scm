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
;; This module defines system-level extensions for hardware.

(define-module (my-guix system extensions hardware)
  #:use-module (gnu)
  #:use-module (my-guix extensions)
  #:use-module (my-guix services hardware))

(define-public keyboard-center-extension
  (extension
    (name "keyboard-center")
    (configuration
     (extender operating-system
         os =>
       (services
        (cons* (service keyboard-center-service-type)
               (operating-system-user-services os)))))))
