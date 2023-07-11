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
;; This module defines hardware services.

(define-module (my-guix services hardware)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (my-guix packages keyboard-center)
  #:export (keyboard-center-service-type))

(define keyboard-center-service-type
  (let ((keyboard-center-pkg (const (list keyboard-center))))
    (service-type (name 'keyboard-center)
                  (extensions
                   (list (service-extension udev-service-type
                                            keyboard-center-pkg)
                         (service-extension profile-service-type
                                            keyboard-center-pkg)))
                  (description "Enable udev rules for keyboard-center.")
                  (default-value '()))))
