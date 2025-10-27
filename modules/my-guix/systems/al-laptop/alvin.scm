;;; Copyright © 2025 Alvin Hsu <aurtzy@gmail.com>
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
;;; This module defines configurations for the "alvin" user on al-laptop.

(define-module (my-guix systems al-laptop alvin)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix home mods base)
  #:use-module (my-guix home mods desktop-extra)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module ((my-guix systems)
                #:select ((initial-desktop-home-environment . initial-he)))
  #:export (modded-home-environment))

(define base-home-environment
  (home-environment
    (inherit initial-he)
    ))

(define modded-home-environment
  (modded-configuration
    (arguments (list
                #:ignored-mods (list home-creative-mod
                                     home-personal-comms-mod)
                #:home-data-entries '("workshop" "areas" "library" "archives")))
    (base base-home-environment)
    (mods (list home-meta-desktop-mod
                home-meta-desktop-extra-mod))))
