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
;;; This module defines hardware-specific mods.

(define-module (my-guix home mods hardware)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix home mods common)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
  #:export (pipewire-mod))

(define pipewire-mod
  (mod
    (name 'pipewire-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((".config/easyeffects/input"
                                 ,(path-append-my-files "impure/pipewire")
                                 "main-mic.json")))
              (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "com.github.wwmm.easyeffects")))))))))
