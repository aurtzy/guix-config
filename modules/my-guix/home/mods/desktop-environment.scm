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
;;; This module provides home-environment mods for specific desktop
;;; environments.

(define-module (my-guix home mods desktop-environment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix mods)
  #:export (wayland-mod
            gnome-mod
            plasma-mod))

(use-package-modules qt)

(define wayland-mod
  (mod
    (name 'wayland)
    (description
     "Configures environment for usage on Wayland compositors.")
    (apply
     (compose
      (mod-he-services
       (list (simple-service name
                             home-bash-service-type
                             (home-bash-extension
                              (environment-variables
                               '(("MOZ_ENABLE_WAYLAND" . "1")))))))))))

(define gnome-mod
  (mod
    (name 'gnome)
    (dependencies
     (list wayland-mod))))

(define plasma-mod-shortcuts
  #~(begin
      ;; Use Overview as default action for Meta
      ;; See: https://zren.github.io/kde/#windowsmeta-key
      (invoke "kwriteconfig5"
              "--file"
              (string-append (getenv "HOME")
                             "/.config/kwinrc")
              "--group"
              "ModifierOnlyShortcuts"
              "--key"
              "Meta"
              (string-join
               (list "org.kde.kglobalaccel"
                     "/component/kwin"
                     "org.kde.kglobalaccel.Component"
                     "invokeShortcut"
                     "Overview")
               ","))
      (invoke "qdbus"
              "org.kde.KWin"
              "/KWin"
              "reconfigure")))

(define plasma-mod
  (mod
    (name 'plasma)
    (dependencies
     (list wayland-mod))
    (apply
     (mod-home-environment
       (services
        (list (simple-service name
                              home-activation-service-type
                              plasma-mod-shortcuts)))))))
