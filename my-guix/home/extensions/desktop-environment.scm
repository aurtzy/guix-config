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
;; This module provides home-environment extensions for specific desktop
;; environments.

(define-module (my-guix home extensions desktop-environment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix extensions)
  #:export (wayland-extension
            gnome-extension
            plasma-extension))

(use-package-modules qt)

(define wayland-extension
  (extension
    (name 'wayland-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-bash-service-type
                               (home-bash-extension
                                (environment-variables
                                 '(("MOZ_ENABLE_WAYLAND" . "1"))))))))))))

(define gnome-extension
  (extension
    (name 'gnome-extension)
    (dependencies
     (list wayland-extension))))

(define plasma-extension-shortcuts
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

(define plasma-extension
  (extension
    (name 'plasma-extension)
    (dependencies
     (list wayland-extension))
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-activation-service-type
                               plasma-extension-shortcuts))))))))
