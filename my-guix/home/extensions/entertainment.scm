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
;; This module provides entertainment-related extensions.

(define-module (my-guix home extensions entertainment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay)
  #:export (game-mangers-extension
            minecraft-extension
            minetest-extension
            syncplay-extension

            entertainment-extensions))

(use-package-modules minetest
                     sdl)

;; NOTE for lutris: must disallow access to home (e.g. via flatseal) because
;; of weird pthread library issue with Guix
;; (e.g. https://logs.guix.gnu.org/guix/2023-01-17.log#124520)
(define game-managers-extension
  (extension
    (name 'game-managers-extension)
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* sdl2
               (home-environment-packages env)))
       (services
        (cons* (simple-service name
                               home-stow-service-type
                               (list "lutris"
                                     "steam"))
               (simple-service name
                               home-flatpak-profile-service-type
                               '(("net.lutris.Lutris" . flathub)
                                 ("net.davidotek.pupgui2" . flathub)
                                 ("com.valvesoftware.Steam" . flathub)))
               (home-environment-user-services env)))))))

(define minecraft-extension
  (extension
    (name 'minecraft-extension)
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* glfw-wayland-minecraft
               (home-environment-packages env)))
       (services
        (cons* (simple-service name
                               home-flatpak-profile-service-type
                               '(("org.prismlauncher.PrismLauncher"
                                  . flathub)))
               (home-environment-user-services env)))))))

(define minetest-extension
  (extension
    (name 'minetest-extension)
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* minetest
               (home-environment-packages env)))))))

(define syncplay-extension
  (extension
    (name 'syncplay-extension)
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* syncplay
               (home-environment-packages env)))))))

(define entertainment-extensions
  (list game-managers-extension
        minecraft-extension
        minetest-extension
        syncplay-extension))
