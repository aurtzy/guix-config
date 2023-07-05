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
  #:use-module (my-guix extensions)
  #:use-module (my-guix home services)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay))

(use-package-modules minetest
                     sdl)

;; NOTE for lutris: must disallow access to home because of weird pthread
;; library issue with Guix
;; (e.g. https://logs.guix.gnu.org/guix/2023-01-17.log#124520)
(define-public game-managers-extension
  (extension
    (name "game-managers")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* sdl2
               (home-environment-packages env)))
       (services
        (cons* (stow-service 'stow-lutris "lutris")
               (stow-service 'stow-steam "steam")
               (flatpak-service 'flatpak-game-managers
                                'flathub
                                '("net.lutris.Lutris"
                                  "net.davidotek.pupgui2"
                                  "com.valvesoftware.Steam"))
               (home-environment-user-services env)))))))

(define-public minecraft-extension
  (extension
    (name "minecraft")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* glfw-wayland-minecraft
               (home-environment-packages env)))
       (services
        (cons* (flatpak-service 'minecraft-flatpak
                                'flathub
                                '("org.prismlauncher.PrismLauncher"))
               (home-environment-user-services env)))))))

(define-public minetest-extension
  (extension
    (name "minetest")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* minetest
               (home-environment-packages env)))))))

(define-public syncplay-extension
  (extension
    (name "syncplay")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* syncplay
               (home-environment-packages env)))))))
