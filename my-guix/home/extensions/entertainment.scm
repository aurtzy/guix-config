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
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay)
  #:use-module (my-guix utils)
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
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list sdl2)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-impure-symlinks-service-type
                               `((".local/share/flatpak/overrides/net.lutris.Lutris"
                                  ,(search-files-path
                                    "impure/lutris/net.lutris.Lutris"))
                                 (".local/share/flatpak/overrides/com.valvesoftware.Steam"
                                  ,(search-files-path
                                    "impure/steam/com.valvesoftware.Steam"))))
               (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "net.lutris.Lutris")
                                 (flathub "net.davidotek.pupgui2")
                                 (flathub "com.valvesoftware.Steam")
                                 (flathub "com.github.Matoking.protontricks"))))))))))

(define minecraft-extension
  (extension
    (name 'minecraft-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list glfw-wayland-minecraft)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "org.prismlauncher.PrismLauncher"))))))))))

(define minetest-extension
  (extension
    (name 'minetest-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list minetest)))))))

(define syncplay-extension
  (extension
    (name 'syncplay-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list syncplay)))))))

(define entertainment-extensions
  (list game-managers-extension
        minecraft-extension
        minetest-extension
        syncplay-extension))
