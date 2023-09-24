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
;;; This module provides entertainment-related mods.

(define-module (my-guix home mods entertainment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix home mods common)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay)
  #:use-module (my-guix utils)
  #:export (game-mangers-mod
            minecraft-mod
            minetest-mod
            syncplay-mod

            entertainment-mods))

(use-package-modules minetest
                     sdl)

(define game-managers-mod
  (let* ((steam-dest ".var/app/com.valvesoftware.Steam")
         (terraria-dest (path-append steam-dest
                                     ".local/share/Terraria"))
         (tmodloader-dest (path-append terraria-dest
                                       "tModLoader"))

         (terraria-src (path-append-my-home "areas/games/terraria"))
         (tmodloader-src (path-append terraria-src "tmodloader")))
    (mod
      (name 'game-managers-mod)
      (dependencies
       (list flatpak-mod))
      (apply
       (apply-mod home-environment
         (packages
          home-environment-packages
          append=>
          (list sdl2))
         (services
          home-environment-user-services
          append=>
          (list (simple-service name
                                home-impure-symlinks-service-type
                                `( ;; Flatpak overrides
                                  (".local/share/flatpak/overrides"
                                   ,(path-append-my-files "impure/lutris")
                                   "net.lutris.Lutris")
                                  (".local/share/flatpak/overrides"
                                   ,(path-append-my-files "impure/steam")
                                   "com.valvesoftware.Steam")
                                  ;; Mindustry
                                  (".var/app/net.lutris.Lutris/data/Mindustry"
                                   ,(path-append-my-home
                                     "areas/games/mindustry")
                                   "saves"
                                   "settings.bin")
                                  ;; Terraria
                                  (,(path-append tmodloader-dest
                                                 "Players/Backups")
                                   ,(path-append tmodloader-src
                                                 "backups/players"))
                                  (,(path-append tmodloader-dest
                                                 "Worlds/Backups")
                                   ,(path-append tmodloader-src
                                                 "backups/worlds"))))
                (simple-service name
                                home-flatpak-profile-service-type
                                '((flathub "net.lutris.Lutris")
                                  (flathub "net.davidotek.pupgui2")
                                  (flathub "com.valvesoftware.Steam")
                                  (flathub "com.github.Matoking.protontricks"))))))))))

(define minecraft-mod
  (mod
    (name 'minecraft-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list glfw-wayland-minecraft))
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "org.prismlauncher.PrismLauncher")))))))))

(define minetest-mod
  (mod
    (name 'minetest-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list minetest))))))

(define syncplay-mod
  (mod
    (name 'syncplay-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list syncplay))))))

(define entertainment-mods
  (list game-managers-mod
        minecraft-mod
        minetest-mod
        syncplay-mod))