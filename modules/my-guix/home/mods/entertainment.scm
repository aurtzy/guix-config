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

(define games-src
  (path-append-my-home "areas/games"))

;; TODO Source paths for steam need to go through data store instead of
;; symlinks in home currently due to steam flatpak currently not liking
;; symlinks.  This is not optimal, so readjust whenever this is fixed.
;;
;; See: https://github.com/flatpak/flatpak/issues/1971
(define games-src-steam
  (path-append-my-home "data/store/areas/games"))

;; TODO probably better to put this mod elsewhere since it's more of a general
;; tool than specifically for entertainment
(define bottles-mod
  (mod
    (name 'bottles-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "com.usebottles.bottles")))
              (simple-service name
                              home-impure-symlinks-service-type
                              `((".local/share/flatpak/overrides"
                                 ,(path-append-my-files "impure/bottles")
                                 "com.usebottles.bottles")))))))))

(define game-managers-mod
  (let* ((lutris-dest ".var/app/net.lutris.Lutris/data")
         (steam-dest ".var/app/com.valvesoftware.Steam"))
    (mod
      (name 'game-managers-mod)
      (dependencies
       (list flatpak-mod
             bottles-mod))
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
                                  (,(path-append lutris-dest "Mindustry")
                                   ,(path-append games-src "mindustry/files")
                                   "saves"
                                   "settings.bin")
                                  ;; Factorio
                                  (,steam-dest
                                   ,(path-append games-src-steam
                                                 "factorio/files")
                                   ".factorio")
                                  ;; tModLoader
                                  (,(path-append steam-dest
                                                 ".local/share/Terraria"
                                                 "tModLoader")
                                   ,(path-append games-src-steam
                                                 "tmodloader/files")
                                   "Players/Backups"
                                   "Worlds/Backups"
                                   "Captures")))
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
