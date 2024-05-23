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
;;; This module provides entertainment-related mods.

(define-module (my-guix home mods entertainment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix home mods common)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay)
  #:use-module (my-guix utils)
  #:export (game-mangers-mod
            minecraft-mod
            minetest-mod
            syncplay-mod

            entertainment-mods))

(use-package-modules freedesktop minetest sdl)

(define games-src
  (path-append-my-home "areas/games"))

;; TODO probably better to put this mod elsewhere since it's more of a general
;; tool than specifically for entertainment
(define bottles-mod
  (mod
    (name 'bottles)
    (dependencies
     (list flatpak-mod))
    (apply
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "com.usebottles.bottles")))
             (simple-service name
                             home-impure-symlinks-service-type
                             `((".local/share/flatpak/overrides"
                                ,(path-append-my-files "impure/bottles")
                                "com.usebottles.bottles")))))))))

;; TODO Set sysctl config option to optimize for gaming
;; https://www.phoronix.com/news/Arch-Linux-vm.max_map_count
(define game-managers-mod
  (let* ((lutris-dest ".var/app/net.lutris.Lutris/data")
         (steam-dest ".local/share/guix-sandbox-home"))
    (mod
      (name 'game-managers)
      (dependencies
       (list flatpak-mod
             bottles-mod))
      (apply
       (compose
        (mod-he-packages
         (list steam-custom-wrapped
               sdl2))
        (mod-he-services
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
                                  ,(path-append games-src
                                                "factorio/files")
                                  ".factorio")
                                 ;; tModLoader
                                 (,(path-append steam-dest
                                                ".local/share/Terraria"
                                                "tModLoader")
                                  ,(path-append games-src "tmodloader/files")
                                  "Players/Backups"
                                  "Worlds/Backups"
                                  "Captures")))
               (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "net.lutris.Lutris")
                                 (flathub "net.davidotek.pupgui2")
                                 (flathub "com.github.Matoking.protontricks"))))))))))

(define minecraft-mod
  (mod
    (name 'minecraft)
    (dependencies
     (list flatpak-mod))
    (apply
     (compose
      (mod-he-packages
       (list glfw-wayland-minecraft))
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "org.prismlauncher.PrismLauncher")))))))))

(define minetest-mod
  (mod
    (name 'minetest)
    (apply
     (compose (mod-he-packages
               (list minetest))))))

(define syncplay-mod
  (mod
    (name 'syncplay)
    (apply
     (compose (mod-he-packages
               (list syncplay))))))

(define entertainment-mods
  (list game-managers-mod
        minecraft-mod
        minetest-mod
        syncplay-mod))
