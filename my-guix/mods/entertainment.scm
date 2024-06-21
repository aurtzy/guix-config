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

(define-module (my-guix mods entertainment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods desktop)
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

(use-service-modules sysctl)

(define games-src
  (path-append-my-home "areas/games"))

(define game-managers-mod
  (let* ((lutris-dest ".var/app/net.lutris.Lutris/data")
         (steam-dest ".local/share/guix-sandbox-home"))
    (mod
      (name 'game-managers)
      (os-extension
       (mod-os-service
        sysctl-service-type
        (lambda (config)
          (sysctl-configuration
           (inherit config)
           ;; Copied value from Arch Linux (and context given in link):
           ;; https://archlinux.org/news/increasing-the-default-vmmax_map_count-value/
           (settings (cons* '("vm.max_map_count" . "1048576")
                            (sysctl-configuration-settings config)))))))
      (he-extension
       (compose
        (mod-he-packages
         (list steam-custom-wrapped
               sdl2))
        (mod-he-services
         (list (simple-service name
                               home-impure-symlinks-service-type
                               `( ;; Flatpak overrides
                                 (".local/share/flatpak/overrides"
                                  ,(path-append-my-files "lutris/impure")
                                  "net.lutris.Lutris")
                                 (".local/share/flatpak/overrides"
                                  ,(path-append-my-files "steam/impure")
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
    (he-extension
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
    (he-extension
     (compose (mod-he-packages
               (list minetest))))))

(define syncplay-mod
  (mod
    (name 'syncplay)
    (he-extension
     (compose (mod-he-packages
               (list syncplay))))))

(define entertainment-mods
  (list game-managers-mod
        minecraft-mod
        minetest-mod
        syncplay-mod))
