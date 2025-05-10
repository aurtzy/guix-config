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
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix utils)
  #:export (game-mangers-mod
            minecraft-mod
            minetest-mod
            syncplay-mod

            entertainment-mods))

(use-package-modules freedesktop minetest sdl video)

(use-service-modules sysctl)

(define games-src
  (path-append-my-static-assets-directory "games"))

(define steam-extra-shares `( ;; Work around steam needing access to files when
                              ;; uploading screenshots/pictures to chat (portal
                              ;; doesn't seem to apply here..?).
                             "$HOME/Pictures/Screenshots"
                             ,games-src
                             "$HOME/Games"
                             "$HOME/storage/steam-alt-library"
                             "$HOME/.config/r2modmanPlus-local"))

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
         (list steam-custom sdl2))
        (mod-he-services
         (list (simple-service name
                               home-impure-symlinks-service-type
                               `( ;; Mindustry
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
                               home-files-service-type
                               `((".local/share/flatpak/overrides/net.lutris.Lutris"
                                  ,(mixed-text-file "net.lutris.Lutris" "\
[Context]
filesystems=" (path-append-my-static-assets-directory "games") ";!home;~/.guix-home;~/Games
"))))
               (simple-service name
                               home-files-service-type
                               `((".local/share/flatpak/overrides/com.valvesoftware.Steam"
                                  ,(mixed-text-file "com.valvesoftware.Steam" "\
[Context]
filesystems=" (path-append-my-static-assets-directory "games") ";~/storage/steam-alt-library;~/Games
"))))
               (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "net.lutris.Lutris")
                                 (flathub "net.davidotek.pupgui2")
                                 (flathub "com.github.Matoking.protontricks")))
               (simple-service name
                               home-environment-variables-service-type
                               `(("GUIX_SANDBOX_EXTRA_SHARES"
                                  .
                                  ,(string-join steam-extra-shares ":"))))
               (simple-service name
                               home-bash-service-type
                               (home-bash-extension
                                (aliases
                                 ;; Set aliases for steam-with-NVK and
                                 ;; steam-with-proprietry driver for
                                 ;; convenience.
                                 '(("steam-custom"
                                    . "\
gamescope --backend sdl -w 1280 -h 720 -W 1280 -H 720 --force-grab-cursor --fullscreen -- \\
	guix shell steam-custom -- steam")
                                   ("steam-nvidia"
                                    . "\
gamescope -w 2560 -h 1440 -W 2560 -H 1440 --force-grab-cursor --fullscreen -- \\
	guix shell steam-nvidia -- steam"))))))))))))

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

(define videa-mod
  (mod
    (name 'videa)
    (he-extension
     (mod-he-services
      (list (simple-service name
                            home-flatpak-profile-service-type
                            '((flathub "info.febvre.Komikku")))
            (simple-service name
                            home-files-service-type
                            `((".local/share/flatpak/overrides/info.febvre.Komikku"
                               ,(mixed-text-file "info.febvre.Komikku" "\
[Context]
filesystems=" (path-append-my-static-assets-directory "komikku" "komikku.db") "
"))))
            (simple-service name
                            home-impure-symlinks-service-type
                            `((".var/app/info.febvre.Komikku/data"
                               ,(path-append-my-static-assets-directory
                                 "komikku")
                               "komikku.db"))))))))

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
        syncplay-mod
        videa-mod))
