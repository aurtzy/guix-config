;;; Copyright © 2023-2025 Alvin Hsu <aurtzy@gmail.com>
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
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu system privilege)
  #:use-module (my-guix home mods)
  #:use-module (my-guix mods)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-game-mangers-mod
            home-minecraft-mod
            home-luanti-mod
            home-syncplay-mod
            home-meta-entertainment-mod))

(use-package-modules freedesktop games gl luanti sdl video)

(define games-src
  (path-append-my-assets-directory "games" ".static"))

(define games-extra-shares
  (concatenate
   (map (lambda (file)
          (let ((%file (if (absolute-file-name? file)
                           file
                           (path-append-my-home file))))
            (if (file-exists? %file)
                (list %file)
                '())))
        (list
         ;; Work around steam needing access to files when uploading images to
         ;; chat (portal doesn't seem to apply here..?).
         "Pictures/Screenshots"
         games-src
         "Games"
         "storage/steam-alt-library"
         ".config/r2modmanPlus-local"
         "solid-drive/steam-library"
         ;; Allow access to store so any dependencies can be resolved.
         "/gnu"))))

(define home-game-managers-mod
  (let* ((lutris-append (cut path-append ".var/app/net.lutris.Lutris/data"
                             <...>))
         (steam-append (cut path-append ".local/share/guix-sandbox-home"
                            <...>))
         (games-src-append (cut path-append games-src <...>)))
    (home-environment-mod
      (name 'home-game-managers)
      (packages (list steam-custom sdl2))
      (services
       (list (simple-service name
                             home-files-service-type
                             `( ;; Mindustry
                               (,(lutris-append "Mindustry/saves")
                                ,(symlink-to (games-src-append
                                              "mindustry/files/saves")))
                               (,(lutris-append "Mindustry/settings.bin")
                                ,(symlink-to (games-src-append
                                              "mindustry/files/settings.bin")))
                               ;; Factorio
                               (,(steam-append ".factorio")
                                ,(symlink-to (games-src-append
                                              "factorio/files/.factorio")))
                               ;; tModLoader
                               (,(steam-append ".local/share/Terraria/tModLoader"
                                               "Players/Backups")
                                ,(symlink-to (games-src-append
                                              "tmodloader/files"
                                              "Players/Backups")))
                               (,(steam-append ".local/share/Terraria/tModLoader"
                                               "Worlds/Backups")
                                ,(symlink-to (games-src-append
                                              "tmodloader/files"
                                              "Worlds/Backups")))
                               (,(steam-append ".local/share/Terraria/tModLoader"
                                               "Captures")
                                ,(symlink-to (games-src-append
                                              "tmodloader/files"
                                              "Captures")))))
             (simple-service name
                             home-flatpak-profile-service-type
                             (list
                              (flatpak-app
                                (id "net.lutris.Lutris")
                                (overrides
                                 (flatpak-overrides-configuration
                                   (filesystems
                                    `(,(path-append-my-assets-directory
                                        "games" ".static")
                                      "!home"
                                      "~/.guix-home"
                                      "~/Games")))))
                              (flatpak-app
                                (id "net.davidotek.pupgui2")
                                (overrides
                                 (flatpak-overrides-configuration
                                   (filesystems
                                    '("xdg-data/guix-sandbox-home/.local/share/Steam")))))
                              (flatpak-app
                                (id "com.github.Matoking.protontricks")
                                (overrides
                                 (flatpak-overrides-configuration
                                   (filesystems
                                    '("xdg-data/guix-sandbox-home")))))
                              ;; TODO: Do I still need this?
                              (flatpak-app
                                (id "com.valvesoftware.Steam")
                                (overrides
                                 (flatpak-overrides-configuration
                                   (filesystems
                                    `(,(path-append-my-assets-directory
                                        "games" ".static")
                                      "~/storage/steam-alt-library"
                                      "~/Games")))))))
             (simple-service name
                             home-environment-variables-service-type
                             `(("GUIX_SANDBOX_EXTRA_SHARES"
                                .
                                ,(string-join games-extra-shares ":"))))
             (simple-service name
                             home-bash-service-type
                             (home-bash-extension
                               (aliases
                                ;; Set aliases for running steam in
                                ;; gamescope, for convenience.
                                ;;
                                ;; Use SDL backend to fix an issue with
                                ;; pointer escaping fullscreen onto secondary
                                ;; monitors.
                                '(("steam-720"
                                   . "\
gamescope --backend sdl -w 1280 -h 720 -W 1280 -H 720 -r 144 \\
	--force-grab-cursor --fullscreen -- steam")
                                  ("steam-1440"
                                   . "\
gamescope --backend sdl -w 2560 -h 1440 -W 2560 -H 1440 -r 144 \\
	--force-grab-cursor --fullscreen -- steam"))))))))))

(define home-minecraft-mod
  (home-environment-mod
    (name 'home-minecraft)
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           '("org.prismlauncher.PrismLauncher"))))))

(define home-luanti-mod
  (home-environment-mod
    (name 'home-luanti)
    (packages (list luanti))))

(define home-videa-mod
  (home-environment-mod
    (name 'home-videa)
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           (list
                            (flatpak-app
                              (id "info.febvre.Komikku")
                              (overrides
                               (flatpak-overrides-configuration
                                 (filesystems
                                  `(,(path-append-my-assets-directory
                                      "komikku" ".static/komikku.db")))
                                 (environment
                                  ;; App crashes on startup with Zink, so
                                  ;; disable it.
                                  '(("NOUVEAU_USE_ZINK" . "0"))))))))
           (simple-service name
                           home-files-service-type
                           `((".var/app/info.febvre.Komikku/data/komikku.db"
                              ,(symlink-to
                                (path-append-my-assets-directory
                                 "komikku" ".static/komikku.db")))))))))

(define home-syncplay-mod
  (home-environment-mod
    (name 'home-syncplay)
    (packages (list syncplay))))

(define home-meta-entertainment-mod
  (home-environment-mod
    (name 'home-entertainment)
    (addons (list home-game-managers-mod
                  home-minecraft-mod
                  home-luanti-mod
                  home-videa-mod
                  home-syncplay-mod))))
