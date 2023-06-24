;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module defines entertainment-related groups.

(define-module (my-guix home groups entertainment)
  #:use-module (gnu packages minetest)
  #:use-module (gnu packages sdl)
  #:use-module (gnu services)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services)
  #:use-module (my-guix packages minecraft-wayland)
  #:use-module (my-guix packages syncplay))

;; note: must disallow access to home because of weird pthread library issue
;; with Guix (e.g. https://logs.guix.gnu.org/guix/2023-01-17.log#124520)
(define-group lutris
  (flatpak-service 'flathub
                   (list "net.lutris.Lutris"))
  (stow-service "lutris"))

(define-group protonup-qt
  (flatpak-service 'flathub
                   (list "net.davidotek.pupgui2")))

(define-group steam
  (manifest-service (list sdl2))
  (flatpak-service 'flathub
                   (list "com.valvesoftware.Steam"))
  (stow-service "steam"))

(define-public game-manager-groups
  (append lutris-group
          protonup-qt-group
          steam-group))

(define-group minecraft
  (manifest-service (list glfw-wayland-minecraft))
  (flatpak-service 'flathub
                   (list "org.prismlauncher.PrismLauncher")))

(define-group minetest
  (manifest-service (list minetest)))

(define-group syncplay
  (manifest-service (list syncplay)))
