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
;; This module defines common groups that nearly any system type would find use.

(define-module (my-guix home groups common)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages music)
  #:use-module (gnu packages web)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (my-guix config)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services))

(define-group akregator
  (flatpak-service 'flathub
                   (list "org.kde.akregator")))

(define-group brave
  (stow-service "brave")
  (flatpak-service 'flathub
                   (list "com.brave.Browser")))

(define-group breeze-theme
  (manifest-service (list breeze
                          breeze-icons)))

(define-group environment
  (stow-service "environment"))

(define-group handbrake
  (flatpak-service 'flathub
                   (list "fr.handbrake.ghb")))

(define-group krita
  (flatpak-service 'flathub
                   (list "org.kde.krita")))

(define-group libreoffice
  (manifest-service (list libreoffice)))

(define-group matrix
  (flatpak-service 'flathub
                   (list "in.cinny.Cinny")))

(define-group mpv
  (manifest-service (list mpv)))

(define-group qalculate!
  (flatpak-service 'flathub
                   (list "io.github.Qalculate.qalculate-qt")))

(define-group quodlibet
  (manifest-service (list quodlibet)))

(define-group soundux
  (stow-service "soundux")
  (flatpak-service 'flathub
                   (list "io.github.Soundux")))

(define-group thunderbird
  (flatpak-service 'flathub
                   (list "org.mozilla.Thunderbird")))

(define-group tor-browser
  (flatpak-service 'flathub
                   (list "com.github.micahflee.torbrowser-launcher")))

(define-group web-server
  (manifest-service (list darkhttpd)))

(define-group yt-dlp
  (manifest-service (list yt-dlp)))

(define-public common-groups
  (append akregator-group
          brave-group
          handbrake-group
          krita-group
          libreoffice-group
          matrix-group
          mpv-group
          qalculate!-group
          ;; broken
          ;; quodlibet-group
          soundux-group
          thunderbird-group
          tor-browser-group
          web-server-group
          yt-dlp-group))
