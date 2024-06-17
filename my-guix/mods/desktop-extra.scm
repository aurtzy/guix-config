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
;;; This module provides extra mods that are commonly used on desktop machines,
;;; but more likely to be individually included on a case-by-case basis.

(define-module (my-guix mods desktop-extra)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix config)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
  #:export (newsreader-mod
            creative-mod
            office-mod
            personal-comms-mod
            programming-mod

            extra-mods))

(use-package-modules libreoffice)

(define newsreaders-mod
  (mod
    (name 'newsreaders)
    (dependencies
     (list flatpak-mod))
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "org.mozilla.Thunderbird")
                               (flathub "org.kde.akregator")))
             (simple-service name
                             home-impure-symlinks-service-type
                             `((".local/share/flatpak/overrides"
                                ,(path-append-my-files "akregator")
                                "org.kde.akregator")
                               (".var/app/org.kde.akregator/data/akregator/data"
                                ,(path-append-my-home "areas/feeds")
                                "feeds.opml")))))))))

(define creative-mod
  (mod
    (name 'creative)
    (dependencies
     (list flatpak-mod))
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "org.kde.kdenlive")
                               (flathub "fr.handbrake.ghb")
                               (flathub "org.kde.krita")))))))))

(define office-mod
  (mod
    (name 'office)
    (he-extension
     (compose (mod-he-packages
               (list libreoffice))))))

(define personal-comms-mod
  (mod
    (name 'personal-comms)
    (dependencies
     (list flatpak-mod))
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-impure-symlinks-service-type
                             `((".local/share/flatpak/overrides"
                                ,(path-append-my-files "soundux/impure")
                                "io.github.Soundux")))
             (simple-service name
                             home-flatpak-profile-service-type
                             '((flathub "in.cinny.Cinny")
                               (flathub "io.github.Soundux")))))))))

(define programming-mod
  (mod
    (name 'programming)
    (dependencies
     (list emacs-mod))
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-impure-symlinks-service-type
                             `((""
                                ,(path-append-my-files)
                                "manifests")))))))))

(define extra-mods
  (list newsreaders-mod
        creative-mod
        office-mod
        personal-comms-mod
        programming-mod))
