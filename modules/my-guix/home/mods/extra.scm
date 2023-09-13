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
;;; This module provides extra mods that are also commonly used on machines,
;;; but more likely to be individually picked on a case-by-case basis.

(define-module (my-guix home mods extra)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix config)
  #:use-module (my-guix mods)
  #:use-module (my-guix home mods common)
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
    (name 'newsreaders-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "org.mozilla.Thunderbird")
                                (flathub "org.kde.akregator")))
              ;; Disable GPU access to fix NVIDIA Wayland issues
              ;; See: https://bugs.kde.org/show_bug.cgi?id=466124
              (simple-service name
                              home-impure-symlinks-service-type
                              `((".local/share/flatpak/overrides"
                                 ,(search-files-path
                                   "impure/akregator")
                                 "org.kde.akregator")
                                (".var/app/org.kde.akregator/data/akregator/data"
                                 ,(string-append
                                   (getenv "HOME")
                                   "/areas/feeds")
                                 "feeds.opml")))))))))

(define creative-mod
  (mod
    (name 'creative-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "fr.handbrake.ghb")
                                (flathub "org.kde.krita")))))))))

(define office-mod
  (mod
    (name 'office-mod)
    (apply
     (apply-mod home-environment
       (packages
        home-environment-packages
        append=>
        (list libreoffice))))))

(define personal-comms-mod
  (mod
    (name 'personal-comms-mod)
    (dependencies
     (list flatpak-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((".local/share/flatpak/overrides"
                                 ,(search-files-path
                                   "impure/soundux")
                                 "io.github.Soundux")))
              (simple-service name
                              home-flatpak-profile-service-type
                              '((flathub "in.cinny.Cinny")
                                (flathub "io.github.Soundux")))))))))

;; TODO experimental; see how this fares
(define programming-mod
  (mod
    (name 'programming-mod)
    (dependencies
     (list emacs-mod))
    (apply
     (apply-mod home-environment
       (services
        home-environment-user-services
        append=>
        (list (simple-service name
                              home-impure-symlinks-service-type
                              `((""
                                 ,(search-files-path)
                                 "manifests")))))))))

(define extra-mods
  (list newsreaders-mod
        creative-mod
        office-mod
        personal-comms-mod
        programming-mod))
