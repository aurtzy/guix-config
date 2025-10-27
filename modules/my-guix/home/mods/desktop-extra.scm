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
;;; This module provides extra desktop-related mods that are commonly used on
;;; machines, but more likely to be individually included on a case-by-case
;;; basis.

(define-module (my-guix home mods desktop-extra)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (my-guix config)
  #:use-module (my-guix home mods)
  #:use-module (my-guix mods)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
  #:export (home-newsreader-mod
            home-creative-mod
            home-office-mod
            home-personal-comms-mod
            home-programming-mod
            home-meta-desktop-extra-mod))

(use-package-modules libreoffice)

(define home-newsreaders-mod
  (home-environment-mod
    (name 'home-newsreaders)
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           '("org.mozilla.Thunderbird" "org.kde.akregator"))
           (simple-service name
                           home-files-service-type
                           `((".local/share/flatpak/overrides/org.kde.akregator"
                              ,(mixed-text-file "org.kde.akregator" "\
[Context]
filesystems=" (path-append-my-assets-directory "akregator" ".static") "
"))))
           (simple-service name
                           home-files-service-type
                           `((".var/app/org.kde.akregator/data/akregator/data/feeds.opml"
                              ,(symlink-to
                                (path-append-my-assets-directory
                                 "akregator" ".static/akregator-feeds.opml")))))))))

(define home-creative-mod
  (home-environment-mod
    (name 'home-creative)
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           '("org.kde.kdenlive" "fr.handbrake.ghb"
                             "org.kde.krita"))))))

(define home-office-mod
  (home-environment-mod
    (name 'home-office)
    (packages (list libreoffice))))

(define home-personal-comms-mod
  (home-environment-mod
    (name 'home-personal-comms)
    (services
     (list (simple-service name
                           home-files-service-type
                           (let ((desktop-file
                                  (string-append (getenv "HOME")
                                                 "/.local/share/flatpak"
                                                 "/exports/share/applications"
                                                 "/im.riot.Riot.desktop")))
                             (if (file-exists? desktop-file)
                                 ;; Force-enable Wayland.
                                 `((".local/share/applications/im.riot.Riot.desktop"
                                    ,(computed-file
                                      "im.riot.Riot.desktop"
                                      (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules ((guix build utils)))
                                            (copy-file #$(local-file desktop-file)
                                                       #$output)
                                            (substitute* #$output
                                              (("Exec=.*/bin/flatpak run.*im\\.riot\\.Riot" exec)
                                               (if (string-contains
                                                    exec "--ozone-platform=")
                                                   exec
                                                   (string-append
                                                    exec " --ozone-platform=wayland")))))))))
                                 '())))
           (simple-service name
                           home-flatpak-profile-service-type
                           (list
                            "in.cinny.Cinny"
                            (flatpak-app
                              (id "im.riot.Riot")
                              (overrides
                               (flatpak-overrides-configuration
                                 ;; Prevent X11 from being used for Element
                                 ;; client.
                                 (sockets '("!x11")))))
                            (flatpak-app
                              (id "io.github.Soundux")
                              (overrides
                               (flatpak-overrides-configuration
                                 (filesystems
                                  `(,(path-append-my-assets-directory "memes")
                                    "~/storage/tmp/audio/etc"))
                                 (environment
                                  ;; Using Zink seems to fix blacked-out
                                  ;; interface issue, so force it on.
                                  '(("NOUVEAU_USE_ZINK" . "1"))))))))))))

(define home-programming-mod
  (home-environment-mod
    (name 'home-programming)
    (services
     (list (simple-service name
                           home-files-service-type
                           `(("manifests"
                              ,(symlink-to (path-append-my-files "manifests")))))))))

(define home-meta-desktop-extra-mod
  (home-environment-mod
    (name 'home-meta-desktop-extra)
    (addons (list home-newsreaders-mod
                  home-creative-mod
                  home-office-mod
                  home-personal-comms-mod
                  home-programming-mod))))
