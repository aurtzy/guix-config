;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module provides extra extensions that are also commonly used on
;; machines, but more likely to be individually picked on a case-by-case
;; basis.

(define-module (my-guix home extensions extras)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions common)
  #:use-module (my-guix home services package-management)
  #:export (newsreader-extension
            creative-extension
            office-extension
            personal-comms-extension
            programming-extension

            extras-extensions))

(use-package-modules libreoffice
                     emacs-xyz
                     python-xyz)

(define newsreaders-extension
  (extension
    (name 'newsreaders-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-flatpak-profile-service-type
                               '(("org.mozilla.Thunderbird" . flathub)
                                 ("com.gitlab.newsflash" . flathub))))))))))

(define creative-extension
  (extension
    (name 'creative-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-flatpak-profile-service-type
                               '(("fr.handbrake.ghb" . flathub)
                                 ("org.kde.krita" . flathub))))))))))

(define office-extension
  (extension
    (name 'office-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list libreoffice)))))))

(define personal-comms-extension
  (extension
    (name 'personal-comms-extension)
    (apply
     (extender home-environment
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-stow-service-type
                               (list "soundux"))
               (simple-service name
                               home-flatpak-profile-service-type
                               '(("in.cinny.Cinny" . flathub)
                                 ("io.github.Soundux" . flathub))))))))))

;; TODO unsure if this should be an extension or manifests to shell into for
;; use
(define programming-extension
  (extension
    (name 'programming-extension)
    (dependencies
     (list emacs-extension))
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list python-lsp-server
               emacs-ccls)))))))

(define extras-extensions
  (list newsreaders-extension
        creative-extension
        office-extension
        personal-comms-extension
        programming-extension))
