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
;;; This module provides extra extensions that are also commonly used on
;;; machines, but more likely to be individually picked on a case-by-case
;;; basis.

(define-module (my-guix home extensions extras)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions common)
  #:use-module (my-guix home services)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix utils)
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
                               '((flathub "org.mozilla.Thunderbird")
                                 (flathub "org.kde.akregator")))
               ;; Disable GPU access to fix NVIDIA Wayland issues
               ;; See: https://bugs.kde.org/show_bug.cgi?id=466124
               (simple-service name
                               home-impure-symlinks-service-type
                               `((".local/share/flatpak/overrides"
                                  ,(search-files-path
                                    "impure/akregator")
                                  "org.kde.akregator"))))))))))

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
                               '((flathub "fr.handbrake.ghb")
                                 (flathub "org.kde.krita"))))))))))

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
                               home-impure-symlinks-service-type
                               `((".local/share/flatpak/overrides"
                                  ,(search-files-path
                                    "impure/soundux")
                                  "io.github.Soundux")))
               (simple-service name
                               home-flatpak-profile-service-type
                               '((flathub "in.cinny.Cinny")
                                 (flathub "io.github.Soundux"))))))))))

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
