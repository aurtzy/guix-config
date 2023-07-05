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
  #:use-module (my-guix extensions)
  #:use-module (my-guix home extensions common)  
  #:use-module (my-guix home services))

(use-package-modules libreoffice
                     emacs-xyz
                     python-xyz)

(define-public newsreaders-extension
  (extension
    (name "newsreaders")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (flatpak-service 'flatpak-newsreaders
                                'flathub
                                '("org.mozilla.Thunderbird"
                                  "com.gitlab.newsflash"))
               (home-environment-user-services env)))))))

(define-public creative-apps-extension
  (extension
    (name "creative-apps")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (flatpak-service 'flatpak-creative-apps
                                'flathub
                                '("fr.handbrake.ghb"
                                  "org.kde.krita"))
               (home-environment-user-services env)))))))

(define-public office-apps-extension
  (extension
    (name "office-apps")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* libreoffice
               (home-environment-packages env)))))))

(define-public personal-comms-extension
  (extension
    (name "personal-comms")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (stow-service 'stow-soundux "soundux")
               (flatpak-service 'flatpak-personal-comms
                                'flathub
                                '("in.cinny.Cinny"
                                  "io.github.Soundux"))
               (home-environment-user-services env)))))))

;; TODO unsure if this should be an extension or manifests to shell into for
;; use
(define-public programming-extension
  (extension
    (name "programming")
    (dependencies
     (list emacs-base-extension))
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* python-lsp-server
               emacs-ccls))))))

(define-public extras-extension
  (extension
    (name "extras")
    (dependencies
     (list newsreaders-extension
           creative-apps-extension
           office-apps-extension
           personal-comms-extension))))
