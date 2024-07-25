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
;;; This module defines mods that set up desktop environments.

(define-module (my-guix mods desktop-environment)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix utils)
  #:export (gnome-mod
            plasma-mod
            wayland-mod))

(use-package-modules freedesktop gnome gnome-xyz image kde kde-frameworks
                     kde-multimedia kde-plasma kde-utils qt)

(use-service-modules desktop sddm xorg)

(define wayland-mod
  (mod
    (name 'wayland)
    (description
     "Configures environment for usage on Wayland compositors.")
    (he-extension
     (compose
      (mod-he-services
       (list (simple-service name
                             home-bash-service-type
                             (home-bash-extension
                              (environment-variables
                               '(("MOZ_ENABLE_WAYLAND" . "1")))))))))))

(define gnome-mod
  (mod
    (inherit wayland-mod)
    (name 'gnome)
    (description
     "Provides configurations for the GNOME desktop environment.")
    (os-extension
     (compose-lambda (os)
       (let ((replace-mesa (replace-mesa))
             (nvidia-proprietary? (nvidia-proprietary?)))
         (list
          (mod-os-extension wayland-mod)
          (mod-os-packages
           (map replace-mesa
                (list gvfs
                      gnome-tweaks
                      gnome-shell-extensions
                      gnome-shell-extension-gsconnect
                      xdg-desktop-portal-kde)))
          (mod-os-services
           (list (if nvidia-proprietary?
                     (set-xorg-configuration
                      (xorg-configuration
                       (keyboard-layout (operating-system-keyboard-layout os))
                       (modules (cons (module-ref (resolve-interface
                                                   '(nongnu packages nvidia))
                                                  'nvda)
                                      %default-xorg-modules))
                       (drivers '("nvidia"))))
                     (set-xorg-configuration
                      (xorg-configuration
                       (keyboard-layout (operating-system-keyboard-layout os)))))
                 (service gnome-desktop-service-type
                          (gnome-desktop-configuration
                           (core-services
                            (list (replace-mesa gnome-meta-core-services)))
                           (shell
                            (list (replace-mesa gnome-meta-core-shell)))
                           (utilities
                            (list (replace-mesa gnome-meta-core-utilities)))
                           (extra-packages
                            (list (replace-mesa gnome-essential-extras)))))
                 (service gdm-service-type)))))))))

(define plasma-mod
  (mod
    (inherit wayland-mod)
    (name 'plasma)
    (description
     "Configures the KDE Plasma desktop environment for this system.")
    (os-extension
     (compose-lambda (os)
       (let ((replace-mesa (replace-mesa))
             (nvidia-proprietary? (nvidia-proprietary?)))
         (list
          (mod-os-extension wayland-mod)
          (mod-os-packages
           (map replace-mesa
                ;; Numerous packages are noted in the packaging recommendations
                ;; wiki page:
                ;; https://community.kde.org/Distributions/Packaging_Recommendations
                (list ark
                      ffmpegthumbs
                      gnome-tweaks      ;Manage GTK application themes with this
                      gwenview
                      icoutils
                      kdeconnect
                      kimageformats
                      okular
                      qtimageformats
                      xdg-desktop-portal-gtk)))
          (mod-os-services
           (list (service plasma-desktop-service-type
                          (plasma-desktop-configuration
                           (plasma-package (replace-mesa plasma))))
                 (service sddm-service-type
                          (sddm-configuration
                           (xorg-configuration
                            (if nvidia-proprietary?
                                (xorg-configuration
                                 (keyboard-layout
                                  (operating-system-keyboard-layout os))
                                 (modules
                                  (cons (module-ref (resolve-interface
                                                     '(nongnu packages nvidia))
                                                    'nvda)
                                        %default-xorg-modules))
                                 (drivers '("nvidia")))
                                (xorg-configuration
                                 (keyboard-layout
                                  (operating-system-keyboard-layout os)))))))))))))))
