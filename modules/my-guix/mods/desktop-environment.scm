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
;;; This module defines mods that set up desktop environments.

(define-module (my-guix mods desktop-environment)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix packages gnome)
  #:use-module (my-guix utils)
  #:use-module (nonguix utils)
  #:export (gnome-mod
            plasma-mod))

(use-package-modules freedesktop gnome gnome-xyz image kde-frameworks
                     kde-graphics kde-internet kde-multimedia kde-plasma
                     kde-utils qt)

(use-service-modules desktop sddm xorg)

(define gnome-mod
  (operating-system-mod
    (name 'gnome)
    (description
     "Provides configurations for the GNOME desktop environment.")
    (services
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((base-configuration base-configuration-argument)
          (replace-mesa replace-mesa-argument)
          (nvidia-proprietary? nvidia-proprietary?-argument))
       (with-transformation
        replace-mesa
        (append
         ;; TODO: Fix GDM not using of Wayland on nonfree drivers.  We
         ;; use SDDM instead for now as a hack, which does not have
         ;; this issue.
         ;; (set-xorg-configuration
         ;;  (xorg-configuration
         ;;   (keyboard-layout (operating-system-keyboard-layout os))
         ;;   (modules (cons (module-ref (resolve-interface
         ;;                               '(nongnu packages nvidia))
         ;;                              'nvda)
         ;;                  %default-xorg-modules))
         ;;   (drivers '("nvidia"))))
         (if nvidia-proprietary?
             (list
              (service sddm-service-type
                       (sddm-configuration
                         (xorg-configuration
                           (if nvidia-proprietary?
                               (xorg-configuration
                                 (keyboard-layout
                                  (operating-system-keyboard-layout
                                   base-configuration))
                                 (modules
                                  (cons (module-ref (resolve-interface
                                                     '(nongnu packages nvidia))
                                                    'nvda)
                                        %default-xorg-modules))
                                 (drivers '("nvidia")))
                               (xorg-configuration
                                 (keyboard-layout
                                  (operating-system-keyboard-layout
                                   base-configuration))))))))
             (list
              (set-xorg-configuration
               (xorg-configuration
                 (keyboard-layout (operating-system-keyboard-layout
                                   base-configuration))))
              (service gdm-service-type)))
         (list (service gnome-desktop-service-type
                        (gnome-desktop-configuration
                          (shell
                           (let ((transform
                                  (options->transformation
                                   ;; HACK: Tests currently fail.
                                   `((without-tests . "gnome-color-manager")))))
                             (list (transform gnome-meta-core-shell))))
                          (extra-packages
                           (list adwaita-icon-theme-legacy
                                 gnome-essential-extras
                                 gnome-shell-extensions
                                 gnome-shell-extension-gsconnect
                                 gnome-tweaks
                                 xdg-desktop-portal-kde)))))))))))

(define plasma-mod
  (operating-system-mod
    (name 'plasma)
    (description
     "Configures the KDE Plasma desktop environment for this system.")
    (packages
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((replace-mesa replace-mesa-argument)
          (nvidia-proprietary? nvidia-proprietary?-argument))
       (map replace-mesa
            ;; Numerous packages here are included by recommendation of the
            ;; wiki page on packaging, although I'm not sure what some of
            ;; them do:
            ;; https://community.kde.org/Distributions/Packaging_Recommendations
            (list ark
                  bluedevil
                  bluez-qt      ;XXX: Propagate for bluedevil settings to work
                  ffmpegthumbs
                  filelight
                  gnome-tweaks
                  gwenview
                  icoutils
                  kdeconnect
                  kded    ;XXX: Fix audio/tray(/more?) integration being funky
                  kimageformats
                  kwayland-integration
                  kwrited
                  okular
                  print-manager
                  qtimageformats
                  system-config-printer
                  xdg-desktop-portal-gtk))))
    (services
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((base-configuration base-configuration-argument)
          (replace-mesa replace-mesa-argument)
          (nvidia-proprietary? nvidia-proprietary?-argument))
       (list (service plasma-desktop-service-type
                      (plasma-desktop-configuration
                        (plasma-package (replace-mesa plasma))))
             (service sddm-service-type
                      (sddm-configuration
                        (xorg-configuration
                          (if nvidia-proprietary?
                              (xorg-configuration
                                (keyboard-layout
                                 (operating-system-keyboard-layout
                                  base-configuration))
                                (modules
                                 (cons (module-ref (resolve-interface
                                                    '(nongnu packages nvidia))
                                                   'nvda)
                                       %default-xorg-modules))
                                (drivers '("nvidia")))
                              (xorg-configuration
                                (keyboard-layout
                                 (operating-system-keyboard-layout
                                  base-configuration)))))
                        ;; FIXME: Time displays are blank when this is used
                        ;; (auto-login-user "alvin")
                        )))))))
