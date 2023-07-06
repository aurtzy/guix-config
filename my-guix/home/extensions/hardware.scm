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
;; This module defines hardware-specific extensions.

(define-module (my-guix home extensions hardware)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home services))

(define-public pipewire-extension
  (extension
    (name "pipewire")
    (configuration
     (extender home-environment
         env =>
       (services
        (cons* (stow-service 'stow-pipewire "pipewire")
               (flatpak-service 'pipewire-flatpaks
                                'flathub
                                '("com.github.wwmm.easyeffects"))
               (home-environment-user-services env)))))))
