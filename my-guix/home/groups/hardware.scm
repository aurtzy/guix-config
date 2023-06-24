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
;; This module defines hardware-specific groups.

(define-module (my-guix home groups hardware)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services)
  #:use-module (my-guix packages keyboard-center))

(define-group pipewire
  (stow-service "pipewire")
  (flatpak-service 'flathub
                   (list "com.github.wwmm.easyeffects")))

(define-group keyboard-center
  (manifest-service (list keyboard-center)))
