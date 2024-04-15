;;; Copyright © 2022 Samuel Culpepper
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages xorg)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (my-guix utils))

(define-public libdecor
  (package
    (name "libdecor")
    (version "0.2.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://gitlab.freedesktop.org/libdecor/libdecor")
                   (recursive? #t)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "05rxchwzhnkm91kcr30mavizkp25wgjlhb6lcraa456pw7vgb04q"))))
    (build-system meson-build-system)
    (native-inputs (list cmake
                         pkg-config))
    (inputs (list cairo
                  dbus
                  egl-wayland
                  gtk+
                  libglvnd
                  libxkbcommon
                  pango
                  wayland
                  wayland-protocols))
    (home-page "https://gitlab.freedesktop.org/libdecor/libdecor")
    (synopsis "Client-side decorations library for Wayland clients")
    (description "libdecor is a library that can help Wayland clients draw
window decorations for them. It aims to provide multiple backends that
implements the decoration drawing.")
    (license license:expat)))
