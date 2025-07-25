;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages gnome)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public adwaita-icon-theme-legacy
  ;; Copied from adwaita-icon-theme@46.2.
  (package
    (name "adwaita-icon-theme-legacy")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "1d1gyacqy7rf9vbljwhqwdkxbyszn5avfcw8s5r4p9c9hpsq112l"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-meson
                 ;; Don't create 'icon-theme.cache'.
                 (lambda _ (substitute* "meson.build"
                             (("gtk4?-update-icon-cache") "true")))))))
    (home-page "https://gitlab.gnome.org/GNOME/adwaita-icon-theme")
    (synopsis "Legacy GNOME icon theme")
    (description "Legacy Adwaita icon theme.  This provides GNOME's
\"old-style\" fullcolor icons, which may be used by apps conforming to the
Freedesktop Icon Naming Specification.")
    (license license:lgpl3)))
