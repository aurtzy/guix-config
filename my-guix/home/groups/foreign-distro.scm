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
;; This module defines groups for use in foreign distributions.

(define-module (my-guix home groups foreign-distro)
  #:use-module (gnu home services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu services)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services))

(define-group foreign-distro
  (manifest-service (list glibc-locales
		          nss-certs))
  ;; TODO figure out why this hack works; apps can find adwaita
  ;; cursors but not others (e.g. breeze_cursors)
  (simple-service home-environment-variables-service-type
                  '(("XCURSOR_PATH" . "/usr/share/icons"))))
