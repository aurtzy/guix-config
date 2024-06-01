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
;;; This module provides a base desktop environments for foreign systems.

(define-module (my-guix base foreign-desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (my-guix base desktop)
  #:export (base-foreign-desktop-home-environment))

(use-package-modules base certs ssh)

(define base-foreign-desktop-home-environment
  (let ((env base-desktop-home-environment))
    (home-environment
     (inherit env)
     (packages
      (cons* nss-certs
             glibc-locales
             ;; Use host's ssh
             (delq openssh
                   (home-environment-packages env))))
     (services
      (cons*
       (simple-service 'base-foreign-desktop-home-environment-variables
                       home-environment-variables-service-type
                       '(("SSL_CERT_DIR"
                          . "$HOME/.guix-home/profile/etc/ssl/certs")
                         ("SSL_CERT_FILE"
                          . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
                         ;; TODO figure out how this hack with XCURSOR_PATH
                         ;; works; apps can find adwaita cursors but not
                         ;; others (e.g. breeze_cursors)
                         ("XCURSOR_PATH"
                          . "/usr/share/icons")))
       (home-environment-user-services env))))))
