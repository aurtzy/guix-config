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
;; This module provides extensions targeted to foreign distributions using
;; Guix.

(define-module (my-guix home extensions foreign)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home services package-management)
  #:export (foreign-extension))

(use-package-modules base
                     certs)

(define foreign-extension
  (extension
    (name 'foreign-extension)
    (apply
     (extender home-environment
       (packages
        (modify-list
         home-environment-packages
         (list nss-certs
               glibc-locales)))
       (services
        (modify-list
         home-environment-user-services
         (list (simple-service name
                               home-environment-variables-service-type
                               '(("SSL_CERT_DIR"
                                  . "$HOME/.guix-home/profile/etc/ssl/certs")
                                 ("SSL_CERT_FILE"
                                  . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
                                 ;; TODO figure out how this hack with
                                 ;; XCURSOR_PATH works; apps can find adwaita
                                 ;; cursors but not others
                                 ;; (e.g. breeze_cursors)
                                 ("XCURSOR_PATH"
                                  . "/usr/share/icons"))))))))))
