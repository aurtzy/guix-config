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
;;; This module defines home services for non-Guix package management.

(define-module (my-guix home services package-management)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (<home-flatpak-configuration>
            home-flatpak-configuration
            home-flatpak-configuration?
            home-flatpak-configuration-flatpak
            home-flatpak-configuration-remotes
            home-flatpak-configuration-profile

            home-flatpak-service-type
            home-flatpak-profile-service-type))

;; TODO: learn and use define-configuration here which should improve
;; extensibility, but this will suffice for now albeit without the ability to
;; extend flatpak remotes

(define-record-type* <home-flatpak-configuration>
  home-flatpak-configuration make-home-flatpak-configuration
  home-flatpak-configuration?
  ;; flatpak package
  (flatpak home-flatpak-configuration-flatpak
           (default flatpak))
  ;; Alist of remote name symbol to remote URL string
  (remotes home-flatpak-configuration-remotes
           (default '()))
  ;; List of flatpaks, each of which should be a list consisting of the remote
  ;; as the first element and the application ID as the second.
  ;; TODO: this could eventually be a list of flatpak definitions, potentially
  ;; records with information about remote name, overrides and such - similar
  ;; to package definitions
  (profile home-flatpak-configuration-profile
           (default '())))

(define (home-flatpak-packages config)
  "Add flatpak package to profile."
  (let ((flatpak (home-flatpak-configuration-flatpak config)))
    (list flatpak)))

(define (home-flatpak-profile-installer config)
  "Gexp to add flatpak remotes and install packages."
  ;; TODO: check if all packages use valid remote names before proceeding and
  ;; deduplicate
  ;; 
  ;; XXX: This service depends on SSL_CERT_FILE pointing to the CA certificates
  ;; file, which is not possible on foreign systems without an initial
  ;; reconfigure.  This file is special in that it is dynamically generated
  ;; rather than built as part of a package (see `ca-certificates-bundle' in
  ;; (guix profiles)), so there does not seem to be a way to insert it into this
  ;; g-exp.
  #~(unless #$(getenv "GUIX_FLATPAK_DISABLE")
      (let ((flatpak (string-append
                      #$(home-flatpak-configuration-flatpak config)
                      "/bin/flatpak"))
            (remotes '#$(home-flatpak-configuration-remotes config))
            (profile '#$(home-flatpak-configuration-profile config)))
        ;; Configure remotes first
        (for-each
         (lambda (remote)
           (let ((remote-name (symbol->string (car remote)))
                 (remote-url (cdr remote)))
             (call-with-port (%make-void-port "w")
               (lambda (port)
                 (with-error-to-port port
                   (lambda ()
                     (system* flatpak
                              "--user"
                              "remote-delete"
                              "--force"
                              remote-name)))))
             (invoke flatpak
                     "--user"
                     "remote-add"
                     remote-name
                     remote-url)))
         remotes)
        ;; Install/update applications in profile
        (for-each
         (lambda (flatpak-app)
           (let ((remote-name (car flatpak-app))
                 (app-id (cadr flatpak-app)))
             (invoke flatpak
                     "--user"
                     "install"
                     "--or-update"
                     "--noninteractive"
                     (symbol->string remote-name)
                     app-id)))
         profile)
        ;; Update any remaining applications
        (invoke flatpak
                "--user"
                "update"
                "--noninteractive"))))

;; TODO: see todo comment at top; this should be able to extend remotes too,
;; likely by changing profile-extensions to be a general extensions argument,
;; which makes the home-flatpak-profile-service-type have an actual purpose
(define (home-flatpak-extend config profile-extensions)
  (home-flatpak-configuration
   (inherit config)
   (profile
    (append (home-flatpak-configuration-profile config)
            profile-extensions))))

(define home-flatpak-service-type
  (service-type (name 'home-flatpak)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-flatpak-packages)
                       (service-extension
                        home-activation-service-type
                        home-flatpak-profile-installer)))
                (compose concatenate)
                (extend home-flatpak-extend)
                (description "Install and configure Flatpak applications.")
                (default-value (home-flatpak-configuration))))

(define home-flatpak-profile-service-type
  (service-type (name 'home-flatpak-profile)
                (extensions
                 (list (service-extension
                        home-flatpak-service-type
                        ;; TODO: this will need to be changed to provide the
                        ;; profile wrapped by a home-flatpak-extension when
                        ;; flatpak refactor takes place
                        identity)))
                (compose concatenate)
                (extend append)
                (description
                 "Add additional Flatpak applications to profile.")
                (default-value '())))
