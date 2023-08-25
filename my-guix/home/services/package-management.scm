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
;; This module defines home services for non-Guix package management.

(define-module (my-guix home services package-management)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:export (<home-flatpak-configuration>
            home-flatpak-configuration
            home-flatpak-configuration?
            home-flatpak-configuration-flatpak
            home-flatpak-configuration-remotes
            home-flatpak-configuration-profile

            home-flatpak-service-type
            home-flatpak-profile-service-type

            <home-stow-configuration>
            home-stow-configuration
            home-stow-configuration?
            home-stow-configuration-stow
            home-stow-configuration-profile

            home-stow-service-type))

;; TODO learn and use define-configuration here which should improve
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
  ;; TODO this could eventually be a list of flatpak definitions, potentially
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
  ;; TODO check if all packages use valid remote names before proceeding and
  ;; deduplicate
  ;; 
  ;; TODO this service only works on foreign distros when the SSL_CERT_FILE
  ;; env var is set properly, which requires at least one run beforehand for
  ;; Guix Home to set it if handling outside. Can this env var be somehow
  ;; included here (without hardcoding)?
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
             ;; TODO fix case where remote does not exist, causing the
             ;; following command to fail
             (invoke flatpak
                     "--user"
                     "remote-delete"
                     "--force"
                     remote-name)
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
                "update"
                "--noninteractive"))))

;; TODO see todo comment at top; this should be able to extend remotes too,
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
                        ;; TODO this will need to be changed to provide the
                        ;; profile wrapped by a home-flatpak-extension when
                        ;; flatpak refactor takes place
                        identity)))
                (compose concatenate)
                (extend append)
                (description
                 "Add additional Flatpak applications to profile.")
                (default-value '())))

(define-record-type* <home-stow-configuration>
  home-stow-configuration make-home-stow-configuration
  home-stow-configuration?
  (stow home-stow-configuration-stow
        (default stow))
  (profile home-stow-configuration-profile
           (default '())))

(define (home-stow-packages config)
  "Add flatpak package to profile."
  (list (home-stow-configuration-stow config)))

(define (home-stow-profile-installer config)
  "Gexp to stow packages."
  (let ((stow (home-stow-configuration-stow config)))
    #~(let ((stow (string-append
                   #$stow
                   "/bin/stow"))
            (packages '#$(home-stow-configuration-profile config)))
        (apply invoke
               stow
               "--no-folding"
               (string-append
                "--dir=" #$(search-files-path "stow"))
               (string-append
                "--target=" (getenv "HOME"))
               "--restow"
               packages))))

(define (home-stow-extend config profile)
  (home-stow-configuration
   (inherit config)
   (profile
    (append profile
            (home-stow-configuration-profile config)))))

;; TODO
;; This service is deprecated in favor of home-impure-symlinks-service-type.
(define home-stow-service-type
  (service-type (name 'home-stow)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-stow-packages)
                       (service-extension
                        home-activation-service-type
                        home-stow-profile-installer)))
                (compose concatenate)
                (extend home-stow-extend)
                (description "Stow packages to the $HOME directory.")
                (default-value (home-stow-configuration))))
