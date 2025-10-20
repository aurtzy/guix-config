;;; Copyright © 2023, 2025 Alvin Hsu <aurtzy@gmail.com>
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
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-flatpak-configuration
            home-flatpak-configuration?
            home-flatpak-configuration-flatpak
            home-flatpak-configuration-remotes
            home-flatpak-configuration-profile

            home-flatpak-service-type
            home-flatpak-profile-service-type))

(define list-of-remotes?
  (match-lambda
    ((((? string?) (? string?)) ...) #t)
    (else #f)))

(define list-of-flatpak-apps?
  (match-lambda
    ((((? string?) (? string?)) ...) #t)
    (else #f)))

(define-configuration/no-serialization home-flatpak-configuration
  (flatpak (file-like flatpak)
           "The Flatpak package to use.")
  (remotes (list-of-remotes '())
           "A list of remotes.  Each element of the list must be a tuple,
where the first element is the remote name, and the second element is the
associated URL.")
  (profile (list-of-flatpak-apps '())
           "A list of flatpak applications.  Each entry in the list must be a
tuple, with the first element being the remote name, and the second element
being the designated application ID."))

(define (home-flatpak-packages config)
  "Add flatpak package to profile."
  (let ((flatpak (home-flatpak-configuration-flatpak config)))
    (list flatpak)))

(define (home-flatpak-profile-installer config)
  "Gexp to add flatpak remotes and install packages."
  ;; XXX: This service depends on SSL_CERT_FILE pointing to the CA certificates
  ;; file, which is not possible on foreign systems without an initial
  ;; reconfigure.  This file is special in that it is dynamically generated
  ;; rather than built as part of a package (see `ca-certificates-bundle' in
  ;; (guix profiles)), so there does not seem to be a way to insert it into this
  ;; g-exp.
  (define flatpak (home-flatpak-configuration-flatpak config))
  (define remotes (home-flatpak-configuration-remotes config))
  (define profile (delete-duplicates
                   (home-flatpak-configuration-profile config)))

  (for-each
   (lambda (app)
     (match app
       ((remote-name app-id)
        (unless (assoc remote-name remotes)
          (raise-exception
           (make-exception-with-message
            (format #f "Flatpak remote does not exist for entry: ~s" app)))))))
   profile)
  #~(begin
      (use-modules (ice-9 match))
      (unless #$(getenv "GUIX_FLATPAK_DISABLE")
        (let ((flatpak #$(file-append flatpak "/bin/flatpak"))
              (remotes '#$remotes)
              (profile '#$profile))
          ;; Configure remotes first
          (for-each (match-lambda
                      ((remote-name remote-url)
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
          (for-each (match-lambda
                      ((remote-name app-id)
                       (invoke flatpak
                               "--user"
                               "install"
                               "--or-update"
                               "--noninteractive"
                               remote-name
                               app-id)))
                    profile)
          ;; Update any remaining applications
          (invoke flatpak
                  "--user"
                  "update"
                  "--noninteractive")))))

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
