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
    ((((? string?) (? string?)) ((? string?) (? string?)) ...) #t)
    (else #f)))

(define list-of-flatpak-apps?
  (match-lambda
    ((((? string?) (? string?)) ...) #t)
    (else #f)))

(define-configuration/no-serialization home-flatpak-configuration
  (flatpak (file-like flatpak)
           "The Flatpak package to use.")
  (remotes list-of-remotes
           "A list of remotes.  Each element of the list must be a tuple,
where the first element is the remote name, and the second element is the
associated URL.  At least one remote is required to use this service.

The first remote in the list is used as the default remote.")
  ;; TODO: Use default remote to allow remote to be omitted in specifications.
  (profile (list-of-flatpak-apps '())
           "A list of flatpak applications.  Each entry in the list must be a
tuple, with the first element being the remote name, and the second element
being the designated application ID."))

(define (home-flatpak-packages config)
  "Add flatpak package to profile."
  (let ((flatpak (home-flatpak-configuration-flatpak config)))
    (list flatpak)))

(define (home-flatpak-activation config)
  "Return a gexp that activates the flatpak configuration specified by CONFIG."
  ;; XXX: This service depends on SSL_CERT_FILE pointing to the CA certificates
  ;; file, which is not possible on foreign systems without an initial
  ;; reconfigure.  This file is special in that it is dynamically generated
  ;; rather than built as part of a package (see `ca-certificates-bundle' in
  ;; (guix profiles)), so there does not seem to be a way to insert it into this
  ;; g-exp.
  (match-record config <home-flatpak-configuration>
                (flatpak remotes profile)
    (for-each (match-lambda
                ((remote-name app-id)
                 (unless (assoc remote-name remotes)
                   (raise-exception
                    (make-exception-with-message
                     (format
                      #f "no flatpak remote named ~s exists for ~s"
                      remote-name app-id))))))
              profile)

    #~(begin
        ;; TODO: why does use-modules not work??
        (define-syntax match-lambda
          (identifier-syntax (@ (ice-9 match) match-lambda)))

        (define flatpak #$(file-append flatpak "/bin/flatpak"))
        (define remotes '#$remotes)
        (define profile '#$profile)

        (define configure-remote
          (match-lambda
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
                     remote-url))))

        (define install-app
          (match-lambda
            ((remote-name app-id)
             (invoke flatpak
                     "--user"
                     "install"
                     "--or-update"
                     "--noninteractive"
                     remote-name
                     app-id))))

        (unless #$(getenv "GUIX_FLATPAK_DISABLE")
          (for-each configure-remote remotes)
          (for-each install-app profile)
          ;; Update any remaining applications.
          (invoke flatpak "--user" "update" "--noninteractive")))))

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
                        home-flatpak-activation)))
                (compose concatenate)
                (extend home-flatpak-extend)
                (description "Install and configure Flatpak applications.")))

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
