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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (flatpak-overrides-configuration
            flatpak-overrides-configuration?
            flatpak-overrides-configuration-shared
            flatpak-overrides-configuration-sockets
            flatpak-overrides-configuration-filesystems
            flatpak-overrides-configuration-persistent
            flatpak-overrides-configuration-features
            flatpak-overrides-configuration-unset-environment
            flatpak-overrides-configuration-environment
            flatpak-overrides-configuration-session-bus-policy
            flatpak-overrides-configuration-system-bus-policy
            flatpak-overrides-configuration-extra-content

            home-flatpak-configuration
            home-flatpak-configuration?
            home-flatpak-configuration-flatpak
            home-flatpak-configuration-remotes
            home-flatpak-configuration-profile
            home-flatpak-configuration-global-overrides

            home-flatpak-service-type
            home-flatpak-profile-service-type))

(define (serialize-list-of-strings-in-context-group field-name value)
  (format #f "[Context]~%~a=~a~%"
          field-name (string-join value ";")))

(define list-of-strings-in-context-group? list-of-strings?)

(define-maybe list-of-strings-in-context-group)

(define (uglify-variables-field-name field-name)
  (string-join (map string-capitalize
                    (string-split (symbol->string field-name) #\-))
               " "))

(define (serialize-alist-of-variables field-name variables)
  (format #f "[~a]~%~{~a~%~}"
          (uglify-variables-field-name field-name)
          (map (match-lambda ((key . value) (format #f "~a=~a" key value)))
               variables)))

(define alist-of-variables?
  (match-lambda
    ((((? string?) . (? string?)) ...) #t)
    (else #f)))

(define-maybe alist-of-variables)

(define serialize-alist-of-bus-policy-variables serialize-alist-of-variables)

(define alist-of-bus-policy-variables?
  (match-lambda
    ((((? string?) . (or "none" "see" "talk" "own")) ...) #t)
    (else #f)))

(define-maybe alist-of-bus-policy-variables)

(define (serialize-string _ value)
  value)

(define-maybe string)

(define-configuration flatpak-overrides-configuration
  ;; TODO: Documentation.
  ;; See: https://docs.flatpak.org/en/latest/flatpak-command-reference.html#flatpak-metadata
  (shared maybe-list-of-strings-in-context-group
          "")
  (sockets maybe-list-of-strings-in-context-group
           "")
  (devices maybe-list-of-strings-in-context-group
           "")
  (filesystems maybe-list-of-strings-in-context-group
               "")
  (persistent maybe-list-of-strings-in-context-group
              "")
  (features maybe-list-of-strings-in-context-group
            "")
  (unset-environment maybe-list-of-strings-in-context-group
                     "")
  (environment maybe-alist-of-variables
               "")
  (session-bus-policy maybe-alist-of-bus-policy-variables
                      "")
  (system-bus-policy maybe-alist-of-bus-policy-variables
                     "")
  (extra-content maybe-string
                 "Extra content to append to overrides file."))

(define-maybe/no-serialization flatpak-overrides-configuration)

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
being the designated application ID.")
  (global-overrides maybe-flatpak-overrides-configuration
                    "Override permissions for all Flatpak applications."))

(define (home-flatpak-file-entries config)
  (match-record config <home-flatpak-configuration>
                (global-overrides)
    (define overrides-dir ".local/share/flatpak/overrides")
    (if (maybe-value-set? global-overrides)
        (list `(,(string-append overrides-dir "/global")
                ,(mixed-text-file
                  "global" (serialize-configuration
                            global-overrides
                            flatpak-overrides-configuration-fields))))
        '())))

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
                        home-activation-service-type
                        home-flatpak-activation)
                       (service-extension
                        home-files-service-type
                        home-flatpak-file-entries)
                       (service-extension
                        home-profile-service-type
                        home-flatpak-packages)))
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
