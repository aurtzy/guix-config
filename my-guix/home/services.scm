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
;; This module defines general service procedures.

(define-module (my-guix home services)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix config)
  #:use-module (my-guix utils)
  #:export (manifest-service
            flatpak-service
            stow-service))

;; TODO note: typo in guile manual, 6.11.8.2, raise-exception:
;; "#:continuable=#f" should be "#:continuable?=#f"

(define (manifest-service name packages)
  (simple-service name home-profile-service-type
                  packages))

;; TODO turn this into a more general procedure for reuse to error-check other stuff
(define flatpak-remotes
  (call-with-values
      (lambda ()
        (symbol->string:alist? $flatpak-remotes))
    (lambda (true error-value)
      (if (not true)
          (raise-exception
           (make-exception
            (make-external-error)
            (make-exception-with-message
             (format #f
                     "Expected $flatpak-remotes to return a list of remote-name-symbol->remote-url pairs. Invalid value: ~:a"
                     error-value)))))
      $flatpak-remotes)))

(define (flatpak-service name remote refs)
  (simple-service name home-activation-service-type
                  #~(unless #$(getenv "GUIX_DISABLE_FLATPAK")
                      (invoke "flatpak"
                              "--user" "remote-add" "--if-not-exists"
                              #$(symbol->string remote)
                              #$(assq-ref flatpak-remotes remote))
                      (apply invoke "flatpak"
                             "--user" "install" "--or-update" "--noninteractive"
                             #$(symbol->string remote)
                             '#$refs))))

(define (stow-service name pkg)
  (simple-service name home-activation-service-type
                  #~(unless #$(getenv "GUIX_DISABLE_STOW")
                      (invoke "stow"
                              "--no-folding"
                              (string-append "--dir=" #$(files-ref "stow"))
                              (string-append "--target=" (getenv "HOME"))
                              "--restow"
                              #$pkg))))
