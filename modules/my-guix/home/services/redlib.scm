;;; Copyright © 2025 Alvin Hsu <aurtzy@gmail.com>
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
;;; This module provides a service for setting up Redlib.

(define-module (my-guix home services redlib)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (my-guix packages redlib)
  #:export (redlib-configuration
            redlib-configuration?
            redlib-configuration-redlib
            redlib-configuration-port
            home-redlib-service-type))

(define-configuration/no-serialization redlib-configuration
  (redlib (file-like redlib)
          "The redlib package.")
  (port integer
        "The port to run Redlib on."))

(define (redlib-shepherd-services config)
  (list (match-record config <redlib-configuration> (redlib port)
          (shepherd-service
            (documentation "Run Redlib.")
            (provision '(redlib))
            (requirement '())
            (start #~(make-forkexec-constructor
                      (list #$(file-append redlib "/bin/redlib")
                            "--port" #$(number->string port))))
            (stop #~(make-kill-destructor))))))

(define home-redlib-service-type
  (service-type
    (name 'home-redlib)
    (description "Run a Redlib server.")
    (extensions
     (list (service-extension home-shepherd-service-type
                              redlib-shepherd-services)))))
