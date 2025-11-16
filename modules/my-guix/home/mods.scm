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
;;; This module provides an interface for extending Guix home-environment
;;; records.  See (my-guix mods) for more information.

(define-module (my-guix home mods)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (guix records)
  #:use-module (my-guix mods)
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9 gnu)
  #:export (home-environment-mod
            home-environment-mod?
            home-environment-mod-name
            home-environment-mod-description
            home-environment-mod-arguments
            home-environment-mod-addons
            home-environment-mod-packages
            home-environment-mod-services
            home-environment-mod-modifier
            this-home-environment-mod-arguments
            modded-configuration-home-environment))


;;; Mod implementation for home-environment records.

(define-record-type* <home-environment-mod>
  home-environment-mod make-home-environment-mod
  home-environment-mod?
  this-home-environment-mod
  (name home-environment-mod-name
        (sanitize (sanitizer <symbol> #:label "Mod name")))
  (description home-environment-mod-description
               (default "")
               (sanitize (sanitizer <string> #:label "Mod description")))
  ;; Internal value.
  (arguments home-environment-mod-arguments
             (default (list))
             (sanitize (sanitizer <list> #:label "Mod arguments"))
             (innate))
  (addons home-environment-mod-addons
          (default (list))
          (sanitize (sanitizer <list> #:label "Mod addons"))
          (thunked))
  (packages home-environment-mod-packages
            (default (list))
            (sanitize (sanitizer <list> #:label "Mod packages"))
            (thunked))
  (services home-environment-mod-services
            (default (list))
            (sanitize (sanitizer <list> #:label "Mod services"))
            (thunked))
  (modifier home-environment-mod-modifier
            (default identity)
            (sanitize (sanitizer <procedure> #:label "Mod modifier"))
            (thunked)))

(define-syntax-rule (this-home-environment-mod-arguments)
  (home-environment-mod-arguments this-home-environment-mod))

(define (write-home-environment-mod type port)
  (format port "#<home-environment-mod ~a ~a>"
          (home-environment-mod-name type)
          (number->string (object-address type) 16)))

(set-record-type-printer! <home-environment-mod> write-home-environment-mod)

;;;; modded-configuration-home-environment

(define (apply-home-environment-mod-packages mod he)
  (home-environment
    (inherit he)
    (packages (append (home-environment-mod-packages mod)
                      (home-environment-packages he)))))

(define (apply-home-environment-mod-services mod he)
  (home-environment
    (inherit he)
    (services (apply append-services
                     (home-environment-mod-services mod)
                     (home-environment-user-services he)
                     (home-environment-mod-arguments mod)))))

(define (apply-home-environment-mod-modifier mod he)
  (let ((modifier (home-environment-mod-modifier mod)))
    (modifier he)))

(define (modded-configuration-home-environment config)
  "Return a home-environment by applying mods to the base record of CONFIG."
  (let* ((base-he (modded-configuration-base config))
         (arguments
          (cons* #:base-configuration base-he
                 #:service-overrides (home-environment-user-services base-he)
                 (modded-configuration-arguments config)))
         (mods
          (map (lambda (he-mod)
                 (home-environment-mod
                   (inherit he-mod)
                   (arguments arguments)))
               (flatten-mods (modded-configuration-mods config)
                             home-environment-mod-addons))))
    (apply apply-modifiers
           (list apply-home-environment-mod-packages
                 apply-home-environment-mod-services
                 apply-home-environment-mod-modifier)
           base-he
           mods
           arguments)))
