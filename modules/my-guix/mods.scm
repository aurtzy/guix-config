;;; Copyright © 2023-2025 Alvin Hsu <aurtzy@gmail.com>
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
;;; This module provides an interface for extending Guix records.

(define-module (my-guix mods)
  #:use-module (gnu)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-71)
  #:export (modded-configuration
            modded-configuration?
            modded-configuration-arguments
            modded-configuration-base
            modded-configuration-mods

            mod-argument
            mod-argument?
            mod-argument-keyword
            mod-argument-default-value
            mod-argument-sanitizer

            base-configuration-argument
            ignored-mods-argument
            service-overrides-argument
            let-mod-arguments
            flatten-mods
            fold-mods
            apply-modifiers
            append-services

            operating-system-mod
            operating-system-mod?
            operating-system-mod-name
            operating-system-mod-description
            operating-system-mod-arguments
            operating-system-mod-addons
            operating-system-mod-kernel-arguments
            operating-system-mod-swap-devices
            operating-system-mod-packages
            operating-system-mod-services
            operating-system-mod-modifier
            this-operating-system-mod-arguments
            modded-configuration-operating-system

            this-operating-system-mod-argument-values
            modded-configuration-operating-system))


;;; Top-level modded configuration.

(define-configuration/no-serialization modded-configuration
  (arguments (list (list))
             "List of keyword argument values that are provided to mods.")
  (base record
        "Initial record, which mods will be applied to.")
  (mods (list (list))
        "Mods to apply to the base record."))


;;; Facilities for operating on mods.

(define anything? (const #t))

(define-maybe/no-serialization anything)

(define-configuration/no-serialization mod-argument
  (keyword keyword "Keyword used to specify argument value.")
  (description (string "") "Description of the argument's purpose.")
  (default-value maybe-anything "Default value for keyword argument.")
  (sanitizer (procedure identity)
             "Sanitizes argument value and returns the actual value to use."))

(define base-configuration-argument
  (mod-argument
    (keyword #:base-configuration)
    (description "The base record of the modded configuration.")))

(define ignored-mods-argument
  (mod-argument
    (keyword #:ignored-mods)
    (description "Mods that have been excluded from being applied.")
    (default-value '())))

(define service-overrides-argument
  (mod-argument
    (keyword #:service-overrides)
    (description
     "Services that are in the #:service-overrides argument (i.e. have the
same @code{service-kind}) will be ignored.  By default, this argument should
be set to the services of @code{modded-configuration-base} when applying mods.
This functions as a way to avoid service conflicts due to mods adding
duplicate services, which would otherwise be an error.

As a recommendation to reduce service conflicts, prefer extending services
(e.g., with @code{simple-service}) over setting initial service values,
even in the same mod, to reduce the likelihood of conflicts occurring.")))

(define (find-keyword-argument keyword arguments)
  "Find keyword-value pair for KEYWORD in ARGUMENTS and return it as a pair.

Return #f if no entry associated with KEYWORD could be found.

If there are duplicate keywords, the last-specified keyword argument is
preferred."
  (let %find-keyword-argument ((last-found #f)
                               (search (memq keyword arguments)))
    (match search
      ((_ value args-rest ...)
       (%find-keyword-argument (cons keyword value) (memq keyword args-rest)))
      (else last-found))))

(define (assoc-mod-argument arguments mod-arg)
  "Get the associated MOD-ARG value from list of keyword arguments ARGUMENTS.

MOD-ARG is a mod-argument record that describes an expected keyword argument
in ARGUMENTS to search for.  If an entry is matched, the value will be passed
through the mod argument's sanitizer.  When there is no match, the mod
argument's default-value field is used; otherwise, an exception is raised when
there is no default available."
  (match-record mod-arg <mod-argument> (keyword default-value sanitizer)
    (match (find-keyword-argument keyword arguments)
      ((_ . value) (sanitizer value))
      (else
       (unless (maybe-value-set? default-value)
         (raise-exception
          (make-exception-with-message
           (format #f "~a argument not provided, but has no default" keyword))))
       (sanitizer default-value)))))

(define-syntax %let-mod-arguments
  (syntax-rules ()
    ((_ args () () body body* ...)
     (begin body body* ...))
    ((_ args (binding binding* ...) (mod-arg mod-arg* ...) body body* ...)
     (let ((binding (assoc-mod-argument args mod-arg)))
       (%let-mod-arguments args (binding* ...) (mod-arg* ...) body body* ...)))))

(define-syntax-rule (let-mod-arguments arguments ((binding mod-arg) ...)
                                       body body* ...)
  "Find mod arguments in ARGUMENTS, bind them, and execute BODY.

ARGUMENTS is a list of keyword arguments.  Values are selectively let-bound
according to MOD-ARG to an associated BINDING symbol."
  (let ((args arguments))
    (%let-mod-arguments args (binding ...) (mod-arg ...) body body* ...)))

(define (flatten-mods mods addons-getter)
  "Flatten the list of MODS to include addons, and return that expanded list.

ADDONS-GETTER is a procedure that is applied to each mod to get any additional
mods to include, recursively."
  (define (%flatten-mods %mods mods-left)
    (match mods-left
      (() (delete-duplicates %mods eq?))
      (else
       (%flatten-mods (append mods-left %mods)
                      (let ((addons (concatenate (map addons-getter mods-left))))
                        (lset-difference eq? addons mods))))))
  (%flatten-mods '() mods))

(define* (fold-mods proc init mods #:key (ignored-mods '()) #:allow-other-keys)
  "Fold MODS over INIT using PROC, and then return the final result.

PROC is called with the form (PROC MOD VALUE), where VALUE is the current
state after applying some mods to INIT, and MOD is a mod in MODS.

If a mod is in IGNORED-MODS, PROC will not be applied to it."
  (define (%fold-mods value mods-left mods-visited)
    (match mods-left
      (() value)
      ((mod mods-rest ...)
       (%fold-mods (if (memq mod ignored-mods) value (proc mod value))
                   mods-rest
                   (cons mod mods-visited)))))
  (%fold-mods init mods '()))

(define* (apply-modifiers modifiers init mods #:rest arguments)
  "Fold MODIFIERS onto INIT, and return the result.

MODIFIERS is a list of procedures that are called with (MODIFIER VALUE MOD),
where VALUE is the intermediate value from folding mods onto the base value
and MOD is a mod in the list of mods to apply.

Mods are applied in modifier-major order.  In other words, a modifier is
applied to all mods before proceeding to the next modifier to do another pass
with the mods, so the last modifier in MODIFIERS will only execute after all
other modifiers have already been applied.

ARGUMENTS is a list of keyword arguments that are passed to fold-mods."
  (fold (lambda (modifier value)
          (apply fold-mods modifier value mods arguments))
        init
        modifiers))

(define (services-equal? o1 o2)
  (eq? (service-kind o1) (service-kind o2)))

(define* (append-services new-services base-services
                          #:key (service-overrides '())
                          #:allow-other-keys)
  "Combine lists of services NEW-SERVICES and BASE-SERVICES.

Services in SERVICE-OVERRIDES are ignored and not included.

If there are conflicting services, an exception will be raised; however, it is
possible to drop one and continue computation under one of the following
conditions:

- The service kind is already present in service-overrides, in which case they
are both dropped.

- The two conflicting services have equal? values, in which case one of the
services can simply be dropped.

- One of the services has a default value (i.e. service-type-default-value),
in which case the one with a non-default value is preferred."
  (fold (lambda (new-service services)
          (match (find (cut services-equal? new-service <>) base-services)
            (#f (cons new-service services))
            (dup-service
             (cond
              ((equal? (service-value new-service) (service-value dup-service))
               services)
              ((equal? (service-type-default-value (service-kind new-service))
                       (service-value new-service))
               services)
              ((equal? (service-type-default-value (service-kind dup-service))
                       (service-value dup-service))
               (cons new-service (modify-services services
                                   (delete (service-kind dup-service)))))
              (else
               (display-hint "Add a service to the base configuration to
override conflicting services.")
               (raise-exception (make-exception-with-message
                                 (format #f "duplicate service: ~s"
                                         (service-kind new-service)))))))))
        base-services
        ;; Exclude base services.  Also, reverse the list to be added so
        ;; ordering is preserved in the final result.
        (reverse (lset-difference
                  services-equal? new-services service-overrides))))


;;; Mod implementation for operating-system records.

(define-record-type* <operating-system-mod>
  operating-system-mod make-operating-system-mod
  operating-system-mod?
  this-operating-system-mod
  (name operating-system-mod-name
        (sanitize (sanitizer <symbol> #:label "Mod name")))
  (description operating-system-mod-description
               (default "")
               (sanitize (sanitizer <string> #:label "Mod description")))
  ;; Internal value.
  ;;
  ;; NOTE: Mods may modify a configuration record's field that may itself be
  ;; thunked.  This makes it infeasible to use parameters, since these values
  ;; would only be evaluated /after/ the final modded configuration is
  ;; returned, where we would no longer have any control (and force-evaluating
  ;; could break use cases where unrelated parameters are used).
  (arguments operating-system-mod-arguments
             (default (list))
             (sanitize (sanitizer <list> #:label "Mod arguments"))
             (innate))
  (addons operating-system-mod-addons
          (default (list))
          (sanitize (sanitizer <list> #:label "Mod addons"))
          (thunked))
  (kernel-arguments operating-system-mod-kernel-arguments
                    (default (list))
                    (sanitize (sanitizer <list> #:label "Mod kernel arguments"))
                    (thunked))
  (swap-devices operating-system-mod-swap-devices
                (default (list))
                (sanitize (sanitizer <list> #:label "Mod swap devices"))
                (thunked))
  (packages operating-system-mod-packages
            (default (list))
            (sanitize (sanitizer <list> #:label "Mod packages"))
            (thunked))
  (services operating-system-mod-services
            (default (list))
            (sanitize (sanitizer <list> #:label "Mod services"))
            (thunked))
  (modifier operating-system-mod-modifier
            (default identity)
            (sanitize (sanitizer <procedure> #:label "Mod modifier"))
            (thunked)))

(define-syntax-rule (this-operating-system-mod-arguments)
  "Return this mod's arguments."
  (operating-system-mod-arguments this-operating-system-mod))

(define (write-operating-system-mod type port)
  (format port "#<operating-system-mod ~a ~a>"
          (operating-system-mod-name type)
          (number->string (object-address type) 16)))

(set-record-type-printer! <operating-system-mod> write-operating-system-mod)

;;;; modded-configuration-operating-system

(define (apply-operating-system-mod-kernel-arguments mod os)
  (operating-system
    (inherit os)
    (kernel-arguments (append (operating-system-mod-kernel-arguments mod)
                              (operating-system-user-kernel-arguments os)))))

(define (apply-operating-system-mod-swap-devices mod os)
  (operating-system
    (inherit os)
    (swap-devices (append (operating-system-mod-swap-devices mod)
                          (operating-system-swap-devices os)))))

(define (apply-operating-system-mod-packages mod os)
  (operating-system
    (inherit os)
    (packages (append (operating-system-mod-packages mod)
                      (operating-system-packages os)))))

(define (apply-operating-system-mod-services mod os)
  (operating-system
    (inherit os)
    (services (apply append-services
                     (operating-system-mod-services mod)
                     (operating-system-user-services os)
                     (operating-system-mod-arguments mod)))))

(define (apply-operating-system-mod-modifier mod os)
  (let ((modifier (operating-system-mod-modifier mod)))
    (modifier os)))

(define (modded-configuration-operating-system config)
  "Return an operating-system by applying mods to the base record of CONFIG."
  (let* ((base-os (modded-configuration-base config))
         (arguments
          (cons* #:base-configuration base-os
                 #:service-overrides (operating-system-user-services base-os)
                 (modded-configuration-arguments config)))
         (mods
          (map (lambda (os-mod)
                 (operating-system-mod
                   (inherit os-mod)
                   (arguments arguments)))
               (flatten-mods (modded-configuration-mods config)
                             operating-system-mod-addons))))
    (apply apply-modifiers
           (list apply-operating-system-mod-kernel-arguments
                 apply-operating-system-mod-swap-devices
                 apply-operating-system-mod-packages
                 apply-operating-system-mod-services
                 apply-operating-system-mod-modifier)
           base-os
           mods
           arguments)))
