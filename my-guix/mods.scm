;;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
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
  #:use-module (gnu home)
  #:use-module (guix records)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-71)
  #:export (<mod>
            mod mod?
            this-mod
            mod-name
            mod-os-extension
            mod-he-extension

            <modded-system>
            modded-system modded-system?
            this-modded-system
            modded-system-mods
            modded-system-initial-os
            modded-system-final-os-extension
            modded-system-initial-he
            modded-system-final-he-extension

            mod-os-packages
            mod-os-services
            mod-os-service
            mod-os-kernel-arguments
            mod-os-swap-devices

            mod-he-packages
            mod-he-services
            mod-he-service

            mods-eq?
            excluded-mods
            fold-extensions
            modded-system-operating-system
            modded-system-home-environment
            modded-system-guess-environment))

(define-record-type* <mod>
  mod make-mod
  mod?
  this-mod
  (name mod-name
        (sanitize (sanitizer <symbol>
                             #:label "Mod name")))
  (description mod-description
               (default "")
               (sanitize (sanitizer <string>
                                    #:label "Mod description")))
  (os-extension mod-os-extension
                (default identity) (thunked)
                (sanitize (sanitizer <procedure>
                                     #:label "Mod operating system extension")))
  ;; TODO: Consider multi-user configurations.  An alist (e.g.
  ;; (user . extension)) might do...
  (he-extension mod-he-extension
                (default identity) (thunked)
                (sanitize (sanitizer <procedure>
                                     #:label "Mod home environment extension"))))

(define-record-type* <modded-system>
  modded-system make-modded-system
  modded-system?
  this-modded-system
  (parameters modded-system-parameters
              (default '())
              (sanitize (sanitizer <list>
                                   #:label "Modded system parameters")))
  (mods modded-system-user-mods
        (default '())
        (sanitize (compose (sanitizer <list>
                                      #:label "Modded system mods")
                           (cut delete-duplicates <> mods-eq?))))
  (initial-os modded-system-initial-os
              (default #f)
              (sanitize (lambda (val)
                          (rnrs:assert (or (not val) (operating-system? val)))
                          val)))
  (final-os-extension modded-system-final-os-extension
                      (default identity)
                      (sanitize
                       (sanitizer
                        <procedure>
                        #:label "Modded system operating-system extension")))
  (initial-he modded-system-initial-he
              (default #f)
              (sanitize (lambda (val)
                          (rnrs:assert (or (not val) (home-environment? val)))
                          val)))
  (final-he-extension modded-system-final-he-extension
                      (default identity)
                      (sanitize
                       (sanitizer
                        <procedure>
                        #:label "Modded system home-environment extension"))))

(define (modded-system-mods system)
  "Return the relevant mods of a modded-system configuration.

This procedure respects the excluded-mods parameter."
  (lset-difference mods-eq?
                   (modded-system-user-mods system)
                   (excluded-mods)))

;;; NOTE: Thunked fields such as services should be treated specially: value
;;; computations should be forced to catch errors; however, it is a good idea to
;;; still use the thunked values (instead of the computed values) so that
;;; parameters like %current-system are maintained.

(define ((mod-os-packages packages) os)
  (operating-system
    (inherit os)
    (packages
     (append packages (operating-system-packages os)))))

(define ((mod-os-services services) os)
  (let ((new-os
         (operating-system
           (inherit os)
           (services (append services (operating-system-user-services os))))))
    (operating-system-user-services new-os)
    new-os))

(define ((mod-os-service service-type config-map) os)
  "Modify an operating-system service type SERVICE-TYPE, calling CONFIG-MAP with
the current service configuration and using the return value as the new one."
  (let ((new-os
         (operating-system
           (inherit os)
           (services
            (modify-services (operating-system-user-services os)
              (service-type
               config => (config-map config)))))))
    (operating-system-user-services new-os)
    new-os))

(define ((mod-os-kernel-arguments arguments) os)
  (operating-system
    (inherit os)
    (kernel-arguments
     (append arguments (operating-system-user-kernel-arguments os)))))

(define ((mod-os-swap-devices devices) os)
  (operating-system
    (inherit os)
    (swap-devices
     (append devices (operating-system-swap-devices os)))))

(define ((mod-he-packages packages) he)
  (home-environment
   (inherit he)
   (packages
    (append packages (home-environment-packages he)))))

(define ((mod-he-services services) he)
  (home-environment
   (inherit he)
   (services
    (append services (home-environment-user-services he)))))

(define ((mod-he-service service-type config-map) he)
  "Modify a home-environment service type SERVICE-TYPE, calling CONFIG-MAP with
the current service configuration and using the return value as the new one."
  (let ((new-he
         (home-environment
          (inherit he)
          (services
           (modify-services (home-environment-user-services he)
             (service-type
              config => (config-map config)))))))
    (home-environment-user-services new-he)
    new-he))

(define (mods-eq? ext1 ext2)
  (eq? (mod-name ext1)
       (mod-name ext2)))

(define excluded-mods (make-parameter '()))

(define (fold-extensions initial-record extension-maps)
  "Fold a list of procedures EXTENSION-MAPS onto INITIAL-RECORD.

To handle extension dependencies, this procedure uses an EAFP (\"easier to ask
for forgiveness than permission\") approach with a queue, throwing extension
maps to the back of the queue if they fail, and retrying after round-trips of
applying extensions.  This allows dependencies to be eventually applied before
reaching the dependent extension again."
  (define extension-maps-q (make-q))
  (for-each (cut enq! extension-maps-q <>) extension-maps)

  (let fold-extensions-pass ((record initial-record))
    "Do a single pass of the queue to fold extensions."
    (if (q-empty? extension-maps-q)
        record
        (let %fold-extensions-pass ((record record)
                                    (round-trip-index (1- (q-length
                                                           extension-maps-q)))
                                    (no-progress? #t))
          "Continuously pop the EXTENSION-MAPS queue and fold the mapping onto
RECORD until ROUND-TRIP-INDEX reaches 0, i.e. the last extension mapping of the
pass is popped.  If no progress has been made and the last mapping resulted in
an exception, the exception is re-raised since it indicates a circular
dependency or other unrelated problem."
          (let* ((extension-map (deq! extension-maps-q))
                 (folded-record
                  exn
                  (with-exception-handler (cut values record <>)
                    (lambda ()
                      (values (extension-map record) #f))
                    #:unwind? #t))
                 (no-progress? (if exn no-progress? #f)))
            (when exn
              (enq! extension-maps-q extension-map))
            (if (zero? round-trip-index)
                (if (and exn no-progress?)
                    (raise-exception exn)
                    (fold-extensions-pass folded-record))
                (%fold-extensions-pass folded-record
                                       (1- round-trip-index)
                                       no-progress?)))))))

(define (modded-system-operating-system system)
  "Construct and return the operating-system record from the specifications of
modded-system SYSTEM."
  (unless (modded-system-initial-os system)
    (raise-exception
     (make-exception-with-message
      "System initial-os field is #f; an operating-system must be specified")))
  (let ((params values (unzip2 (modded-system-parameters system)))
        (map-final-extension (modded-system-final-os-extension system)))
    (with-parameters*
     params
     values
     (lambda ()
       (map-final-extension
        (fold-extensions
         (modded-system-initial-os system)
         (map mod-os-extension (modded-system-mods system))))))))

(define (modded-system-home-environment system)
  "Construct and return the home-environment record from the specifications of
modded-system SYSTEM."
  (unless (modded-system-initial-he system)
    (raise-exception
     (make-exception-with-message
      "System initial-he field is #f; a home-environment must be specified")))
  (let ((params values (unzip2 (modded-system-parameters system)))
        (map-final-extension (modded-system-final-he-extension system)))
    (with-parameters*
     params
     values
     (lambda ()
       (map-final-extension
        (fold-extensions
         (modded-system-initial-he system)
         (map mod-he-extension (modded-system-mods system))))))))

(define (modded-system-guess-environment system)
  "Return the operating-system or home-environment record from a modded-system
record depending on the current environment.  If the environment cannot be
determined, return #f.

If the command-line indicates 'guix home' or 'guix system' was invoked, these
are used to determine the result.  Otherwise, the GUIX_CONFIG_FALLBACK
environment variable is used.

If set, GUIX_CONFIG_FALLBACK must be one of the following
values (case-insensitive): 'system', 'home'"
  (define arguments (cons (basename (car (command-line)))
                          (cdr (command-line))))
  (define guix-config-fallback (getenv "GUIX_CONFIG_FALLBACK"))
  (match arguments
    (("guix" "system" _ ...)
     (modded-system-operating-system system))
    (("guix" "home" _ ...)
     (modded-system-home-environment system))
    (else
     (if guix-config-fallback
         (match (string-downcase guix-config-fallback)
           ("system"
            (modded-system-operating-system system))
           ("home"
            (modded-system-home-environment system)))
         #f))))
