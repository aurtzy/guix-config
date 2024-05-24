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
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:use-module (srfi srfi-1)
  #:export (<mod>
            mod mod?
            this-mod
            mod-name
            mod-dependencies
            mod-apply

            <modded-system>
            modded-system modded-system?
            this-modded-system
            modded-system-mods
            modded-system-base-os
            modded-system-base-he

            mod-os-packages
            mod-os-services
            mod-os-kernel-arguments
            mod-os-swap-devices

            mod-he-packages
            mod-he-services

            mods-eq?
            mod-dependencies-all
            apply-mods))

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
  (dependencies mod-dependencies
                (default '())
                (sanitize (sanitizer <list>
                                     #:label "Mod dependencies")))
  (apply mod-apply
         (default identity)
         (sanitize (sanitizer <procedure>
                              #:label "Mod apply field"))))

(define-record-type* <modded-system>
  modded-system make-modded-system
  modded-system?
  this-modded-system
  (mods modded-system-mods
        (default '())
        (sanitize (sanitizer <list>
                             #:label "Modded system mods")))
  (base-os modded-system-base-os
           (default #f)
           (sanitize (lambda (val)
                       (rnrs:assert (or (not val) (operating-system? val)))
                       val)))
  (base-he modded-system-base-he
           (default #f)
           (sanitize (lambda (val)
                       (rnrs:assert (or (not val) (home-environment? val)))
                       val))))

(define ((mod-os-packages packages) os)
  (operating-system
    (inherit os)
    (packages
     (append packages (operating-system-packages os)))))

(define ((mod-os-services services) os)
  (operating-system
    (inherit os)
    (services
     (append services (operating-system-user-services os)))))

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

(define (mods-eq? ext1 ext2)
  (eq? (mod-name ext1)
       (mod-name ext2)))

(define exclude-mods (make-parameter '()))

(define (%mod-dependencies-all mod visited-deps)
  (fold
   (lambda (dep visited-deps)
     (if (or (member dep visited-deps mods-eq?)
             (member dep (exclude-mods) mods-eq?))
         visited-deps
         (%mod-dependencies-all dep
                                (cons dep visited-deps))))
   visited-deps
   (mod-dependencies mod)))

(define (mod-dependencies-all . mods)
  "Returns a deduplicated list of all the mods required by MOD,
recursively. Mod names specified by EXCLUDE will not be traversed.

This procedure can accept multiple mods as arguments for convenience of use
with optimizations for traversing the dependency tree."
  (fold
   (lambda (mod visited-deps)
     (%mod-dependencies-all mod visited-deps))
   '()
   mods))

(define* (apply-mods record mods #:key (exclude '()))
  "Extends RECORD with a list MODS of mod records, including dependencies of
mods (recursively).

Each mod's configuration procedure will be folded onto record; that is, RECORD
will be the input for the first mod's configuration procedure, and then the
output record of that will be passed to the second mod's configuration
procedure, and so on.

The resulting record from applying all mod configurations will be returned.

Optionally, a list of mods to exclude from the operation can be provided with
the EXCLUDE keyword."
  ;; Remove excluded mods
  (let ((mods (lset-difference mods-eq?
                               mods
                               exclude)))
    (fold
     (lambda (mod record)
       ((mod-apply mod) record))
     record
     ;; Add the top-level mods to dependencies list that is returned
     (lset-union mods-eq?
                 mods
                 (parameterize ((exclude-mods exclude))
                   (apply mod-dependencies-all
                          mods))))))
