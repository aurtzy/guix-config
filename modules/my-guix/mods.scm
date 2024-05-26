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
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-71)
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
            modded-system-initial-os
            modded-system-initial-he

            mod-os-packages
            mod-os-services
            mod-os-kernel-arguments
            mod-os-swap-devices

            mod-he-packages
            mod-he-services

            mods-eq?
            excluded-mods
            mod-dependencies/deep
            modded-system-operating-system
            modded-system-home-environment))

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
  (parameters modded-system-parameters
              (default '())
              (sanitize (sanitizer <list>
                                   #:label "Modded system parameters")))
  (mods modded-system-mods
        (default '())
        (sanitize (sanitizer <list>
                             #:label "Modded system mods")))
  (initial-os modded-system-initial-os
              (default #f)
              (sanitize (lambda (val)
                          (rnrs:assert (or (not val) (operating-system? val)))
                          val)))
  (initial-he modded-system-initial-he
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

(define excluded-mods (make-parameter '()))

(define (mod-dependencies/deep mod)
  "Get all the dependencies of a mod, recursively.  Mods specified by the
EXCLUDED-MODS parameter will not be included in the returned list."
  (define (dependencies/deep mod visited)
    (fold
     (lambda (dep visited)
       (if (member dep (append visited (excluded-mods)) mods-eq?)
           visited
           (dependencies/deep dep (cons dep visited))))
     visited
     (mod-dependencies mod)))
  (dependencies/deep mod '()))

(define (all-unique-mods mods)
  "Return all unique mods from the list of mods MODS provided, including
dependencies (recursive).  This procedure uses MOD-DEPENDENCIES/DEEP, which
respects the EXCLUDED-MODS parameter.  The parameter is further respected by
removing mods in MODS if any are members of EXCLUDED-MODS."
  (let ((mods (lset-difference mods-eq? mods (excluded-mods))))
    (lset-union mods-eq? mods (concatenate (map mod-dependencies/deep mods)))))

(define (modded-system-operating-system system)
  "Construct and return the operating-system record from the specifications of
modded-system SYSTEM."
  (unless (modded-system-initial-os system)
    (raise-exception
     (make-exception-with-message
      "System initial-os field is #f; an operating-system must be specified")))
  (let ((params values (unzip2 (modded-system-parameters system))))
    (with-parameters*
     params
     values
     (lambda ()
       (fold
        (lambda (mod record)
          ;; TODO use os-extension when it exists
          ((mod-apply mod) record))
        (modded-system-initial-os system)
        (all-unique-mods (modded-system-mods system)))))))

(define (modded-system-home-environment system)
  "Construct and return the home-environment record from the specifications of
modded-system SYSTEM."
  (unless (modded-system-initial-he system)
    (raise-exception
     (make-exception-with-message
      "System initial-he field is #f; a home-environment must be specified")))
  (let ((params values (unzip2 (modded-system-parameters system))))
    (with-parameters*
     params
     values
     (lambda ()
       (fold
        (lambda (mod record)
          ;; TODO use he-extension when it exists
          ((mod-apply mod) record))
        (modded-system-initial-he system)
        (all-unique-mods (modded-system-mods system)))))))
