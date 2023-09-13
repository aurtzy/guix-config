;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
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
  #:use-module (guix records)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<mod>
            mod mod?
            this-mod
            mod-name
            mod-dependencies
            mod-apply
            mods-eq?

            apply-mod
            mod-dependencies-all
            apply-mods))

(define-record-type* <mod>
  mod make-mod
  mod?
  this-mod
  (name mod-name
        (sanitize (sanitizer <symbol>
                             #:label "Mod name")))
  (dependencies mod-dependencies
                (default '())
                (sanitize (sanitizer <list>
                                     #:label "Mod dependencies")))
  (apply mod-apply
         (default identity)
         (sanitize (sanitizer <procedure>
                              #:label "Mod apply field"))))

(define (mods-eq? ext1 ext2)
  (eq? (mod-name ext1)
       (mod-name ext2)))

(define-syntax modify-field
  (syntax-rules (=>
                 append=>)
    "Modifies a record field and return the resulting modified value.

MODIFY is the most primitive field modifier, giving a BINDING for the field
obtained from FIELD-GETTER and returning the result of the last EXP.

MODIFY-LIST appends the field obtained from FIELD-GETTER with EXP. Optionally,
BINDING may be omitted."
    ((_ record field-getter binding => exp)
     (let ((binding (field-getter record)))
       exp))
    ((_ record field-getter binding append=> exp)
     (modify-field record field-getter binding => (append exp binding)))
    ((_ record field-getter append=> exp)
     (modify-field record field-getter %binding append=> exp))))

(define-syntax %modify-fields
  (syntax-rules ()
    "Applies each FIELD modification specified to RECORD using CONSTRUCTOR as
the record constructor."
    ((_ record constructor fields)
     (constructor
      (inherit record)
      .
      fields))
    ((_ record constructor field ... (field-name args ...) fields)
     (%modify-fields record
                     constructor
                     field ...
                     ((field-name (modify-field record args ...))
                      . fields)))))

(define-syntax apply-mod
  (syntax-rules (=>)
    "Returns a procedure that - when passed RECORD - will apply the FIELD
modifications specified using CONSTRUCTOR as the record constructor.

This form should be used when specifying the apply field for mods."
    ((_ constructor record => field ...)
     (lambda (record)
       (%modify-fields record constructor field ... ())))
    ((_ constructor field ...)
     (apply-mod constructor %record => field ...))))

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
