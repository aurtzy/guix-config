;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module provides a simple generic interface for extending Guix record
;; configurations.

(define-module (my-guix extensions)
  #:use-module (guix records)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<extension>
            extension extension?
            this-extension
            extension-name
            extension-dependencies
            extension-apply
            extensions-eq?

            extender
            extension-dependencies-all
            apply-extensions))

(define-record-type* <extension>
  extension make-extension
  extension?
  this-extension
  (name extension-name
        (sanitize (sanitizer <symbol>
                             #:label "Extension name")))
  (dependencies extension-dependencies
                (default '())
                (sanitize (sanitizer <list>
                                     #:label "Extension dependencies")))
  (apply extension-apply
         (default identity)
         (sanitize (sanitizer <procedure>
                              #:label "Extension apply field"))))

(define (extensions-eq? ext1 ext2)
  (eq? (extension-name ext1)
       (extension-name ext2)))

(define-syntax modify-field
  (syntax-rules (modify =>
                 modify-list)
    "Modifies a record field and return the resulting modified value.

MODIFY is the most primitive field modifier, giving a BINDING for the field
obtained from FIELD-GETTER and returning the result of the last EXP.

MODIFY-LIST appends the field obtained from FIELD-GETTER with EXP. Optionally,
BINDING may be omitted."
    ((_ record (modify field-getter binding => exp ...))
     (let ((binding (field-getter record)))
       exp ...))
    ((_ record (modify-list field-getter binding => exp))
     (let ((binding (field-getter record)))
       (append exp binding)))
    ((_ record (modify-list field-getter exp))
     (modify-field record (modify-list field-getter %binding => exp)))))

(define-syntax %extend
  (syntax-rules ()
    "Applies each FIELD modification specified to RECORD using CONSTRUCTOR as
the record constructor."
    ((_ record constructor fields)
     (constructor
      (inherit record)
      .
      fields))
    ((_ record constructor field ... (field-name modification) fields)
     (%extend record
              constructor
              field ...
              ((field-name (modify-field record modification))
               .
               fields)))))

(define-syntax extender
  (syntax-rules (=>)
    "Returns a procedure that - when passed RECORD - will apply the FIELD
modifications specified using CONSTRUCTOR as the record constructor.

This form should be used when specifying the apply field for extensions."
   ((_ constructor record => field ...)
    (lambda (record)
      (%extend record constructor field ... ())))
   ((_ constructor field ...)
    (extender constructor %record => field ...))))

(define exclude-extensions (make-parameter '()))

(define (%extension-dependencies-all extension visited-deps)
  (fold
   (lambda (dep visited-deps)
     (if (or (member dep visited-deps extensions-eq?)
             (member dep (exclude-extensions) extensions-eq?))
         visited-deps
         (%extension-dependencies-all dep
                                      (cons dep visited-deps))))
   visited-deps
   (extension-dependencies extension)))

(define (extension-dependencies-all . extensions)
  "Returns a deduplicated list of all the extensions required by EXTENSION,
recursively. Extension names specified by EXCLUDE will not be traversed.

This procedure can accept multiple extensions as arguments for convenience of
use with optimizations for traversing the dependency tree."
  (fold
   (lambda (extension visited-deps)
     (%extension-dependencies-all extension visited-deps))
   '()
   extensions))

(define* (apply-extensions record extensions #:key (exclude '()))
  "Extends RECORD with a list EXTENSIONS of extension records, including
dependencies of extensions (recursively).

Each extension's configuration procedure will be folded onto record; that is,
RECORD will be the input for the first extension's configuration procedure,
and then the output record of that will be passed to the second extension's
configuration procedure, and so on.

The resulting record from applying all extension configurations will be
returned.

Optionally, a list of extensions to exclude from the extend operation can be provided with the EXCLUDE keyword."
  ;; Remove excluded extensions
  (let ((extensions (lset-difference extensions-eq?
                                     extensions
                                     exclude)))
    (fold
     (lambda (extension record)
       ((extension-apply extension) record))
     record
     ;; Add the top-level extensions to dependencies list that is returned
     (lset-union extensions-eq?
                 extensions
                 (parameterize ((exclude-extensions exclude))
                   (apply extension-dependencies-all
                          extensions))))))
