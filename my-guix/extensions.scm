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
  #:use-module (srfi srfi-1)
  #:export (<extension>
            extension extension?
            this-extension
            extension-name
            extension-dependencies
            extension-configuration
            extensions-eq?

            extender
            extension-dependencies-all
            extend))

(define-record-type* <extension>
  extension make-extension
  extension?
  this-extension
  (name extension-name
        (sanitize
         (lambda (value)
           (unless (symbol? value)
             (raise-exception
              (make-exception
               (make-programming-error)
               (make-exception-with-message "~a: ~s")
               (make-exception-with-irritants
                (list "Extension name not a symbol"
                      value)))))
           value)))
  (dependencies extension-dependencies
                (default '())
                (sanitize
                 (lambda (value)
                   (unless (list? value)
                     (raise-exception
                      (make-exception
                       (make-programming-error)
                       (make-exception-with-message "~a: ~s")
                       (make-exception-with-irritants
                        (list "Extension dependencies field not a list"
                              value)))))
                   (for-each
                    (lambda (elem)
                      (unless (extension? elem)
                        (raise-exception
                         (make-exception
                          (make-programming-error)
                          (make-exception-with-message "~a: ~s")
                          (make-exception-with-irritants
                           (list "Extension dependencies field contains value that is not an extension"
                                 elem))))))
                    value)
                   value)))
  (configuration extension-configuration
                 (default identity)
                 (sanitize
                  (lambda (value)
                    (unless (procedure? value)
                      (raise-exception
                       (make-exception
                        (make-programming-error)
                        (make-exception-with-message "~a: ~s")
                        (make-exception-with-irritants
                         (list "Extension configuration field not a procedure"
                               value)))))
                    value))))

(define (extensions-eq? ext1 ext2)
  (eq? (extension-name ext1)
       (extension-name ext2)))

(define-syntax extender
  (syntax-rules (=>)
    "Return a procedure that takes RECORD as an input. This new procedure,
when called, will return a record via RECORD-PRODUCER that inherits RECORD and
replaces FIELD.

This form should be used when specifying extension configuration fields."
    ((_ record-producer record => field ...)
     (lambda (record)
       (record-producer
        (inherit record)
        field ...)))))

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

(define* (extend record extensions #:key (exclude '()))
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
       ((extension-configuration extension) record))
     record
     ;; Add the top-level extensions to dependencies list that is returned
     (lset-union extensions-eq?
                 extensions
                 (parameterize ((exclude-extensions exclude))
                   (apply extension-dependencies-all
                          extensions))))))
