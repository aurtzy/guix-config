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
;;; This module defines general utility procedures.

;;; TODO: Add some kind of path-append procedure/macro that uses files/mod-name
;;; as the base path?

(define-module (my-guix utils)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (my-guix config)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (path-append
            path-append-my-home
            path-append-my-files
            path-append-my-static-assets-directory
            search-my-patches
            build-path-augmentation
            sanitizer
            crate-package-source
            compose-lambda
            package-all-inputs))

(define static-assets-sub-directory ".static")

(define (path-append . paths)
  (string-join paths "/"))

(define (path-append-my-home . paths)
  (apply path-append (getenv "HOME") paths))

(define (path-append-my-files . paths)
  (apply path-append
         GUIX_CONFIG_DIR
         "files"
         paths))

(define (path-append-my-static-assets-directory alias . paths)
  "Path-append PATHS to the static assets directory corresponding to ALIAS, as
specified in DENOTE_ALIASES_FILE, and return the result.  If the file does not
exist or an entry for ALIAS is not found, #false is returned."
  (define aliases
    (false-if-exception (call-with-input-file DENOTE_ALIASES_FILE read)))

  (define alias-id
    (assoc-ref aliases alias))

  (define (is-sub-directory? file)
    (and (not (string-prefix? "." file))
         (and=> (stat file #f)
                (lambda (st)
                  (eq? 'directory (stat:type st))))))

  (if (not alias-id)
      #f
      (let* ((data-directories
              (with-directory-excursion DENOTE_DIRECTORY
                (map canonicalize-path
                     (scandir "." is-sub-directory?))))
             (all-assets-directories
              (concatenate
               (map (lambda (data-dir)
                      (with-directory-excursion data-dir
                        (map canonicalize-path
                             (scandir data-dir is-sub-directory?))))
                    data-directories)))
             (matched-asset-dir
              (car (member alias-id all-assets-directories
                           (lambda (alias-id path)
                             (string-contains path alias-id))))))
        (unless matched-asset-dir
          (raise-exception
           (make-exception-with-message
            (format #f "Aliased ID found, but assets directory does not exist: ~s (~s)"
                    alias alias-id))))
        (apply path-append matched-asset-dir static-assets-sub-directory paths))))

(define (search-my-patches . names)
  (map (lambda (name)
         (let ((path (path-append-my-files "patches" name)))
           (unless (file-exists? path)
             (raise-continuable
              (make-exception
               (make-warning)
               (make-exception-with-message
                (format #f "Custom patch does not exist: ~s" path)))))
           path))
       names))

(define (build-path-augmentation var path . paths)
  "Builds an sh expression that augments the environment variable VAR to
include PATH in a colon-separated fashion."
  (format #f
          "~a${~a:+:${~a}}"
          (string-join (cons path paths) ":")
          var
          var))

(define* (sanitizer type #:key (label "Value"))
  "Returns a procedure that asserts a value is of type TYPE, raising an
exception if the condition does not hold.

Values must be compatible with GOOPS types (which is what this procedure
uses)."
  (lambda (value)
    (unless (is-a? value type)
      (raise-exception
       (make-exception
        (make-programming-error)
        (make-exception-with-message "~a ~a ~s: ~s")
        (make-exception-with-irritants
         (list label
               "not of type"
               (class-name type)
               value)))))
    value))

(define* (crate-package-source file
                               #:key
                               (name (package-upstream-name* file))
                               (version (package-version file)))
  "Return a <file-append> object that concatenates FILE to the crate source
code location designated by cargo-build-system.  By default, the file is
assumed to be a package, where the crate source directory name is constructed
from the package's VERSION and upstream NAME."
  (file-append file "/share/cargo/src/" name "-" version))

(define-syntax compose-lambda
  (syntax-rules ()
    "A lazy version of compose, where the evaluation of BODY is delayed until
the composed procedure is called with FORMALS as arguments.

The last expression of BODY should evaluate to a list, which will be applied
to compose for the composed procedure."
    ((_ (formals ...) body ...)
     (lambda (formals ...)
       ((apply compose (let () body ...)) formals ...)))
    ((_ formals body ...)
     (lambda formals
       (apply (apply compose (let () body ...)) formals)))))

(define (package-all-inputs package)
  "Return a list of all inputs of PACKAGE, recursively."
  (let %package-all-inputs ((next-inputs (package-direct-inputs package))
                            (all-inputs '()))
    (match next-inputs
      ((input next-inputs ...)
       (cond ((member input all-inputs)
              (%package-all-inputs next-inputs all-inputs))
             (else
              (%package-all-inputs
               (append next-inputs
                       (match input
                         ((_ (? package? pkg) _ ...)
                          (package-direct-inputs pkg))
                         (else
                          '())))
               (cons input all-inputs)))))
      (else
       all-inputs))))
