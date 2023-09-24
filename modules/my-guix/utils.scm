;;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix config)
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (not-pair?))
  #:export (search-files-path
            path-append
            path-append-my-home
            path-append-my-files
            build-path-augmentation
            sanitizer))

(define (search-files-path . relpaths)
  (let ((path (string-join
               (cons* $my-guix-config
                      "files"
                      relpaths)
               "/")))
    (if (file-exists? path)
        (canonicalize-path path)
        ;; TODO I'd prefer to use this instead of the print statement below,
        ;; but there are cases where I may want to test other things without
        ;; regard for the warning, except that when there's no exception
        ;; handler - like when using the repl (maybe it can be enabled?) - it
        ;; becomes less useful.
        ;;
        ;; (raise-continuable
        ;;  (make-exception
        ;;   (make-warning)
        ;;   (make-exception-with-message "~a: ~s")
        ;;   (make-exception-with-irritants
        ;;    (list "File not found in files path"
        ;;          file))))
        (begin
          (format (current-error-port)
                  "WARNING: ~a: ~s\n"
                  "File not found in files path"
                  file)
          path))))
(define (path-append . paths)
  (string-join paths "/"))

(define (path-append-my-home . paths)
  (apply path-append (getenv "HOME") paths))

(define (path-append-my-files . paths)
  (apply path-append
         $my-guix-config
         "files"
         paths))


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
