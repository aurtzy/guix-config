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
  #:use-module (ice-9 exceptions)
  #:use-module (my-guix config)
  #:use-module (oop goops)
  #:export (path-append
            path-append-my-home
            path-append-my-files
            search-my-patches
            build-path-augmentation
            sanitizer))

(define (path-append . paths)
  (string-join paths "/"))

(define (path-append-my-home . paths)
  (apply path-append (getenv "HOME") paths))

(define (path-append-my-files . paths)
  (apply path-append
         $my-guix-config
         "files"
         paths))

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
