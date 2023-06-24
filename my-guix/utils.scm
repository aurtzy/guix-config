;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This modules defines general utility procedures.

(define-module (my-guix utils)
  #:use-module (guix gexp)
  #:use-module ((srfi srfi-1) #:select (not-pair?))
  #:export (symbol->string:alist?))

;; Returns whether an object is an association list with symbol->string pairs.
;; If not an association list, the problem value will be stored
;; in the second return value.
(define (symbol->string:alist? obj)
  (if (not (list? obj))
      (values #f obj)
      (let ((throw-invalid-alist
             (lambda (value)
               ((raise-exception
                 (make-exception-from-throw
                  'invalid-alist (list value))
                 #:continuable? #t)))))
        (with-exception-handler
         (lambda (exn)
           (if (eq? 'invalid-alist (exception-kind exn))
               (values #f (car (exception-args exn)))
               (raise-exception exn)))
         (lambda ()
           (begin
             (for-each
              (lambda (elem)
                (begin
                  (if (not-pair? elem)
                      (throw-invalid-alist elem))
                  (if (not (and
                            (symbol? (car elem))
                            (string? (cdr elem))))
                      (throw-invalid-alist elem))))
              obj)
             (values #t #f)))
         #:unwind? #t))))
