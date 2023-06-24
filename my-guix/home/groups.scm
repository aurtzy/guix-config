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
;; This module defines group-related utilities.

(define-module (my-guix home groups)
  #:use-module (gnu services)
  #:use-module (my-guix config)
  #:use-module (my-guix utils)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module (ice-9 q)
  #:use-module (ice-9 exceptions)
  #:use-module ((srfi srfi-1) #:select (concatenate any))
  #:export (define-group))

;; make this less hacky; see TODO document
(define-macro (define-group name . services)
  (let ((name-str (symbol->string name)))
    `(define-public ,(string->symbol (string-append name-str "-group"))
       (list . ,(map
                 (lambda (service)
                   (let ((service-def (car service)))
                     (cons* service-def
                            `(string->symbol
                              ,(string-append
                                name-str "-" (symbol->string service-def)))
                            (cdr service))))
                 services)))))
