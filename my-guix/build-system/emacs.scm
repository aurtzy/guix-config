;;; Copyright © 2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; This module comes from a WIP patch found here (with a few modifications):
;;; https://issues.guix.gnu.org/72406

(define-module (my-guix build-system emacs)
  #:use-module (gnu packages emacs)
  #:use-module (guix build-system emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (package-with-explicit-emacs
            package-with-emacs-pgtk))

(define* (package-with-explicit-emacs emacs old-prefix new-prefix
                                      #:key variant-property)
  "Return a procedure of one argument, P.  The procedure creates a package with
the same fields as P, which is assumed to use EMACS-BUILD-SYSTEM, such that
it is compiled with EMACS instead.  The inputs are changed recursively
accordingly.  If the name of P starts with OLD-PREFIX, this is replaced by
NEW-PREFIX; otherwise, NEW-PREFIX is prepended to the name.

When VARIANT-PROPERTY is present, it is used as a key to search for
pre-defined variants of this transformation recorded in the 'properties' field
of packages.  The property value must be the promise of a package.  This is a
convenient way for package writers to force the transformation to use
pre-defined variants."
  (define package-variant
    (if variant-property
        (lambda (package)
          (assq-ref (package-properties package)
                    variant-property))
        (const #f)))

  (define (transform pkg)
    (cond
     ;; If VARIANT-PROPERTY is present, use that.
     ((package-variant pkg)
      => force)

     ;; Otherwise build the new package object graph.
     ((and (eq? (package-build-system pkg) emacs-build-system)
           ;; XXX: Patch deviation: emacs-yaml build hangs(?) using emacs-pgtk
           (not (equal? "emacs-yaml" (package-name pkg))))
      (package/inherit pkg
        (location (package-location pkg))
        (name (let ((name (package-name pkg)))
                (string-append new-prefix
                               (if (string-prefix? old-prefix name)
                                   (substring name
                                              (string-length old-prefix))
                                   name))))
        (arguments
         (let ((emacs (if (promise? emacs)
                          (force emacs)
                          emacs)))
           (ensure-keyword-arguments (package-arguments pkg)
                                     `(#:emacs ,emacs))))))
     (else pkg)))

  (define (cut? pkg)
    (or (not (eq? (package-build-system pkg) emacs-build-system))
        (package-variant pkg)))

  ;; XXX: Patch deviation: deep? needs to be true to avoid conflicting entries
  (package-mapping transform cut? #:deep? #t))

(define package-with-emacs-pgtk
  (package-with-explicit-emacs emacs-pgtk
                               "emacs-"
                               ;; XXX: Prefix is supposed to be "emacs-pgtk" but
                               ;; it requires changes to the build system
                               ;; internally (e.g. prefix stripping) I don't
                               ;; want to bother with.
                               "emacs-"
                               #:variant-property 'emacs-pgtk-variant))
