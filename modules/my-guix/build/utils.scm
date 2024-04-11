;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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
;;; This module provides utilities for use during Guix build phases.

(define-module (my-guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (patch-crate-wrap-file-script))

;; TODO Generalize this somehow?  Consider:
;; - package -> source ?  Can have this parameter be a gexp that evaluate to
;;   a path to the source.
;; - Any other needed specifications from wrap files?  Is it necessary to
;;   allow for specifying the entire wrap file?
;; - What to do about packagefiles?  Is this standardized?
(define (patch-crate-wrap-file-script wrap-file package)
  "Writes a wrap file for PACKAGE at path WRAP-FILE to enable using the
non-pinned package version in meson builds."
  (let* ((crate-name (package-upstream-name* package))
         (crate-full-name (string-append
                           crate-name "-" (package-version package)))
         (crate #~#$(file-append package
                                 "/share/cargo/src/"
                                 crate-full-name))
         (wrap-dir (dirname wrap-file)))
    #~(call-with-output-file #$wrap-file
        (lambda (port)
          (copy-recursively #$crate
                            #$(string-append wrap-dir "/" crate-full-name))
          (copy-recursively #$(string-append wrap-dir
                                             "/packagefiles/"
                                             crate-name)
                            #$(string-append wrap-dir "/" crate-full-name))
          (format
           port
           "[wrap-file]
directory = ~a
patch_directory = ~a
"
           #$crate-full-name
           #$crate-name)))))
