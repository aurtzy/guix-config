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
  #:use-module (ice-9 match)
  #:export (patch-wrap-file-script
            crate-package-source))


(define* (patch-wrap-file-script subproject-name
                                 gexp-dir
                                 #:key
                                 (subprojects-dir "subprojects")
                                 (provides '()))
  "Return a gexp script that generates a local directory with GEXP-DIR
recursively copied to it, and then patches the wrap file for SUBPROJECT-NAME
to use it.  This is particularly useful for wrap files that would otherwise
attempt to fetch the content from the Internet.

SUBPROJECT-NAME is the name of the subproject that the wrap file will apply
to.  It should match the base name of the wrap file (without the \".wrap\"
extension) to patch.

GEXP-DIR is a g-expression that should evaluate to a string directory path.
This is the path that SUBPROJECT-NAME will be patched to use.

SUBPROJECTS-DIR is a string path to the directory where wrap files are
located for a project.

The [provide] section of the wrap file can also be configured via an alist
specification provided to PROVIDES."
  #~(let* ((subproject-name #$subproject-name)
           (subprojects-dir #$subprojects-dir)
           (wrap-file (string-append
                       subprojects-dir "/" subproject-name ".wrap"))
           (subproject-source #$gexp-dir)
           (subproject-dest (string-append
                             subprojects-dir "/" subproject-name))
           (package-file (string-append
                          subprojects-dir "/packagefiles/" subproject-name)))
      (copy-recursively subproject-source subproject-dest)
      ;; Apply packagefiles when available
      (when (file-exists? package-file)
        (copy-recursively package-file subproject-dest))
      (call-with-output-file wrap-file
        (lambda (port)
          (format port
                  "[wrap-file]
directory = ~a
"
                  subproject-name)
          ;; TODO this hasn't been tested yet.
          (unless #$(null? provides)
            (format port
                    "[provide]
~a"
                    '#$(string-join
                        (map (match-lambda
                               ((key . value)
                                (string-append key " = " value)))
                             provides)
                        "\n"
                        'suffix)))))))

(define* (crate-package-source file
                               #:key
                               (name (package-upstream-name* file))
                               (version (package-version file)))
  "Return a <file-append> object that concatenates FILE to the crate source
code location designated by cargo-build-system.  By default, the file is
assumed to be a package, where the crate source directory name is constructed
from the package's VERSION and upstream NAME."
  (file-append file "/share/cargo/src/" name "-" version))
