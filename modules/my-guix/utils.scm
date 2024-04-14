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

(define-module (my-guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (my-guix config)
  #:use-module (oop goops)
  #:export (path-append
            path-append-my-home
            path-append-my-files
            search-my-patches
            build-path-augmentation
            sanitizer
            patch-wrap-file-script
            crate-package-source))

(define (path-append . paths)
  (string-join paths "/"))

(define (path-append-my-home . paths)
  (apply path-append (getenv "HOME") paths))

(define (path-append-my-files . paths)
  (apply path-append
         GUIX_CONFIG_DIR
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

(define* (patch-wrap-file-script subproject-name
                                 gexp-dir
                                 #:key
                                 (subprojects-dir "subprojects")
                                 (overlay-dir (string-append
                                               subprojects-dir
                                               "/packagefiles/"
                                               subproject-name))
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

OVERLAY-DIR specifies the directory where files to overlay on the subproject
directory are located.  By default, this is expected to be in
\"SUBPROJECTS-DIR/packagefiles/SUBPROJECT-NAME\".

The [provide] section of the wrap file can also be configured via an alist
specification provided to PROVIDES."
  #~(let* ((subproject-name #$subproject-name)
           (subprojects-dir #$subprojects-dir)
           (wrap-file (string-append
                       subprojects-dir "/" subproject-name ".wrap"))
           (subproject-source #$gexp-dir)
           (subproject-dest (string-append
                             subprojects-dir "/" subproject-name))
           (overlay-dir #$overlay-dir))
      (copy-recursively subproject-source subproject-dest)
      (when (file-exists? overlay-dir)
        (copy-recursively overlay-dir subproject-dest))
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
