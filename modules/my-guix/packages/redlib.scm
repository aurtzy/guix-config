;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024-2025 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages redlib)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

(define-public redlib
  (package
    (name "redlib")
    (version "dev")
    ;; This requires `redlib` to be separately built from source.
    (source (local-file (string-append (getenv "HOME")
                                       "/src/redlib/target/debug/redlib")))
    (build-system binary-build-system)
    (arguments
     (list
      ;; TODO: This needs patching to validate runpath.
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-to-bin
            (lambda _
              (mkdir-p "bin")
              (rename-file "redlib" "bin/redlib")
              (chmod "bin/redlib" #o777))))))
    (home-page "https://github.com/redlib-org/redlib")
    (synopsis "Alternative private front-end to Reddit")
    (description
     "This package provides Redlib, an alternative private front-end to
Reddit.")
    (license license:agpl3)))
