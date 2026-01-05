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
  #:use-module (gnu packages perl)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

(define-public redlib
  (let ((commit "43fe3585ff2a10c588de081a293d7bab60562d68")
        (revision "0"))
    (package
      (name "redlib")
      (version (git-version "0.36.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/chowder/redlib")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00ccms8pnj8i0prk6vzn6xa4xlq0psgxv9v86k63i6d4q33samc8"))))
      (build-system cargo-build-system)
      (arguments (list #:install-source? #f
                       ;; TODO: Fix tests.
                       #:tests? #f))
      (native-inputs (list perl))
      (inputs (cargo-inputs 'redlib #:module '(my-guix packages rust-crates)))
      (home-page "https://github.com/redlib-org/redlib")
      (synopsis "Alternative private front-end to Reddit")
      (description
       "This package provides Redlib, an alternative private front-end to
Reddit.")
      (license license:agpl3))))
