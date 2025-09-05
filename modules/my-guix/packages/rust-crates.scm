;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2025 Alvin Hsu <aurtzy@gmail.com>
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

(define-module (my-guix packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

;; TODO: I think automating this requires guix import to support Meson's
;; naming convention for rust dependencies in subprojects?  Needs to be looked
;; into further.
(define-cargo-inputs lookup-cargo-inputs
                     (mesa =>
                           (list (@@ (gnu packages rust-crates) rust-syn-2.0.106)
                                 (@@ (gnu packages rust-crates) rust-unicode-ident-1.0.18)
                                 (@@ (gnu packages rust-crates) rust-quote-1.0.40)
                                 (@@ (gnu packages rust-crates) rust-proc-macro2-1.0.101)
                                 (@@ (gnu packages rust-crates) rust-paste-1.0.15)
                                 (@@ (gnu packages rust-crates) rust-rustc-hash-2.1.1))))
