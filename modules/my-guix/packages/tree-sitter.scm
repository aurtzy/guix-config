;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Alvin Hsu <aurtzy@gmail.com>
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

(define-module (my-guix packages tree-sitter)
  #:use-module (gnu packages)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 exceptions))

(define tree-sitter-grammar
  (@@ (gnu packages tree-sitter) tree-sitter-grammar))

(define-public tree-sitter-c-sharp/newer
  (tree-sitter-grammar
   "c-sharp" "C#"
   "16lnp3642y7c0j6qxhd0rviw1inkvss9px68xnw6h6050jbh141p"
   "0.23.5"))

(define-public tree-sitter-razor
  (let ((commit "a3399c26610817c6d32c7643793caf3729cfb6d2")
        (revision "0"))
    (tree-sitter-grammar
     "razor" "Razor"
     "1s4brh8dg2zny0aw2d1czfvxpr4yilnra5r4qsr06as8kqiflzc4"
     (git-version "0.0.0" revision commit)
     #:commit commit
     #:repository-url
     "https://github.com/tris203/tree-sitter-razor"
     #:inputs (delay (list tree-sitter-c-sharp/newer))
     #:license license:expat)))

(define tree-sitter-razor/local
  (let ((source-path (string-append (getenv "HOME") "/src/tree-sitter-razor")))
    (if (file-exists? source-path)
        (package
          (inherit tree-sitter-razor)
          (name "tree-sitter-razor")
          (version "0.0.0-local")
          (source (local-file source-path
                              #:recursive? #t
                              #:select? (git-predicate source-path)))
          (inputs (list tree-sitter-c-sharp/newer)))
        #f)))

(define-public tree-sitter-razor/maybe-newer
  (or tree-sitter-razor/local tree-sitter-razor))
