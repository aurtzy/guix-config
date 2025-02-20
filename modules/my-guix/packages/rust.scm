;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Declan Tsien <declantsien@riseup.net>
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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

;;; Commentary:
;;;
;;; Guix supports Rust, but unfortunately not for i686 due to bootstrapping
;;; issues.  The main purpose of this module is to provide a Rust package that
;;; works with i686.
;;;
;;; See potentially related issue: https://issues.guix.gnu.org/35519
;;;
;;; The rust-binary package is based on the one found in this channel:
;;; https://git.sr.ht/~declantsien/guix-channel

(define-module (my-guix packages rust)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages rust)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:export (package/with-rust-binary))

(define gcc-cc
  (package
    (name "gcc-cc")
    (version (package-version gcc))
    (source #f)
    (inputs (list gcc))
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let ((out (assoc-ref %outputs "out"))
                  (gcc (assoc-ref %build-inputs "gcc")))
              (mkdir-p (string-append out "/bin"))
              (symlink (string-append gcc "/bin/gcc")
                       (string-append out "/bin/cc")))))))
    (home-page (package-home-page gcc))
    (synopsis (package-synopsis gcc))
    (description (package-description gcc))
    (license (package-license gcc))))

(define (stable-uri version arch)
  (string-append "https://static.rust-lang.org/dist/rust-"
                 version "-" arch ".tar.gz"))

(define-public rust-binary
  (package
    (inherit rust)
    (name "rust-binary")
    (version "1.82.0")
    (source #f)
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (outputs '("out" "cargo"))
    (native-inputs
     (list `(,gcc "lib")))
    (propagated-inputs
     ;; This is needed for cargo, which specifically tries to run `cc'.
     (list gcc-cc))
    (arguments
     (list
      #:strip-binaries? #t
      #:validate-runpath? #t
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (define unpack (assoc-ref %standard-phases 'unpack))
              (unpack
               #:source
               #+(cond
                  ((target-x86-64?)
                   (origin
                     (method url-fetch)
                     (uri (stable-uri version "x86_64-unknown-linux-gnu"))
                     (sha256
                      (base32
                       "0sv7ry7qnqqwh5xh8ddy1dh4mwmibxh498j8a2bdxi4px65c0r82"))))
                  ((target-x86-32?)
                   (origin
                     (method url-fetch)
                     (uri (stable-uri version "i686-unknown-linux-gnu"))
                     (sha256
                      (base32
                       "0qdym09fp6p6p1k3mckcgdj3pq4mfjc29h47kvipzyly7pxn3ckp"))))))))
          (delete 'patchelf)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (for-each
               (lambda (output)
                 (let* ((name (car output))
                        (source (cond
                                 ((equal? "out" name) "rustc/")
                                 (else (string-append name "/"))))
                        (target (assoc-ref outputs name))
                        (arch #$(match (or (%current-target-system)
                                           (%current-system))
                                  ("x86_64-linux"
                                   "x86_64-unknown-linux-gnu")
                                  ("i686-linux"
                                   "i686-unknown-linux-gnu"))))
                   (if (equal? "out" name)
                       (copy-recursively
                        (format #f "rust-std-~a/lib/"
                                arch)
                        (string-append target "/lib/")))
                   (if (equal? "rust-analyzer-preview" name)
                       (copy-recursively
                        (format #f "rust-analysis-~a/lib/"
                                arch)
                        (string-append target "/lib/")))
                   (copy-recursively source target)))
               outputs)))
          (add-after 'install 'patchelf2
            (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)

              (let* ((ld.so (string-append
                             (assoc-ref inputs "libc")
                             #$((@@ (gnu packages bootstrap) glibc-dynamic-linker)))))
                (for-each
                 (lambda (output)
                   (let* ((name (car output))
                          (out (assoc-ref outputs name))
                          (libdir (string-append out "/lib"))
                          (bindir (string-append out "/bin"))
                          (rpath (string-join
                                  (append
                                   (list "$ORIGIN" libdir)
                                   (if (equal? "out" name)
                                       (list)
                                       (list
                                        (string-append
                                         (assoc-ref outputs "out")
                                         "/lib")))
                                   (map
                                    (lambda (input)
                                      (string-append (cdr input) "/lib"))
                                    inputs))
                                  ":")))

                     (define (patch-elf file)
                       (format #t "Patching ~a ...~%" file)
                       (unless (string-contains file ".so")
                         (invoke "patchelf" "--set-interpreter" ld.so file))
                       (invoke "patchelf" "--set-rpath" rpath file))

                     (for-each (lambda (file)
                                 (when (elf-file? file)
                                   (patch-elf file)))
                               (find-files out (lambda (file stat)
                                                 (elf-file? file))))))
                 outputs))))
          (add-after 'install 'wrap-rustc
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-wrapper (assoc-ref inputs "ld-wrapper")))
                ;; Let gcc find ld and libc startup files.
                (wrap-program (string-append out "/bin/rustc")
                  `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                  `("LIBRARY_PATH" ":"
                    suffix (,(string-append libc "/lib"))))))))))))

(define-syntax-rule (package/with-rust-binary pkg field ...)
  "Create a package with the RUST keyword argument set as rust-binary.  This
currently assumes that the keyword is not already set."
  (package
    (inherit (package/inherit pkg
               (arguments
                (cons*
                 #:rust rust-binary
                 (package-arguments pkg)))))
    field ...))
