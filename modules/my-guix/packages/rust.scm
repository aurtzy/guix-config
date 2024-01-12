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
  #:export (rust-binary))

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

;;; TODO Attempt to combine sources for different architectures

(define* (make-rust-binary name version source
                           #:key
                           (outputs (list "out" "cargo")))
  (package
    (inherit rust)
    (name name)
    (version version)
    (source source)
    (build-system binary-build-system)
    (outputs outputs)
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

(define (stable-uri version arch)
  (string-append "https://static.rust-lang.org/dist/rust-"
                 version "-" arch ".tar.gz"))

(define-public rust-binary-x86_64
  (let ((version "1.75.0"))
    (make-rust-binary
     "rust-binary-x86_64"
     version
     (origin
       (method url-fetch)
       (uri (stable-uri version "x86_64-unknown-linux-gnu"))
       (sha256
        (base32
         "1xkl1p7yhijbj7krcqcnbbwkqs3b3hhib4z8z64n68gzz2v7hfa7"))))))

(define-public rust-binary-i686
  (let ((version "1.75.0"))
    (make-rust-binary
     "rust-binary-i686"
     version
     (origin
       (method url-fetch)
       (uri (stable-uri version "i686-unknown-linux-gnu"))
       (sha256
        (base32
         "13v7qrbhjvz98w47ji26nddj59lxh93z05cb6f7k7ayy4n48syqh"))))))

