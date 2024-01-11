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

(define-module (my-guix packages rust)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages rust)
  #:use-module (nonguix build-system binary)
  #:export (rust-binary))

;;; Origin channel link: https://git.sr.ht/~declantsien/guix-channel

;;; TODO binary-build-system does not support compiling to anything other than
;;; x86_64, apparently...  May have to be resolved upstream.

(define* (make-rust-binary version uri base32-hash
                           #:key
                           (outputs (list "out" "cargo")))
  (package
    (inherit rust)
    (name "rust-binary")
    (version version)
    (source (origin
              (method url-fetch)
              (uri uri)
              (sha256
               (base32 base32-hash))))
    (build-system binary-build-system)
    (outputs outputs)
    (supported-systems (list "x86_64-linux"
                             ;; TODO add i686 support
                             ;; "i686-linux"
                             ))
    (native-inputs (modify-inputs (package-native-inputs rust)
                     (append `(,gcc "lib"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs rust)
                         (append gcc-toolchain)))
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
                        (target (assoc-ref outputs name)))

                   (if (equal? "out" name)
                       (copy-recursively "rust-std-x86_64-unknown-linux-gnu/lib/"
                                         (string-append target "/lib/")))
                   (if (equal? "rust-analyzer-preview" name)
                       (copy-recursively "rust-analysis-x86_64-unknown-linux-gnu/lib/"
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
                                        (string-append (assoc-ref outputs "out") "/lib")))

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

(define-public (rust-stable-x86_64-linux version base32)
  (let ((uri (string-append "https://static.rust-lang.org/dist/rust-"
                            version "-x86_64-unknown-linux-gnu.tar.gz")))
    (make-rust-binary version uri base32
                      #:outputs (list "out"
                                      "cargo"
                                      "rust-docs"
                                      "rust-docs-json-preview"
                                      "clippy-preview"
                                      "llvm-tools-preview"
                                      "miri-preview"
                                      "rls-preview"
                                      "rust-analyzer-preview"
                                      "rust-demangler-preview"
                                      "rustfmt-preview"))))

(define-public (rust-stable-i686-linux version base32)
  (let ((uri (string-append "https://static.rust-lang.org/dist/rust-"
                            version "-i686-unknown-linux-gnu.tar.gz")))
    (make-rust-binary version uri base32
                      #:outputs (list "out"
                                      "cargo"
                                      "rust-docs"
                                      "rust-docs-json-preview"
                                      "clippy-preview"
                                      "llvm-tools-preview"
                                      "miri-preview"
                                      "rls-preview"
                                      "rust-analyzer-preview"
                                      "rust-demangler-preview"
                                      "rustfmt-preview"))))

(define-public rust-binary
  (rust-stable-x86_64-linux
   "1.75.0"
   "1xkl1p7yhijbj7krcqcnbbwkqs3b3hhib4z8z64n68gzz2v7hfa7"))

;;; TODO Attempt to combine with rust-binary
;;;
;;; This does not actually work right now.
(define rust-stable-1.75.0-i686-linux
  (package
    (inherit (rust-stable-i686-linux
              "1.75.0"
              "13v7qrbhjvz98w47ji26nddj59lxh93z05cb6f7k7ayy4n48syqh"))
    (name "rust-i686")))
