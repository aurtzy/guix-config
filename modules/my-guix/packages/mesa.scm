;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017-2019, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021, 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages mesa)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils))

(define-public rust-bindgen-cli
  (package/inherit rust-bindgen-0.64
    (name "rust-bindgen-cli")
    (version "0.64.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-lang/rust-bindgen.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14xbg4r1bcdg4ni4amz9f9fmbvb9q8m026vgcf5r0w184sdjg91l"))))
    (arguments
     (cons*
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-cc
            (lambda* (#:key inputs #:allow-other-keys)
              (symlink (string-append (assoc-ref inputs "gcc") "/bin/gcc")
                       "cc")
              (setenv "PATH" (string-append
                              (getenv "PATH")
                              ":"(canonicalize-path ".")))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (bindgen (string-append bin "/bindgen"))
                     (llvm-dir (string-append
                                (assoc-ref inputs "clang")
                                "/lib")))
                (mkdir-p bin)
                (copy-file "target/release/bindgen" bindgen)
                (wrap-program bindgen
                  `("LIBCLANG_PATH" = (,llvm-dir)))))))
      (substitute-keyword-arguments (package-arguments rust-bindgen-0.64)
        ((#:skip-build? _)
         #f)
        ((#:cargo-inputs original-inputs)
         `(("rust-bindgen" ,rust-bindgen-0.64)
           ("rust-diff" ,rust-diff-0.1)
           ("rust-quickcheck" ,rust-quickcheck-0.4)
           ("rust-tempdir" ,rust-tempdir-0.3)
           ("rust-block" ,rust-block-0.1)
           ("rust-objc" ,rust-objc-0.2)
           ,@original-inputs)))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs rust-bindgen-0.64)
       (append clang)))))

(define-public meson/newest
  (package/inherit meson/newer
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mra0gh5jz254p8wg2m25pazbvrqpqaq5qj1zga465nyvs5mc830"))))))

(define-public rust-syn-2/newer
  (let ((name (package-name rust-syn-2))
        (version "2.0.39"))
    (package/inherit rust-syn-2
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (crate-uri "syn" version))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32
           "0ymyhxnk1yi4pzf72qk3lrdm9lgjwcrcwci0hhz5vx7wya88prr3")))))))

(define-public rust-proc-macro2-1/newer
  (let ((name (package-name rust-proc-macro2-1))
        (version "1.0.70"))
    (package/inherit rust-proc-macro2-1
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (crate-uri "proc-macro2" version))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32
           "0fzxg3dkrjy101vv5b6llc8mh74xz1vhhsaiwrn68kzvynxqy9rr")))))))

;; Reference package: https://aur.archlinux.org/packages/vulkan-nouveau-git
;;
;; TODO Consider optimizations like the AUR package has done.
(define-public mesa-git
  (let ((version "23.3")
        (revision "0")
        (commit "a84729d36866bc79619523065a6038c3d8444f97"))
    (package/inherit mesa
      (name "mesa-git")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/mesa/mesa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32
                  "0p3hv2x7icxvpirl0mnf0xm2qqlx630ccdpk1y1bpj71n6hh6d2g"))
         (patches
          (let* ((url "https://aur.archlinux.org/cgit/aur.git/plain")
                 (id "be7fab11e95214db6895b7de57b6017fd9ce7ca3")
                 (patch-uri
                  (lambda (file)
                    (format #f "~a/~a.patch?h=vulkan-nouveau-git&id=~a"
                            url file id))))
            (list (origin
                    (method url-fetch)
                    (uri (patch-uri "nvk-memory-budget"))
                    (file-name "nvk-memory-budget.patch")
                    (sha256
                     (base32
                      "0aa3gnpv4xq4q10qnd57kf3njppca9zxwfmnmymgqcq8dcyb8rhz"))))))))
      (arguments
       (cons*
        #:meson meson/newest
        (substitute-keyword-arguments (package-arguments mesa)
          ((#:configure-flags original-flags)
           #~(append #$original-flags
                     '("-Dvulkan-drivers=nouveau-experimental")))
          ((#:phases original-phases)
           #~(modify-phases #$original-phases
               (add-after 'unpack 'change-subproject-sources
                 ;; As downloading is not allowed in the build, we 1. download
                 ;; them ahead of time via inputs, then 2. patch the defined
                 ;; URLs to point to the inputs in the Guix store.  This is
                 ;; done with the assumption that we are unable to properly
                 ;; build Rust dependencies using meson-build-system, so we
                 ;; rely on cargo to do it for us using its fallback method;
                 ;; however, this assumption is probably wrong, and this hacky
                 ;; code could probably be improved.
                 (lambda _
                   (for-each
                    (lambda (subproject)
                      (let ((file (car subproject))
                            (input (assoc-ref %build-inputs
                                              (cdr subproject))))
                        (substitute* file
                          (("https.*/download")
                           (string-append "file://" input)))))
                    '(("subprojects/syn.wrap"
                       . "rust-syn")
                      ("subprojects/unicode-ident.wrap"
                       . "rust-unicode-ident")
                      ("subprojects/quote.wrap"
                       . "rust-quote")
                      ("subprojects/proc-macro2.wrap"
                       . "rust-proc-macro2")))))
               (add-before 'configure 'fix-files-causing-errors
                 (lambda _
                   (map delete-file
                        (find-files "build/subprojects"
                                    "^.*\\.rlib$"))
                   (map delete-file
                        (find-files "build/src/nouveau/compiler"
                                    "^.*\\.rlib$")))))))))
      (native-inputs
       (cons* `("rust-syn"
                ,(package-source rust-syn-2/newer))
              `("rust-unicode-ident"
                ,(package-source rust-unicode-ident-1))
              `("rust-quote"
                ,(package-source rust-quote-1))
              `("rust-proc-macro2"
                ,(package-source rust-proc-macro2-1/newer))
              (modify-inputs (package-native-inputs mesa)
                (append rust rust-bindgen-cli)))))))
