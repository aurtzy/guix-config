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
;;; Copyright © 2021-2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024-2025 aurtzy <aurtzy@gmail.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (my-guix packages rust)
  #:use-module (my-guix utils))

(define patch-cargo-wrap-files-gexp
  #~(let ((wrap-name-regexp (make-regexp "^(.*)-([^-]*)-rs$"))
          (guix-vendored-name-regexp (make-regexp "^rust-.+-([0-9]+(\\.[0-9]+)*)")))
      (lambda* (#:key (vendor-dir "guix-vendor") #:allow-other-keys)
        "Unbundle Cargo/Rust dependencies (subprojects) and point Meson to our own
vendored inputs."
        (define (find-vendored-input name version-prefix)
          (let* ((guix-downstream-namever
                  (string-append "rust-" (string-replace-substring name  "_" "-")
                                 "-" version-prefix))
                 (matches
                  (scandir vendor-dir (cut string-match
                                           (regexp-quote guix-downstream-namever)
                                           <>))))
            (match matches
              (() #f)
              ;; Pick the first match, assuming it's the correct one.
              ((input . _)
               ;; This is slightly hacky: since the only information we have about
               ;; the rust inputs are the files themselves, we figure out the full
               ;; version by parsing it out of the matched directory.
               (let* ((m (regexp-exec guix-vendored-name-regexp input))
                      (version (match:substring m 1)))
                 (list input version))))))

        ;; Set vendor directory as a location that Meson will use to search for
        ;; names corresponding to the "directory" key in wrap file.
        (setenv "MESON_PACKAGE_CACHE_DIR" vendor-dir)
        (for-each
         (lambda (wrap-file)
           (let* ((wrap-name (basename wrap-file ".wrap"))
                  (m (regexp-exec wrap-name-regexp wrap-name))
                  (name (match:substring m 1))
                  (version-prefix (match:substring m 2))
                  (overlay-dir (string-append
                                "subprojects/packagefiles/" wrap-name)))
             (match (find-vendored-input name version-prefix)
               ((input version)
                ;; Don't use any bundled dependencies.
                (invoke "meson" "subprojects" "purge" "--confirm" wrap-name)
                (when (file-exists? overlay-dir)
                  ;; Adjust subproject's meson.build file to have the correct
                  ;; version associated with input.
                  (with-directory-excursion overlay-dir
                    (invoke "meson" "rewrite" "kwargs"
                            "set" "project" "/" "version" version)))
                ;; Patch local source in wrap file.
                (substitute* wrap-file
                  (("^source.*$") "")
                  (("^directory.*$")
                   (string-append "directory = " input "\n")))
                ;; "Download" source from the patched-in local path.
                (invoke "meson" "subprojects" "download" wrap-name))
               (else
                (format #t "Vendored input for ~s was not found~%" name)
                #f))))
         (if (file-exists? "subprojects")
             ;; Meson uses the naming scheme "{NAME}-{VERSIONPREFIX}-rs" for
             ;; Cargo/Rust dependencies.
             (with-directory-excursion "subprojects"
               (and=> (scandir "." (cut string-match "-rs\\.wrap$" <>))
                      (cut map canonicalize-path <>)))
             '())))))

(define-public nvsa-git
  ;; slimmed mesa git version for NVIDIA drivers.
  (package
    (inherit mesa)
    (name "nvsa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.freedesktop.org/mesa/mesa.git")
              (commit "39a7d65113c6df7d1da6e88b16fab0686db6fc83")))
       (file-name (git-file-name name "git"))
       (sha256
        (base32 "0aqhssmwdcwgzysjzabr95lxmbv35kmcp3zajllb04xn4fw0sgyw"))))
    (arguments
     (cons*
      #:imported-modules `(,@%meson-build-system-modules
                           ,@%cargo-build-system-modules
                           (guix build utils)
                           (my-guix build utils))
      (substitute-keyword-arguments (package-arguments mesa)
        ((#:modules original-modules)
         (append original-modules
                 '(((guix build cargo-build-system) #:prefix cargo:)
                   (ice-9 ftw)
                   (ice-9 string-fun)
                   (ice-9 regex)
                   (my-guix build utils)
                   (srfi srfi-26))))
        ((#:configure-flags original-flags)
         #~(append (lset-difference equal? #$original-flags
                                    ;; These flags were removed upstream.
                                    '("-Dgallium-xa=enabled" "-Dosmesa=true"))
                   ;; Only enable NVIDIA drivers to reduce build times
                   '#$(if (or (target-x86-64?) (target-x86-32?))
                          '("-Dgallium-drivers=nouveau,llvmpipe,zink"
                            "-Dvulkan-drivers=swrast,nouveau")
                          '())))
        ((#:phases original-phases)
         #~(modify-phases #$original-phases
             (delete 'patch-subproject-sources)
             (add-before 'configure 'patch-cargo-wrap-files
               #$patch-cargo-wrap-files-gexp)
             (add-before 'patch-cargo-wrap-files 'vendor-cargo-inputs
               (lambda args
                 (apply (assoc-ref cargo:%standard-phases 'configure) args)))
             (add-before 'build 'patch-out-rustfmt
               (lambda _
                 ;; XXX: Patch out rustfmt call, which appears to require rust
                 ;; nightly to build.  I don't know what consequences this may
                 ;; have.
                 (substitute* "../source/src/nouveau/headers/lib_rs_gen.py"
                   (("subprocess\\.run\\(\\['rustfmt',.*$")
                    "pass\n")))))))))
    (native-inputs
     ;; Support NVK on x86_32 arch by using rust-binary
     (if (target-x86-32?)
         (modify-inputs native-inputs
           (prepend rust-binary
                    (package/with-rust-binary rust-bindgen-cli)
                    (package/with-rust-binary rust-cbindgen-0.26)))
         (modify-inputs (package-native-inputs mesa)
           (replace "rust" rust-binary)
           (replace "rust-bindgen-cli"
             (package/with-rust-binary rust-bindgen-cli))
           (replace "rust-cbindgen"
             (package/with-rust-binary rust-cbindgen-0.26)))))
    (inputs
     ;; HACK: modify-inputs doesn't support adding lists, so we add input
     ;; labels manually before combining lists.
     (append ((@@ (guix packages) maybe-add-input-labels)
              (cargo-inputs 'mesa #:module '(my-guix packages rust-crates)))
             (package-inputs mesa)))))

(define mesa/nvsa-git
  (package
    (inherit mesa)
    (replacement nvsa-git)))

(define-public replace-mesa->nvsa-git
  (package-input-rewriting `((,mesa . ,mesa/nvsa-git))))

(define-public nvsa-git-with-libglvnd
  (package/inherit nvsa-git
    (name "nvsa")
    (inputs (modify-inputs (package-inputs nvsa-git)
              (prepend libglvnd)))
    (properties (cons `(hidden? . #t)
                      (package-properties nvsa-git)))))

(define mesa/nvsa-git-with-libglvnd
  (package/inherit mesa
    (replacement nvsa-git-with-libglvnd)))

(define-public replace-mesa->nvsa-git-with-libglvnd
  (package-input-rewriting `((,mesa . ,mesa/nvsa-git-with-libglvnd))))
