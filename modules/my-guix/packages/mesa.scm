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
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-io)
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

(define-public meson-next
  (package/inherit meson
    (name "meson-next")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mesonbuild/meson/"
                           "releases/download/" version  "/meson-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bwfr2fm2vjv8qyl7nn5lb1q5dgan3cdf4na7p89nlbi28qj76qa"))))))

(define-public spirv-headers-next
  (package/inherit spirv-headers
    (name "spirv-headers-next")
    (version "1.4.304.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-Headers")
             (commit (string-append "vulkan-sdk-" version))))
       (sha256
        (base32 "1hlmjhavc6lyw39visr93rq4frrqxd785h7zci8pr3m6vj5kw91h"))
       (file-name (git-file-name name version))))))

(define-public spirv-tools-next
  (package/inherit spirv-tools
    (name "spirv-tools-next")
    (version "1.4.304.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/KhronosGroup/SPIRV-Tools")
            (commit (string-append "vulkan-sdk-" version))))
      (sha256
       (base32 "08ipi93bi9idp8rvgmsv2l8k0gdjvnc7cid49q8knkwvp9gphlka"))
      (file-name (git-file-name name version))))
    (inputs (modify-inputs (package-inputs spirv-tools)
              (replace "spirv-headers" spirv-headers-next)))))

(define-public wayland-protocols-next
  (package/inherit wayland-protocols
    (name "wayland-protocols-next")
    (version "1.41")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "148wh3cw88pv1adbhmkr13ass2vznzpa03hc3f6hwmwfv4bjsdlr"))))))

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
             (commit "89f3ee4cb2e71dfe13812bae7d754bbe79596bcb")))
       (file-name (git-file-name name "git"))
       (sha256 (base32
                "1ra9m2sxsjja65qc7n6idqca45c58ki16x6dbgzjd0sbisa7bvmp"))))
    (arguments
     (cons*
      #:meson meson-next
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build utils)
                           (my-guix build utils))
      (substitute-keyword-arguments (package-arguments mesa)
        ((#:modules original-modules)
         (append original-modules
                 '((my-guix build utils))))
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
             #$@(let ((patch-subproject-sources
                       ;; Subproject source URLs are patched to point to the
                       ;; store, which avoids an attempt to download them
                       ;; mid-build.
                       #~(lambda _
                           (for-each
                            (match-lambda
                              ((name source)
                               (patch-wrap-file name source)))
                            '#+(map (lambda (pkg)
                                      (let ((pkg (package/with-rust-binary pkg)))
                                        (list (package-upstream-name* pkg)
                                              (crate-package-source pkg))))
                                    (list rust-syn-2
                                          rust-unicode-ident-1
                                          rust-quote-1
                                          rust-proc-macro2-1
                                          rust-paste-1
                                          rust-rustc-hash-2))))))
                  (cond
                   ((target-x86-32?)
                    #~((add-after 'unpack 'patch-subproject-sources
                         #$patch-subproject-sources)))
                   ((target-x86-64?)
                    #~((replace 'patch-subproject-sources
                         #$patch-subproject-sources)))
                   (else
                    #~())))
             (add-before 'build 'patch-out-rustfmt
               (lambda _
                 ;; XXX: Patch out rustfmt call, which appears to require rust
                 ;; nightly to build.  I don't know what consequences this may
                 ;; have.
                 (substitute* "../source/src/nouveau/headers/lib_rs_gen.py"
                   (("subprocess\\.run\\(\\['rustfmt',.*$")
                    "pass\n")))))))))
    (native-inputs
     (let ((native-inputs
            (let ((replace-spirv-inputs
                   (package-input-rewriting/spec
                    `(("spirv-headers" . ,(const spirv-headers-next))
                      ("spirv-tools" . ,(const spirv-tools-next))))))
              (modify-inputs (package-native-inputs mesa)
                (prepend (replace-spirv-inputs libclc))))))
       ;; Support NVK on x86_32 arch by using rust-binary
       (if (target-x86-32?)
           (modify-inputs native-inputs
             (prepend rust-binary
                      (package/with-rust-binary rust-bindgen-cli)
                      (package/with-rust-binary rust-cbindgen-0.26)))
           ;; Mesa requires rust >= 1.78 now.
           (modify-inputs native-inputs
             (replace "rust" rust-binary)
             (replace "rust-bindgen-cli"
               (package/with-rust-binary rust-bindgen-cli))
             (replace "rust-cbindgen"
               (package/with-rust-binary rust-cbindgen-0.26))))))
    (inputs
     (modify-inputs (package-inputs mesa)
       (replace "wayland-protocols" wayland-protocols-next)))))

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
