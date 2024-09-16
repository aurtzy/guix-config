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

(define-public meson-1.3
  (package
    (inherit meson)
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ajvkcyly1nsxwjc2vly1vlvfjrwpfnza5prfr104wxhr18b8bj9"))))))

(define-public llvm-for-mesa/newer
  ;; Note: update the 'clang' input of mesa-opencl when bumping this.
  (let ((base-llvm llvm-18))
    (package
      (inherit base-llvm)
      (name "llvm-for-mesa")
      (arguments
       (substitute-keyword-arguments (package-arguments base-llvm)
         ((#:modules modules '((guix build cmake-build-system)
                               (guix build utils)))
          `((ice-9 regex)
            (srfi srfi-1)
            (srfi srfi-26)
            ,@modules))
         ((#:configure-flags cf ''())
          #~(cons*
             #$@(if (%current-target-system)
                    '("-DBUILD_SHARED_LIBS:BOOL=TRUE"
                      "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE")
                    '())
             ;; Skipping tools and utils decreases the output by ~100 MiB.
             "-DLLVM_BUILD_TOOLS=NO"
             (remove
              (cut string-match
                   #$(if (%current-target-system)
                         "-DLLVM_(LINK_LLVM_DYLIB|INSTALL_UTILS).*"
                         "-DLLVM_INSTALL_UTILS.*") <>)
              #$cf)))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              #$@(if (%current-target-system)
                     '()
                     #~((add-after 'install 'delete-static-libraries
                          ;; If these are just relocated then llvm-config
                          ;; can't find them.
                          (lambda* (#:key outputs #:allow-other-keys)
                            (for-each delete-file
                                      (find-files
                                       (string-append
                                        (assoc-ref outputs "out") "/lib")
                                       "\\.a$"))))))
              ;; llvm-config is how mesa and others find the various
              ;; libraries and headers they use.
              (add-after 'install 'build-and-install-llvm-config
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (substitute*
                      "tools/llvm-config/CMakeFiles/llvm-config.dir/link.txt"
                      (((string-append (getcwd) "/build/lib"))
                       (string-append out "/lib")))
                    (invoke "make" "llvm-config")
                    (install-file "bin/llvm-config"
                                  (string-append out "/bin"))))))))))))

(define-public libdrm/newer
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.122")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0lgjj9ign3cl27hzmqnxr7xwv21mm486pifc029wmzvxfydhgxfr"))))))

(define-public wayland-protocols/newer
  (package/inherit wayland-protocols
    (name "wayland-protocols")
    (version "1.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.freedesktop.org/wayland/"
                           name "/-/releases/" version "/downloads/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1sxgvis0abkymc02nhx2svm60myiq3shvy759sphpxl5rp52g6y5"))))))

(define-public spirv-llvm-translator-15
  ;; Use commit from branch llvm_release_150 instead of tag due to issue
  ;; (causing "not found" error) in headers:
  ;; https://github.com/KhronosGroup/SPIRV-LLVM-Translator/issues/2261
  ;;
  ;; Mesa requires version < 15.1, so current Guix package cannot be used
  (package
    (name "spirv-llvm-translator")
    (version "15.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
             (commit "c8597d16bbfa7f7d2c2f49c2757344276c315a8a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z0cwpw9scrr7zc3ij2hp39vbmz93zkb347pf6xdysr4bpkhsdrs"))))
    (build-system cmake-build-system)
    (arguments
     ;; 7 tests fail due to issue found here:
     ;; https://github.com/KhronosGroup/SPIRV-LLVM-Translator/pull/2555
     `(#:tests? #f
       #:configure-flags
       (list (string-append "-DLLVM_EXTERNAL_SPIRV_HEADERS_SOURCE_DIR="
                            (assoc-ref %build-inputs "spirv-headers")
                            "/include/spirv")
             (string-append "-DLLVM_EXTERNAL_LIT="
                            (assoc-ref %build-inputs "python-lit")
                            "/bin/lit")
             "-DLLVM_SPIRV_INCLUDE_TESTS=ON")))
    (inputs (list llvm-15))
    (native-inputs (list clang-15 llvm-15 python-lit spirv-headers))
    (home-page "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
    (synopsis "Bi-directional translation between SPIR-V and LLVM IR")
    (description
     "The LLVM/SPIR-V Bi-Directional Translator is a library and tool for
translation between LLVM IR and SPIR-V.")
    (license license:asl2.0)))

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
             (commit
              ;; "Bad", newer commit
              "2d10233f2f1e5b779dde5c5553de12de4cf9e351"
              ;; "Good", older commit (for Factorio)
              ;; See: https://gitlab.freedesktop.org/mesa/mesa/-/issues/11666
              ;; "67b778445afec51883e36618c8d5c535b3fd149f"
              )
             ))
       (file-name (git-file-name name "git"))
       (sha256 (base32
                ;; "Bad" commit
                "0fpxnwi7avvj8jw5f0yg6rdmlfqf1486217pvid337pdndnfj6qp"
                ;; "Good" commit
                ;; "1xpwikc3q9ann8mpasbki3mka85ypi6s2c2dzgxylyx7agap7lfw"
                ))
       (patches
        (list
         ;; Needs rebase, so comment out for now.
         ;; (origin
         ;;   (method url-fetch)
         ;;   (uri
         ;;    "https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/25576.patch")
         ;;   (sha256 (base32
         ;;            "0vcifnca44b27plb33r8kslna6qxj3ss9iwz70pq18ar1g7br1wv")))
         ))
       ))
    (arguments
     (cons*
      #:meson meson-1.3
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build utils)
                           (my-guix build utils))
      (substitute-keyword-arguments (package-arguments mesa)
        ((#:modules original-modules)
         (append original-modules
                 '((my-guix build utils))))
        ((#:configure-flags _)
         #~(list
            #$@(cond
                ((or (target-aarch64?) (target-arm32?))
                 '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,\
panfrost,r300,r600,svga,swrast,tegra,v3d,vc4,virgl,zink"))
                ((or (target-ppc64le?) (target-ppc32?) (target-riscv64?))
                 '("-Dgallium-drivers=nouveau,r300,r600,radeonsi,svga,swrast,virgl,zink"))
                (else
                 '("-Dgallium-drivers=nouveau,swrast,zink")
                 ;; TEMP: Reduce build time
                 ;; '("-Dgallium-drivers=crocus,iris,nouveau,r300,r600,radeonsi,\
                 ;; svga,swrast,virgl,zink")
                 ))
            ;; Enable various optional features.  TODO: opencl requires libclc,
            ;; omx requires libomxil-bellagio
            "-Dplatforms=x11,wayland"
            "-Dglx=dri"              ;Thread Local Storage, improves performance
            ;; "-Dopencl=true"
            ;; "-Domx=true"
            "-Dosmesa=true"
            "-Dgallium-xa=enabled"

            ;; features required by wayland
            "-Dgles2=enabled"
            "-Dgbm=enabled"
            "-Dshared-glapi=enabled"

            ;; Explicitly enable Vulkan on some architectures.
            #$@(cond
                ((target-x86-64?)
                 ;; TEMP: Reduce build time
                 ;; '("-Dvulkan-drivers=intel,intel_hasvk,amd,swrast,nouveau")
                 '("-Dvulkan-drivers=swrast,nouveau")
                 )
                ((target-x86-32?)
                 ;; TEMP: Reduce build time
                 ;; '("-Dvulkan-drivers=intel,intel_hasvk,amd,swrast")
                 '("-Dvulkan-drivers=swrast,nouveau"))
                ((or (target-ppc64le?) (target-ppc32?))
                 '("-Dvulkan-drivers=amd,swrast"))
                ((target-aarch64?)
                 '("-Dvulkan-drivers=freedreno,amd,broadcom,swrast"))
                ((target-riscv64?)
                 '("-Dvulkan-drivers=amd,swrast"))
                (else
                 '("-Dvulkan-drivers=auto")))

            ;; Enable the Vulkan overlay layer on all architectures.
            "-Dvulkan-layers=device-select,overlay"

            ;; Enable all the codecs that were built by default as part of the
            ;; 21.3.x releases to avoid functionality regressions.
            "-Dvideo-codecs=all"

            ;; Enable ZSTD compression for shader cache.
            "-Dzstd=enabled"

            ;; Also enable the tests.
            "-Dbuild-tests=true"

            "-Dllvm=enabled"))          ; default is x86/x86_64 only
        ((#:phases original-phases)
         #~(modify-phases #$original-phases
             (add-after 'unpack 'change-subproject-sources
               ;; Subproject source URLs are patched to point to the store,
               ;; which avoids an attempt to download them mid-build.
               (lambda _
                 #+(if (or (target-x86-64?) (target-x86-32?))
                       #~(for-each
                          (match-lambda
                            ((name source)
                             (patch-wrap-file name source)))
                          '#+(map (lambda (pkg)
                                    (let ((pkg
                                           (if (target-x86-32?)
                                               (package/with-rust-binary pkg)
                                               pkg)))
                                      (list (package-upstream-name* pkg)
                                            (crate-package-source pkg))))
                                  (list rust-syn-2
                                        rust-unicode-ident-1
                                        rust-quote-1
                                        rust-proc-macro2-1
                                        rust-paste-1)))
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
     (modify-inputs (package-native-inputs mesa)
       (prepend clang-15
                llvm-15
                python-ply
                python-pyyaml
                ;; Support 32-bit NVK with rust-binary
                (if (target-x86-32?) rust-binary rust)
                (if (target-x86-32?)
                    (package/with-rust-binary rust-bindgen-cli)
                    rust-bindgen-cli)
                (if (target-x86-32?)
                    (package/with-rust-binary rust-cbindgen-0.26)
                    rust-cbindgen-0.26))))
    (inputs
     (modify-inputs (package-inputs mesa)
       (prepend (package/inherit libclc
                  (name "libclc")
                  (propagated-inputs
                   (modify-inputs (package-propagated-inputs libclc)
                     (replace "spirv-llvm-translator"
                       spirv-llvm-translator-15))))
                wayland-protocols/newer)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs mesa)
       (replace "libdrm" libdrm/newer)))))

(define mesa/nvsa-git
  (package
    (inherit mesa)
    (replacement nvsa-git)))

(define-public replace-mesa->nvsa-git
  (package-input-rewriting `((,mesa . ,mesa/nvsa-git))))
