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
              "d3429a7e00dca7d101d8386db9d3f8b473f4e1e4")))
       (file-name (git-file-name name "git"))
       (sha256 (base32
                "1802r799ijci854qk1krgmyphw1l5lz12mp1v8w79d2637hckhrm"))
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
      #:meson meson-1.5
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build utils)
                           (my-guix build utils))
      (substitute-keyword-arguments (package-arguments mesa)
        ((#:modules original-modules)
         (append original-modules
                 '((my-guix build utils))))
        ((#:configure-flags original-flags)
         #~(append #$original-flags
                   ;; Only enable NVIDIA drivers to reduce build times
                   '#$(if (or (target-x86-64?) (target-x86-32?))
                          '("-Dgallium-drivers=nouveau,swrast,zink"
                            "-Dvulkan-drivers=swrast,nouveau")
                          '())))
        ((#:phases original-phases)
         #~(modify-phases #$original-phases
             (add-after 'unpack 'change-subproject-sources
               ;; Subproject source URLs are patched to point to the
               ;; store, which avoids an attempt to download them
               ;; mid-build.
               (lambda _
                 (for-each
                  (match-lambda
                    ((name source)
                     (patch-wrap-file name source)))
                  '#+(map (lambda (pkg)
                            (let ((pkg (if (target-x86-32?)
                                           (package/with-rust-binary pkg)
                                           pkg)))
                              (list (package-upstream-name* pkg)
                                    (crate-package-source pkg))))
                          (list rust-syn-2
                                rust-unicode-ident-1
                                rust-quote-1
                                rust-proc-macro2-1
                                rust-paste-1)))))
             (add-before 'build 'patch-out-rustfmt
               (lambda _
                 ;; XXX: Patch out rustfmt call, which appears to require rust
                 ;; nightly to build.  I don't know what consequences this may
                 ;; have.
                 (substitute* "../source/src/nouveau/headers/lib_rs_gen.py"
                   (("subprocess\\.run\\(\\['rustfmt',.*$")
                    "pass\n")))))))))
    (native-inputs
     (let ((native-inputs (modify-inputs (package-native-inputs mesa)
                            (prepend clang-18
                                     libclc
                                     python-ply
                                     python-pyyaml))))
       (cond
        ((target-x86-64?)
         (modify-inputs native-inputs
           (prepend rust
                    rust-bindgen-cli
                    rust-cbindgen-0.26)))
        ;; Support NVK on x86_32 arch by using rust-binary
        ((target-x86-32?)
         (modify-inputs native-inputs
           (prepend rust-binary
                    (package/with-rust-binary rust-bindgen-cli)
                    (package/with-rust-binary rust-cbindgen-0.26))))
        (else
         native-inputs))))
    (inputs
     (modify-inputs (package-inputs mesa)
       (replace "llvm-for-mesa" llvm-for-mesa/newer)
       (replace "wayland-protocols" wayland-protocols/newer)))))

(define mesa/nvsa-git
  (package
    (inherit mesa)
    (replacement nvsa-git)))

(define-public replace-mesa->nvsa-git
  (package-input-rewriting `((,mesa . ,mesa/nvsa-git))))
