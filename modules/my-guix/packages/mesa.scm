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
  #:use-module (guix build-system meson)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (my-guix utils))

(define-public meson-1.3
  (package
    (inherit meson/newer)
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ajvkcyly1nsxwjc2vly1vlvfjrwpfnza5prfr104wxhr18b8bj9"))))))

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

(define-public spirv-llvm-translator/newer
  ;; Current Guix version does not build due to a name mismatch (causing "not
  ;; found" error) in headers:
  ;; https://github.com/KhronosGroup/SPIRV-LLVM-Translator/issues/2261
  (package/inherit spirv-llvm-translator
    (name "spirv-llvm-translator")
    (version "15.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
             ;; Take commit from llvm_release_150 branch
             (commit "a12739b11c191605a11bfa7bab92c040e7c53344")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a84p01y40wf31jmh05yq094zzna09b9dvj4ymzp8pn0222jxgdf"))))))

(define-public mesa-nvk-git
  (let ((name "mesa-nvk-git")
        (version "24.1")
        (revision "0")
        (commit "d77a1762bd3da3216b7935ac856ba3f56c61f64d"))
    (package
      (inherit mesa)
      (name name)
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/mesa/mesa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32
                  "1yc2ryrqkb51drv2fagw25f45garnngsvj81wapwmlrzikacl2kb"))))
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
          ((#:configure-flags original-flags)
           #~(append #$original-flags
                     '("-Dvulkan-drivers=nouveau")))
          ((#:phases original-phases)
           #~(modify-phases #$original-phases
               (add-after 'unpack 'change-subproject-sources
                 ;; Subproject source URLs are patched to point to the store,
                 ;; which avoids an attempt to download them mid-build.
                 (lambda _
                   #+(if (target-x86-64?)
                         #~(for-each
                            (match-lambda
                              ((name source)
                               (patch-wrap-file name source)))
                            '#+(map (lambda (pkg)
                                      (list (package-upstream-name* pkg)
                                            (crate-package-source pkg)))
                                    (list rust-syn-2
                                          rust-unicode-ident-1
                                          rust-quote-1
                                          rust-proc-macro2-1
                                          rust-paste-1)))
                         #~()))))))))
      (native-inputs
       (modify-inputs (package-native-inputs mesa)
         (prepend clang-15
                  llvm-15
                  python-ply
                  rust
                  rust-bindgen-cli
                  rust-cbindgen-0.26)))
      (inputs
       (modify-inputs (package-inputs mesa)
         (prepend (package/inherit libclc
                    (name "libclc")
                    (propagated-inputs
                     (modify-inputs (package-propagated-inputs libclc)
                       (replace "spirv-llvm-translator"
                                spirv-llvm-translator/newer))))
                  wayland-protocols/newer))))))
