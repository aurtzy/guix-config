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
  #:use-module (gnu packages xdisorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
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

(define-public libdrm/newer
  (package
    (inherit libdrm)
    (version "2.4.120")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0yijzgg6rdsa68bz03sw0lcfa2nclv9m3as1cja50wkcyxim7x9v"))))))

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

(define (patch-crate-wrap-file-script wrap-file package)
  "Writes a wrap file for PACKAGE at path WRAP-FILE to enable using the
non-pinned package version in meson builds."
  (let* ((crate-name (package-upstream-name* package))
         (crate-full-name (string-append
                           crate-name "-" (package-version package)))
         (crate #~#$(file-append package
                                 "/share/cargo/src/"
                                 crate-full-name))
         (wrap-dir (dirname wrap-file)))
    #~(call-with-output-file #$wrap-file
        (lambda (port)
          (copy-recursively #$crate
                            #$(string-append wrap-dir "/" crate-full-name))
          (copy-recursively #$(string-append wrap-dir
                                             "/packagefiles/"
                                             crate-name)
                            #$(string-append wrap-dir "/" crate-full-name))
          (format
           port
           "[wrap-file]
directory = ~a
patch_directory = ~a
"
           #$crate-full-name
           #$crate-name)))))

(define-public mesa-nvk-git
  (let ((name "mesa-nvk-git")
        (version "24.1")
        (revision "0")
        (commit "1fb74672a91b28cc62f2bcb881047135f0258a8c"))
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
                  "0yi51ip6jb26k3s9cqszc55qfp084iqxlyr2ch8wqy40xqkyy3bq"))))
      (arguments
       (cons*
        #:meson meson-1.3
        (substitute-keyword-arguments (package-arguments mesa)
          ((#:configure-flags original-flags)
           #~(append #$original-flags
                     '("-Dvulkan-drivers=nouveau")))
          ((#:phases original-phases)
           #~(modify-phases #$original-phases
               (add-after 'unpack 'change-subproject-sources
                 ;; Subproject source URLs are patched to point to the store,
                 ;; which avoids an attempt to download them mid-build.
                 (lambda _
                   #$@(if (target-x86-64?)
                          (list
                           (patch-crate-wrap-file-script
                            "subprojects/syn.wrap"
                            rust-syn-2)
                           (patch-crate-wrap-file-script
                            "subprojects/unicode-ident.wrap"
                            rust-unicode-ident-1)
                           (patch-crate-wrap-file-script
                            "subprojects/quote.wrap"
                            rust-quote-1)
                           (patch-crate-wrap-file-script
                            "subprojects/proc-macro2.wrap"
                            rust-proc-macro2-1)
                           (patch-crate-wrap-file-script
                            "subprojects/paste.wrap"
                            rust-paste-1))
                          '()))))))))
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
         (prepend libclc
                  wayland-protocols/newer)))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs mesa)
         (replace "libdrm" libdrm/newer))))))
