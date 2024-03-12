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
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public rust-bindgen-cli-0.69
  (package
    (inherit rust-bindgen-0.69)
    (name "rust-bindgen-cli")
    (version "0.69.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-lang/rust-bindgen.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mksvspqymdypgflgx6xqfxdr9a5wwx534imgcnn3mk7ffz0sqlm"))))
    (arguments
     (cons*
      ;; #:rust (match (or (%current-target-system)
      ;;                   (%current-system))
      ;;          ("x86_64-linux" rust-binary-x86_64)
      ;;          ("i686-linux" rust-binary-i686))

      ;; 1 test case fails (header_ptr32_has_different_size_h); related issue:
      ;; https://github.com/rust-lang/rust-bindgen/issues/2638
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
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
      (substitute-keyword-arguments (package-arguments rust-bindgen-0.69)
        ((#:skip-build? _)
         #f)
        ((#:cargo-inputs original-inputs)
         `(("rust-bindgen" ,rust-bindgen-0.69)
           ("rust-block" ,rust-block-0.1)
           ("rust-clap" ,rust-clap-complete-4)
           ("rust-env-logger" ,rust-env-logger-0.10)
           ("rust-libloading" ,rust-libloading-0.7)
           ("rust-objc" ,rust-objc-0.2)
           ("rust-owo-colors" ,rust-owo-colors-3)
           ("rust-prettyplease" ,rust-prettyplease-0.2)
           ("rust-quickcheck" ,rust-quickcheck-0.4)
           ("rust-similar" ,rust-similar-2)
           ,@original-inputs)))))
    (native-inputs
     (modify-inputs (package-native-inputs rust-bindgen-0.69)
       (prepend clang)))))

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

(define-public rust-syn-2.0.39
  (package
    (inherit rust-syn-2)
    (name "rust-syn")
    (version "2.0.39")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ymyhxnk1yi4pzf72qk3lrdm9lgjwcrcwci0hhz5vx7wya88prr3"))))))

(define-public rust-proc-macro2-1.0.70
  (package
    (inherit rust-proc-macro2-1)
    (name "rust-proc-macro2")
    (version "1.0.70")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fzxg3dkrjy101vv5b6llc8mh74xz1vhhsaiwrn68kzvynxqy9rr"))))))

(define-public rust-quote-1.0.33
  (package
    (inherit rust-quote-1)
    (name "rust-quote")
    (version "1.0.33")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quote" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1biw54hbbr12wdwjac55z1m2x2rylciw83qnjn564a3096jgqrsj"))))))

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

;; Reference package: https://aur.archlinux.org/packages/vulkan-nouveau-git
;;
;; TODO Consider optimizations like the AUR package has done.
(define-public mesa-git
  (let ((name "mesa-git")
        (version "24.0")
        (revision "0")
        (commit "626502d7c7f6b2266e2bdc7979e512b7a7292e44"))
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
                  "0zry20yj5i8n9z0pch57aa34a60rjsp0qmciijx3php1kqqpl9da"))))
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
                   (for-each
                    (lambda (subproject)
                      (let ((file (car subproject))
                            (input (cdr subproject)))
                        (substitute* file
                          (("https.*/download")
                           (string-append "file://" input)))))
                    '#$(if (target-x86-64?)
                           #~(("subprojects/syn.wrap"
                               . #$(package-source rust-syn-2.0.39))
                              ("subprojects/unicode-ident.wrap"
                               . #$(package-source rust-unicode-ident-1))
                              ("subprojects/quote.wrap"
                               . #$(package-source rust-quote-1.0.33))
                              ("subprojects/proc-macro2.wrap"
                               . #$(package-source rust-proc-macro2-1.0.70)))
                           '())))))))))
      (native-inputs
       (modify-inputs (package-native-inputs mesa)
         (prepend rust
                  rust-bindgen-cli-0.69
                  clang-15
                  llvm-15
                  python-ply)))
      (inputs
       (modify-inputs (package-inputs mesa)
         (prepend libclc)))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs mesa)
         (replace "libdrm" libdrm/newer))))))
