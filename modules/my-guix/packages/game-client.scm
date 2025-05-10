;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Kozo <kozodev@runbox.com>
;;; Copyright © 2021, 2022, 2023, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Elijah Malaby
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2024-2025 aurtzy <aurtzy@gmail.com>
;;; Copyright © 2024 dan <i@dan.games>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages game-client)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix utils)
  #:use-module (nonguix multiarch-container)
  #:use-module (nongnu packages game-client)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define stb-deprecated
  (let ((stb (@@ (gnu packages stb) stb)))
    (package
      (inherit stb)
      (name "stb-deprecated")
      (arguments
       (substitute-keyword-arguments (package-arguments stb)
         ((#:phases original-phases)
          #~(modify-phases #$original-phases
              (replace 'install
                (lambda _
                  (let ((files (make-regexp "\\.(c|h|md)$")))
                    (with-directory-excursion "deprecated"
                      (for-each (lambda (file)
                                  (install-file file #$output))
                                (scandir "." (cut regexp-exec files <>))))
                    #t))))))))))

(define (make-deprecated-stb-header-package name version description)
  (package
    (inherit stb-deprecated)
    (name name)
    (version version)
    (source #f)
    (inputs (list stb-deprecated))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((stb (assoc-ref %build-inputs "stb-deprecated"))
                         (lib (string-join (string-split ,name #\-) "_"))
                         (out (assoc-ref %outputs "out")))
                     (install-file (string-append stb "/" lib ".h")
                                   (string-append out "/include"))
                     #t))))
    (description description)))

(define-public stb-image-resize
  (make-deprecated-stb-header-package
   "stb-image-resize" "0.97"
   "stb-image-resize is a library that supports scaling and translation of
images."))

(define-public libavif-1.0
  (package
    (inherit libavif)
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AOMediaCodec/libavif")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name libavif) version))
              (sha256
               (base32 "0k72q7yvfdn92wkslyifw14319nm981a8r3kd84i4ylxmrkgi0zm"))))))

;; Upstream strongly recommends using some of its pinned dependencies due to
;; relying on unstable features; these should be checked when updating
;; gamescope.  See:
;; <https://github.com/ValveSoftware/gamescope/commit/7741cd587fa2274989f3307a3c6f23ab08e98460>
(define %gamescope-version "3.16.4")

(define libliftoff-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.freedesktop.org/emersion/libliftoff.git")
          (commit "8b08dc1c14fd019cc90ddabe34ad16596b0691f4")))
    (file-name (git-file-name "libliftoff-for-gamescope" %gamescope-version))
    (sha256 (base32 "163g8ndsbma7acy2k9mrnvlpb7yi4431hgkx1gygkafgwpq1ii1x"))))

(define reshade-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/reshade")
          (commit "696b14cd6006ae9ca174e6164450619ace043283")))
    (file-name (git-file-name "reshade-for-gamescope" %gamescope-version))
    (sha256
     (base32 "1zvhf3pgd8bhn8bynrsh725xn1dszsf05j8c9g6zabgv7vnz04a5"))))

(define vkroots-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/vkroots")
          (commit "5106d8a0df95de66cc58dc1ea37e69c99afc9540")))
    (file-name (git-file-name "vkroots-for-gamescope" %gamescope-version))
    (sha256 (base32 "0hrp0xqq93552ipw2bmryixgm1aywnz49xagsx5rwzg2d0hwa0aa"))))

(define wlroots-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/wlroots.git")
          (commit "4bc5333a2cbba0b0b88559f281dbde04b849e6ef")))
    (file-name (git-file-name "wlroots-for-gamescope" %gamescope-version))
    (sha256 (base32 "14m9j9qkaphzm3g36im43b6h92rh3xyjh7j46vw9w2qm602ndwcf"))))

;; From: https://gitlab.com/nonguix/nonguix/-/merge_requests/200
;;
;; XXX: When run inside steam container, sometimes complains about
;; /tmp/.X11-unix not belonging to root or user.
;; `sudo chown $USER /tmp/.X11-unix' fixes this as a workaround.
;; Similar related issue: https://github.com/NixOS/nixpkgs/issues/162562
;;
;; FIXME: When the cap_sys_nice capability is set, gamescope becomes unable to
;; find files like VkLayer_MESA_device_select.json (mesa has this).  Simple
;; test: Copy gamescope from store to anywhere.  See that it still runs.  Run
;; 'sudo setcap "cap_sys_nice=pie" gamescope' and see that attempting to run
;; again gives a VkResult -9 error.
;;
;; 'strace gamescope' shows that gamescope searches data directories pulled
;; from somewhere (what variable is used?) for these files.  The above test
;; notably causes gamescope to /not/ search any data directories belonging to
;; the user (security policy?  root owner things?); it succeeded before
;; because the files were present in ~/.guix-home/profile/share/vulkan/...,
;; which was searched.
;;
;; The following variables appear to be read: $XDG_CONFIG_DIRS,
;; $XDG_DATA_DIRS, $XDG_CONFIG_HOME
(define-public gamescope
  (package
    (name "gamescope")
    (version %gamescope-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ValveSoftware/gamescope")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09h7046vwqn0w3kv1zaij4h3rcrvs1r2qlm0vva3mk3gg44fnhjl"))
       (modules '((guix build utils)
                  (ice-9 match)))
       (snippet
        #~(begin
            ;; Add some dependencies to source tree where they're expected.
            (for-each (match-lambda
                        ((source dest)
                         (copy-recursively source dest)))
                      '((#$libliftoff-for-gamescope "subprojects/libliftoff")
                        (#$reshade-for-gamescope "src/reshade")
                        (#$vkroots-for-gamescope "subprojects/vkroots")
                        (#$wlroots-for-gamescope "subprojects/wlroots")))))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dpipewire=enabled"
                                "-Denable_openvr_support=false")
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (my-guix build utils))
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build utils)
                           (my-guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-usr-dir
            (lambda _
              (substitute* "src/reshade_effect_manager.cpp"
                (("/usr") #$output))))
          (add-after 'unpack 'patch-loader-path
            ;; TODO: Test this again and document what exactly this fixes.
            ;;
            ;; Related issue: https://issues.guix.gnu.org/71109
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/rendervulkan.cpp"
                (("dlopen\\( \"libvulkan\\.so")
                 (string-append "dlopen( \""
                                (search-input-file
                                 inputs "/lib/libvulkan.so"))))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "src/meson.build"
                (("^vcs_tag = .*$")
                 (string-append
                  "vcs_tag = '" #$(package-version this-package) "'\n")))))
          (add-after 'unpack 'patch-deps
            (lambda _
              ;; stb
              (patch-wrap-file
               "stb"
               #+(file-append
                  ;; TODO: reference with this-package-native-input instead
                  (directory-union "stb" (list stb-image
                                               stb-image-write
                                               stb-image-resize))
                  "/include"))
              ;; libdisplay-info
              (substitute* "src/meson.build"
                (("../thirdparty/SPIRV-Headers")
                 #$(this-package-native-input "spirv-headers"))))))))
    (native-inputs
     (list gcc-14
           glslang
           pkg-config
           python-3
           spirv-headers
           vulkan-headers))
    (inputs
     (list benchmark
           glm
           hwdata
           lcms
           libavif-1.0
           libdecor
           libdisplay-info
           libdrm
           libei
           libinput
           libseat
           libx11
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxkbcommon
           libxmu
           libxrender
           libxres
           libxt
           libxtst
           libxxf86vm
           luajit
           pipewire
           pixman
           sdl2
           vulkan-loader
           xcb-util-wm
           xcb-util-errors
           xorg-server-xwayland
           wayland
           wayland-protocols))
    (home-page "https://github.com/ValveSoftware/gamescope")
    (synopsis "Session compositing window manager")
    (description "Gamescope is a Wayland compositor for running games,
formerly known as steamcompmgr.  It is designed for use in embedded sessions
and as a nested compositor on top of a regular desktop environment through
sandboxed Xwayland sessions.")
    (license license:bsd-2)))

(define steam-client-custom
  (let ((steam-client (@@ (nongnu packages game-client) steam-client)))
    (package
      (inherit steam-client)
      (arguments
       (substitute-keyword-arguments (package-arguments steam-client)
         ((#:phases original-phases)
          #~(modify-phases #$original-phases
              (add-after 'install 'wrap-steam
                (lambda _
                  (wrap-program (string-append #$output "/bin/steam")
                    #:sh #$(file-append bash-minimal "/bin/bash")
                    '("QT_X11_NO_MITSHM" = ("1"))))))))))))

(define-public steam-container-custom
  (let ((steam-client-libs (@@ (nongnu packages game-client)
                               steam-client-libs))
        (steam-gameruntime-libs (@@ (nongnu packages game-client)
                                    steam-gameruntime-libs))
        (container (steam-container-for nvsa-git)))
    (nonguix-container
     (inherit container)
     (name "steam-custom")
     (binary-name "steam")
     (wrap-package steam-client-custom)
     (union64
      ;; TODO: Bug?  Look into potential bug with modify-inputs changing
      ;; output paths; seems to happen when there are multiple prepend
      ;; clauses.
      (fhs-union (modify-inputs `(,@steam-client-libs
                                  ,@steam-gameruntime-libs
                                  ,@fhs-min-libs)
                   ;; gamescope MUST produce have same derivation as the
                   ;; gamescope in privileged-programs so all of its
                   ;; dependencies can be found in the container.
                   (prepend (replace-mesa->nvsa-git gamescope)
                            (replace-mesa->nvsa-git sdl2)
                            ;; Debugging tools
                            ;; (@ (gnu packages gdb) gdb)
                            ;; (@ (gnu packages emacs) emacs)
                            )
                   (replace "mesa" nvsa-git))
                 #:name "fhs-union-64"))
     ;; Requires i686-linux rust; package upstream in Guix does not build, so a
     ;; binary version is required if we want 32-bit NVK for the time being.
     ;; (union32
     ;;  (fhs-union (modify-inputs `(,@steam-client-libs
     ;;                              ,@steam-gameruntime-libs
     ;;                              ,@fhs-min-libs)
     ;;               (prepend (replace-mesa->nvsa-git sdl2))
     ;;               (replace "mesa" nvsa-git-with-libglvnd))
     ;;             #:name "fhs-union-32"
     ;;             #:system "i686-linux"))
     (exposed (cons* "/run/privileged/bin/gamescope"
                     (ngc-exposed container))))))

(define-public steam-custom
  (nonguix-container->package steam-container-custom))
