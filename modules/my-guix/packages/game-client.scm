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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; Upstream strongly recommends using some of its pinned dependencies due to
;; relying on unstable/fork-specific features; these should be checked when
;; updating gamescope.  See:
;; <https://github.com/ValveSoftware/gamescope/commit/7741cd587fa2274989f3307a3c6f23ab08e98460>
(define %gamescope-version "3.16.17")

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
          (commit "54e844748029d4874e14d0c086d50092c04c8899")))
    (file-name (git-file-name "wlroots-for-gamescope" %gamescope-version))
    (sha256 (base32 "0sxgs157nzm6bkfyzh4dnl9zajg2bq1m1kq09xpxi2lm8ran3g05"))))

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
        (base32 "1b0w3is5carascz3salxlp57m8qbs2p4y8n42j2y7gkh79qdi7c9"))
       (patches
        (search-my-patches
         ;; Provide -Dglm_include_dir and -Dstb_include_dir configure flags.
         ;; See: https://github.com/ValveSoftware/gamescope/pull/1846
         "0001-build-add-options-to-override-subproject-paths.patch"))
       (modules '((guix build utils)
                  (ice-9 match)))
       (snippet
        #~(begin
            ;; Add pinned dependencies to the source tree where they're
            ;; expected.
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
      #:configure-flags
      #~(list "-Denable_openvr_support=false"
              (string-append "-Dglm_include_dir="
                             #+(this-package-input "glm")
                             "/include")
              "-Dpipewire=enabled"
              (string-append "-Dstb_include_dir="
                             #+(directory-union
                                "stb-for-gamescope"
                                (map (cut this-package-native-input <>)
                                     (list "stb-image"
                                           "stb-image-write"
                                           "stb-image-resize")))
                             "/include"))
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-usr-dir
            (lambda _
              ;; FIXME: I'm pretty sure this doesn't actually do anything
              ;; because there are no shaders provided with gamescope at
              ;; #$output/share/gamescope/reshade.  Maybe configure search
              ;; paths for this package so shaders can be provided in their
              ;; own packages?
              (substitute* "src/reshade_effect_manager.cpp"
                ;; Search for shaders in output instead of /usr.
                (("return \"/usr\";")
                 (string-append "return \"" #$output "\";")))))
          (add-after 'unpack 'patch-gamescopereaper
            (lambda _
              (substitute* "src/Utils/Process.cpp"
                ;; Explicitly use gamescopereaper from output to avoid PATH.
                (("\"gamescopereaper\"")
                 (string-append "\"" #$output "/bin/gamescopereaper" "\"")))))
          (add-after 'unpack 'patch-loader-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/rendervulkan.cpp"
                ;; Fix "Failed to load vulkan module" error.
                ;; Related issue: https://issues.guix.gnu.org/71109
                (("dlopen\\( \"libvulkan\\.so")
                 (string-append "dlopen( \""
                                (search-input-file
                                 inputs "/lib/libvulkan.so"))))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "src/meson.build"
                ;; This determines what `gamescope --version` prints.
                (("^vcs_tag = .*$")
                 (string-append
                  "vcs_tag = '" #$(package-version this-package) "'\n")))))
          (add-after 'unpack 'unbundle-spirv-headers
            (lambda _
              (delete-file-recursively "thirdparty/SPIRV-Headers")
              (copy-recursively #$(this-package-native-input "spirv-headers")
                                "thirdparty/SPIRV-Headers"))))))
    (native-inputs (list gcc
                         glslang
                         pkg-config
                         python-3
                         spirv-headers
                         stb-image
                         stb-image-write
                         stb-image-resize
                         vulkan-headers))
    (inputs (list benchmark
                  glm
                  hwdata
                  lcms
                  libavif
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
    (synopsis "Micro-compositor for running games")
    (description
     "gamescope is a micro-compositor for running games.  Its goal is to
provide an isolated compositor that is tailored towards gaming and supports
many gaming-centric features such as:
@itemize
@item Spoofing resolutions.
@item Upscaling.
@item Limiting framerates.
@end itemize")
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
      (fhs-union (modify-inputs (ngc-packages container)
                   (prepend libglvnd)
                   (replace "mesa"
                     (package/inherit nvsa-git
                       (inputs (modify-inputs (package-inputs nvsa-git)
                                 (prepend libglvnd))))))))
     (union32
      ;; Avoid building 32-bit version of mesa normally unless a situation
      ;; calls for it.  Note that Guix's Rust does not build on i686-linux, so
      ;; an alternative (like rust-binary) must be used to enable 32-bit NVK.
      (ngc-union32 (steam-container-for mesa)))
     (exposed (cons* "/run/privileged/bin/gamescope"
                     (ngc-exposed container))))))

(define-public steam-custom
  (nonguix-container->package steam-container-custom))
