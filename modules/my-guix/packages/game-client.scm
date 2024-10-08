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
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
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

;; From: https://gitlab.com/nonguix/nonguix/-/merge_requests/200
;;
;; XXX: When run inside steam container, sometimes complains about
;; /tmp/.X11-unix not belonging to root or user.
;; `sudo chown $USER /tmp/.X11-unix' fixes this as a workaround.
;; Similar related issue: https://github.com/NixOS/nixpkgs/issues/162562
(define-public gamescope
  (let ((version "3.14.20")
        (revision "0")
        (commit "650b0959a7677c89ea1395025ae9ebc84d982f0a"))
    (package
      (name "gamescope")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ValveSoftware/gamescope")
               (commit commit)
               (recursive? #t)))
         (sha256
          (base32 "10c4bk3ahphp4cg9fkpbd6wazx4q5r7b587l87wpyc477da0q6v8"))
         (file-name (git-file-name name version))))
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
            (add-after 'unpack 'patch-subprojects
              (lambda _
                ;; stb
                (patch-wrap-file
                 "stb"
                 #+(file-append
                    (directory-union "stb" (list stb-image
                                                 stb-image-write
                                                 stb-image-resize))
                    "/include"))
                ;; libdisplay-info
                (substitute* "src/meson.build"
                  ;; Allow newer versions
                  (("(version: \\['>= 0\\.0\\.0'), '< 0\\.2\\.0'(\\])"
                    _ left-part right-part)
                   (string-append left-part right-part))))))))
      (native-inputs
       (list benchmark
             glslang
             hwdata
             pkg-config
             vulkan-headers
             wayland-protocols/newer))
      (inputs
       (list (module-ref (resolve-interface '(gnu packages commencement))
                         'gcc-toolchain-12)
             glm
             libavif-1.0
             libdecor
             libdisplay-info
             libdrm
             libinput
             libseat
             libx11
             libxcomposite
             libxcursor
             libxdamage
             libxext
             libxkbcommon
             libxmu
             libxres
             libxt
             libxtst
             pipewire
             pixman
             python-3
             sdl2
             vulkan-loader
             xcb-util-wm
             xcb-util-errors
             xorg-server-xwayland
             wayland))
      (home-page "https://github.com/ValveSoftware/gamescope")
      (synopsis "Session compositing window manager")
      (description "Gamescope is a Wayland compositor for running games,
formerly known as steamcompmgr.  It is designed for use in embedded sessions
and as a nested compositor on top of a regular desktop environment through
sandboxed Xwayland sessions.")
      (license license:bsd-2))))

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
                                    steam-gameruntime-libs)))
    (nonguix-container
     (inherit steam-container)
     (name "steam-custom")
     (binary-name "steam")
     (wrap-package steam-client-custom)
     (union64
      (fhs-union (modify-inputs `(,@steam-client-libs
                                  ,@steam-gameruntime-libs
                                  ,@fhs-min-libs)
                   (prepend (@ (gnu packages gdb) gdb)
                            (replace-mesa->nvsa-git gamescope)
                            libglvnd
                            (replace-mesa->nvsa-git sdl2))
                   ;; Use newer version of gcc for gamescope
                   (replace "gcc:lib" gcc-12)
                   ;; Add libglvnd to mesa to fix Factorio segfaulting on
                   ;; startup.  Do it here instead of in nvsa-git definition to
                   ;; avoid issues with GL library not being found in system
                   ;; environment; seems like it only works here because there's
                   ;; only one place to search (i.e. /lib64) Relevant
                   ;; discussions:
                   ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/11666
                   ;; https://issues.guix.gnu.org/49339
                   (replace "mesa"
                     (package/inherit nvsa-git
                       (inputs (modify-inputs (package-inputs nvsa-git)
                                 (prepend libglvnd))))))
                 #:name "fhs-union-64"))
     ;; Requires i686-linux rust; package upstream in Guix does not build, so a
     ;; binary version is required if we want 32-bit NVK for the time being.
     (union32
      (fhs-union (modify-inputs `(,@steam-client-libs
                                  ,@steam-gameruntime-libs
                                  ,@fhs-min-libs)
                   (prepend sdl2)
                   (replace "mesa" nvsa-git))
                 #:name "fhs-union-32"
                 #:system "i686-linux")))))

(define-public steam-custom
  (nonguix-container->package steam-container-custom))
