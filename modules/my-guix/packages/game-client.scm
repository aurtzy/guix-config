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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
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
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix packages xorg)
  #:use-module (my-guix utils)
  #:use-module (nonguix multiarch-container)
  #:use-module (nongnu packages game-client)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public vulkan-headers/newer
  (package
    (inherit vulkan-headers)
    (name "vulkan-headers")
    (version "vulkan-sdk-1.3.280.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/Vulkan-Headers")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13mmv5621z73hlfnsrccbcb4z0d7kwj92a081701vbpss45a4whj"))))))

(define-public stb-image-resize
  ((@@ (gnu packages stb) make-stb-header-package)
   "stb-image-resize" "0.96"
   "stb-image-resize is a library that supports scaling and translation of
images."))

;; From: https://gitlab.com/nonguix/nonguix/-/merge_requests/200
;;
;; Doesn't work for NVK yet :c
;; (https://gitlab.freedesktop.org/mesa/mesa/-/issues/9480)
(define-public gamescope
  ;; Use older than 3.14.3 due to a bug:
  ;; https://github.com/ValveSoftware/gamescope/issues/1218
  (let ((version "3.14.2")
        (revision "0")
        (commit "d0d23c4c3010c81add1bd90cbe478ce4a386e28d"))
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
          (base32 "1sw2br3g16mird7jc7idbcxf5xxjmiyr6hjw3966s0nsv6bn8vb2"))
         (file-name (git-file-name name version))))
      (build-system meson-build-system)
      (arguments
       (list
        #:modules '((guix build meson-build-system)
                    (guix build utils)
                    (my-guix build utils))
        #:imported-modules `(,@%meson-build-system-modules
                             (guix build utils)
                             (my-guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-subprojects
              (lambda _
                ;; glm
                ;;
                ;; TODO Look for a better way to do this.  Kind of a hacky way
                ;; to handle zip files, and it doesn't seem like I can
                ;; auto-extract the zip without making it a package (also not
                ;; preferable).
                (invoke
                 #+(file-append unzip "/bin/unzip")
                 #+(origin
                     (method url-fetch)
                     (uri
                      "https://wrapdb.mesonbuild.com/v2/glm_0.9.9.8-2/get_patch")
                     (sha256
                      (base32
                       "0gfqg3j1kfhycg7bygdxxfhp1qarzxqlrk4j9sz893d2sgya2c6r")))
                 "-d"
                 "subprojects/packagefiles/")
                (copy-recursively "subprojects/packagefiles/glm-0.9.9.8"
                                  "subprojects/packagefiles/glm")
                ;; stb
                (patch-wrap-file
                 "stb"
                 #+(directory-union "stb" (list stb-image
                                                stb-image-write
                                                stb-image-resize)))
                (substitute* "subprojects/stb/meson.build"
                  (("include_directories\\('\\.'\\)")
                   (string-append "include_directories('./include')")))
                ;; libdisplay-info
                (substitute* "src/meson.build"
                  ;; Allow newer versions
                  (("(version: \\['>= 0\\.0\\.0'), '< 0\\.2\\.0'(\\])"
                    _ left-part right-part)
                   (string-append left-part right-part)))
                ;; hwdata:pnp
                (substitute* "meson.build"
                  (("warning\\('Building without hwdata pnp id support\\.'\\)")
                   (string-append
                    "add_project_arguments("
                    "'-DHWDATA_PNP_IDS=\"" #$hwdata:pnp "/share/hwdata\"',"
                    "language: 'cpp'"
                    ")"))))))))
      (native-inputs
       (list cmake
             pkg-config
             python-3))
      (inputs
       (list clang
             eudev
             gcc-toolchain-12
             glm
             glslang
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
             libxxf86vm
             ;; openvr ;does not build when included
             pipewire
             pixman
             sdl2
             vulkan-headers
             vulkan-loader
             wayland
             wayland-protocols
             xcb-util-wm
             xorg-server-xwayland))
      (home-page "https://github.com/ValveSoftware/gamescope")
      (synopsis "Session compositing window manager")
      (description "Gamescope is a Wayland compositor for running games,
formerly known as steamcompmgr.  It is designed for use in embedded sessions
and as a nested compositor on top of a regular desktop environment through
sandboxed Xwayland sessions.")
      (license license:gpl3+))))

(define (replace-mesa inputs)
  ;; Because this is a hacky hack, do a sanity check to make sure mesa is
  ;; actually matched, then remove it
  (let ((matched (and=> (member "mesa" inputs (lambda (x input)
                                                (equal? x (car input))))
                        car)))
    (unless matched
      (display "SANITY CHECK FAILED: MESA NOT FOUND\n")
      (raise-exception (make-exception)))
    (cons `("mesa" ,mesa-nvk-git) (delq matched inputs))))

(define-public steam-container-custom
  (let ((steam-client-libs (replace-mesa
                            (@@ (nongnu packages game-client)
                                steam-client-libs)))
        (steam-gameruntime-libs (@@ (nongnu packages game-client)
                                    steam-gameruntime-libs)))
    (nonguix-container
     (inherit steam-container)
     (union64
      (fhs-union `(,@(delete "gcc:lib"
                             steam-client-libs
                             (lambda (x elem)
                               (equal? x
                                       (car elem))))
                   ,@steam-gameruntime-libs
                   ,@fhs-min-libs
                   ;; Use newer version for gamescope (and remove older one
                   ;; above)
                   ("gcc:lib" ,gcc-12 "lib")
                   ("gamescope" ,gamescope))
                 #:name "fhs-union-64"))
     ;; Uncomment this to apply 32-bit version of mesa-git.  Requires
     ;; i686-linux rust, which is not available in Guix at the moment
     ;; (although rust-binary from (my-guix packages rust) may be used with
     ;; mesa-git to achieve this).
     ;;
     ;; (union32
     ;;  (fhs-union `(,@steam-client-libs
     ;;               ,@steam-gameruntime-libs
     ;;               ,@fhs-min-libs)
     ;;             #:name "fhs-union-32"
     ;;             #:system "i686-linux"))
     )))

(define steam-custom
  (let ((steam-pkg (nonguix-container->package steam-container-custom)))
    (package
      (inherit steam-pkg)
      (version (string-append (package-version steam-pkg) "-custom")))))

;;; TODO This probably belongs in mods.
(define-public steam-custom-wrapped
  (package
    (name "steam-custom")
    (version (package-version steam-custom))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let* ((out (assoc-ref %outputs "out"))
                   (out-steam (string-append out "/bin/steam"))
                   (steam (assoc-ref %build-inputs "steam")))
              (mkdir-p (string-append out "/bin"))
              (symlink (string-append steam "/bin/steam")
                       out-steam)
              (mkdir-p (string-append out "/share"))
              (symlink (string-append steam "/share/applications")
                       (string-append out "/share/applications"))
              (wrap-program out-steam
                #:sh #$(file-append bash-minimal "/bin/bash")
                '("QT_X11_NO_MITSHM" = ("1"))
                '("GUIX_SANDBOX_EXTRA_SHARES"
                  prefix ("$HOME/storage/steam-alt-library"
                          "$HOME/areas/games"
                          "$HOME/.config/r2modmanPlus-local"))))))))
    (inputs
     (list steam-custom))
    (home-page (package-home-page steam-custom))
    (synopsis (package-synopsis steam-custom))
    (description (package-description steam-custom))
    (license (package-license steam-custom))))
