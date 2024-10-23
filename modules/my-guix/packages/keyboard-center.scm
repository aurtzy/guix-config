;;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages keyboard-center)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (my-guix utils))

(define-public python-lupa
  (package
    (name "python-lupa")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lupa" version))
       (sha256
        (base32 "0kcjr2s7l59gprcjnrhaj5ahqsnh56wj8f4mvggsr6ldrxmh0nk6"))))
    (build-system pyproject-build-system)
    ;; There are tests that attempt to use internet.
    (arguments (list #:tests? #f))
    (home-page "https://github.com/scoder/lupa")
    (synopsis "Python wrapper around Lua and LuaJIT")
    (description "Python wrapper around Lua and @code{LuaJIT}.")
    (license license:expat)))

(define-public python-uinput
  (package
    (name "python-uinput")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-uinput" version))
       (sha256 (base32
                "1fkfic1mk681spsi0281m58mslghpipvlcmfshvmbpv49cs9fdl5"))
       (patches
        (search-my-patches
         "0001-libsuinput-Add-module-initialization-function.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-libudev
            (lambda _
              (substitute* "setup.py"
                (("libraries=\\[.*libudev_so\\]" all)
                 (string-append
                  "library_dirs=['" #$eudev "/lib'], "
                  "extra_link_args=['-ludev']"))))))))
    (home-page "https://github.com/pyinput/python-uinput")
    (synopsis "Pythonic API to Linux uinput kernel module.")
    (description "Pythonic API to Linux uinput kernel module.")
    (license license:gpl3+)))

;; TODO: Latest version requires Python 3.12+; look into this:
;; https://git.ditigal.xyz/~ruther/guix-exprs/tree/main/item/ruther/packages/python-next.scm#L51
;;
;; FIXME: icons do not show up.  Potentially related:
;; https://www.github.com/zocker-160/keyboard-center/issues/55
(define-public keyboard-center
  (let ((version "1.0.10")
        (hash "1hc7vv7pzadqn27f4hg93ydrw2k94x3p004f3m58w4vxf5xnzj5n"))
    (package
      (name "keyboard-center")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zocker-160/keyboard-center")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))))
      (build-system copy-build-system)
      (arguments
       (list
        #:modules '((guix build copy-build-system)
                    (guix build qt-utils)
                    (guix build utils)
                    (ice-9 match))
        #:imported-modules `(,@%copy-build-system-modules
                             (guix build qt-utils))
        #:install-plan #~'(("src"
                            "/lib/keyboard-center")
                           ("linux_packaging/60-keyboard-center.rules"
                            "/lib/udev/rules.d/")
                           ("linux_packaging/uinput-keyboard-center.conf"
                            "/lib/modules-load.d/keyboard-center.conf")
                           ("linux_packaging/assets/keyboard-center.png"
                            "/share/icons/hicolor/512x512/apps/")
                           ("linux_packaging/assets/keyboard-center.desktop"
                            "/share/applications/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-libraries
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "src/lib/hid.py"
                    ;; "keyboard_center/lib/hid.py"
                  (("library_paths = " assign)
                   ;; Push the originally assigned tuple onto the next line to
                   ;; make it a noop that can be ignored
                   (string-append assign
                                  "('"
                                  (search-input-file
                                   inputs "lib/libhidapi-hidraw.so")
                                  "',)\n")))))
            (add-after 'install 'install-bin
              (lambda _
                (mkdir-p (string-append #$output "/bin"))
                (with-output-to-file (string-append
                                      #$output "/bin/keyboard-center")
                  (lambda _
                    (display
                     (string-append
                      "#!" (which "sh") "\n"
                      (which "python3")
                      " " #$output "/lib/keyboard-center/main.py $@"))))
                (chmod (string-append #$output "/bin/keyboard-center") #o777)))
            (add-after 'install-bin 'wrap-program
              (lambda* (#:key inputs #:allow-other-keys)
                (wrap-program (string-append #$output "/bin/keyboard-center")
                  '("LD_LIBRARY_PATH" ":" = (#$(file-append eudev "/lib"))))
                (wrap-qt-program "keyboard-center"
                                 #:output %output
                                 #:inputs inputs))))))
      (inputs
       (list bash-minimal
             hidapi
             libnotify
             qtwayland-5))
      (propagated-inputs
       (list python
             python-pyqt
             python-pyusb
             python-ruamel.yaml
             python-uinput))
      (synopsis "Application for mapping macro keys on Logitech keyboards")
      (description "")
      (home-page "https://github.com/zocker-160/keyboard-center")
      (license license:gpl3))))
