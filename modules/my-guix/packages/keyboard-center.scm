;;; Copyright © 2023-2025 aurtzy <aurtzy@gmail.com>
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
  #:use-module (gnu packages lua)
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
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix packages rust)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

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
    (native-inputs (list python-cython python-setuptools python-wheel))
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
                (("libraries=\\[.*libudev_so\\]" libs)
                 (string-append libs ", extra_link_args=['-ludev']"))))))))
    (native-inputs (list python-setuptools python-wheel))
    (inputs (list eudev))
    (home-page "https://github.com/pyinput/python-uinput")
    (synopsis "Pythonic API to Linux uinput kernel module.")
    (description "Pythonic API to Linux uinput kernel module.")
    (license license:gpl3+)))

;; FIXME: icons do not show up.  Potentially related:
;; https://www.github.com/zocker-160/keyboard-center/issues/55
(define-public keyboard-center
  (let ((version "2.0.6")
        (hash "0zv1zx19pq48ibmbxzqfqal7jrgyg5sva2nnfpym9iwl8m23g57j"))
    (package
      (name "keyboard-center")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/zocker-160/keyboard-center")
                (commit version)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))
         (patches
          (search-my-patches
           "0001-Fix-crash-when-running-with-Python-3.11.patch"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        ;; Tests are available, but not hooked up to the build system.
        #:tests? #f
        #:modules '((guix build pyproject-build-system)
                    (guix build qt-utils)
                    (guix build utils)
                    (ice-9 match))
        #:imported-modules `(,@%pyproject-build-system-modules
                             (guix build qt-utils))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-libraries
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "keyboard_center/lib/hid.py"
                  (("library_paths = " assign)
                   ;; Push the originally assigned tuple onto the next line to
                   ;; make it a noop that can be ignored
                   (string-append assign
                                  "('"
                                  (search-input-file
                                   inputs "lib/libhidapi-hidraw.so")
                                  "',)\n")))))
            (add-after 'install 'install-files
              (lambda _
                (install-file "linux_packaging/60-keyboard-center.rules"
                              (string-append #$output "/lib/udev/rules.d"))
                ;; TODO: This was a workaround for an issue with
                ;; EndeavourOS (and Arch-based?).  Is this still
                ;; needed?
                ;; (install-file "linux_packaging/uinput-keyboard-center.conf"
                ;;               (string-append #$output "/lib/modules-load.d"))
                ))
            (add-after 'install 'install-desktop-files
              (lambda _
                (install-file "linux_packaging/assets/keyboard-center.png"
                              (string-append
                               #$output "/share/icons/hicolor/512x512/apps"))
                (install-file "linux_packaging/assets/keyboard-center.desktop"
                              (string-append
                               #$output "/share/applications"))))
            (add-after 'create-entrypoints 'wrap-program
              (lambda* (#:key inputs #:allow-other-keys)
                (wrap-qt-program "keyboard-center"
                                 #:output #$output
                                 #:inputs inputs))))))
      (native-inputs (list python-setuptools python-wheel))
      (inputs (list bash-minimal hidapi libnotify qtwayland-5))
      (propagated-inputs (list lua
                               python-pyqt
                               python-pyusb
                               python-uinput
                               python-lupa))
      (synopsis "Application for mapping macro keys on Logitech keyboards")
      (description "")
      (home-page "https://github.com/zocker-160/keyboard-center")
      (license license:gpl3))))
