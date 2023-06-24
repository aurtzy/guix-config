;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

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
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public python-uinput
  (package
    (name "python-uinput")
    (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-uinput" version))
              (sha256 (base32 "033zqiypjz0nigav6vz0s57pbzikvds55mxphrdpkdbpdikjnfcr"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (delete 'sanity-check)
         (delete 'validate-runpath))))
    (propagated-inputs
     (list eudev))
    (home-page "http://tjjr.fi/sw/python-uinput/")
    (synopsis "Pythonic API to Linux uinput kernel module.")
    (description "Pythonic API to Linux uinput kernel module.")
    (license gpl3+)))

(define-public keyboard-center
  (package
    (name "keyboard-center")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zocker-160/keyboard-center/")
                    (commit "1.0.0")))
              (file-name (git-file-name name version))
              (sha256 (base32 "0hf3vaif7xwylda6jbfrg8wppjfwz6hnsimcx9kdxr631v20cv8v"))))
    (build-system copy-build-system)
    (arguments
     `(#:modules ((guix build copy-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%copy-build-system-modules
                           (guix build qt-utils))
       #:install-plan '(("src/"
                         "/lib/keyboard-center/"
                         #:include-regexp ("assets/"
                                           "config/"
                                           "devices/"
                                           "gui/"
                                           "lib/"
                                           "main.py"
                                           "mainUi.py"
                                           "service.py"
                                           "constants.py"))
                        ("linux_packaging/assets/keyboard-center.sh"
                         "/bin/keyboard-center")
                        ("linux_packaging/60-keyboard-center.rules"
                         "/lib/udev/rules.d/")
                        ("linux_packaging/uinput-keyboard-center.conf"
                         "/lib/modules-load.d/keyboard-center.conf")
                        ("linux_packaging/assets/keyboard-center.png"
                         "/share/icons/hicolor/512x512/apps/")
                        ("linux_packaging/assets/keyboard-center.desktop"
                         "/share/applications/"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-script
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "linux_packaging/assets/keyboard-center.sh"
                 (("/usr/lib") (string-append out "/lib")))
               (chmod "linux_packaging/assets/keyboard-center.sh" #o777))))
         (add-after 'install 'wrap-prog
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((input-libpath (lambda (path)
                                     (string-append
                                      (assoc-ref inputs path)
                                      "/lib")))
                    (out (assoc-ref outputs "out"))
                    (eudev (input-libpath "eudev"))
                    (hidapi (input-libpath "hidapi")))
               (wrap-program (string-append out "/bin/keyboard-center")
                 `("LD_LIBRARY_PATH" ":" = ,(list eudev
                                                  hidapi)))
               (wrap-qt-program "keyboard-center"
                                #:output %output
                                #:inputs inputs)))))))
    (native-inputs
     (list qtwayland-5))
    (inputs
     (list bash-minimal
           eudev
           hidapi))
    (propagated-inputs
     (list python
           python-pyqt
           breeze
           python-uinput
           python-ruamel.yaml
           python-pyusb
           python-inotify-simple
           libnotify))
    (synopsis "Application for mapping macro keys on Logitech keyboards")
    (description "")
    (home-page "https://github.com/zocker-160/keyboard-center")
    (license gpl3)))
