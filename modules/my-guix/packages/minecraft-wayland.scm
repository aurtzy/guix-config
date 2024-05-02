;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Samuel Culpepper <sculpepper@newstore.com>
;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; See: https://github.com/Admicos/minecraft-wayland/issues/18

(define-module (my-guix packages minecraft-wayland)
  #:use-module (gnu packages kde-frameworks) ; extra-cmake-modules
  #:use-module (gnu packages xdisorg) ; xkbcommon
  #:use-module (gnu packages gl)
  ;; ^ for > 3.3.4 + glfw-wayland
  #:use-module (gnu packages glib)  ; for dbus
  #:use-module (gnu packages cmake) ; for cmake
  #:use-module (gnu packages gtk)   ; for cairo
  ;; ^ for libdecor
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system waf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (my-guix utils))

(define-public glfw-wayland-minecraft
  (package
    (name "glfw-wayland-minecraft")
    (version "3.4.0")
    (source
     (origin (method url-fetch)
             (uri (string-append
                   "https://github.com/glfw/glfw/archive/"
                   "62e175ef9fae75335575964c845a302447c012c7" ;; commit
                   ".tar.gz"))
             (sha256
              (base32
               "1zlbc4jyxgpll8vnaq382fa92r98y84prcdk15bqi6fygb2rd3wq"))
             (patches
              (search-my-patches
               ;; https://github.com/Admicos/minecraft-wayland/tree/bdc3c0d192097459eb4e72b26c8267f82266e951
               "0003-Don-t-crash-on-calls-to-focus-or-icon.patch"
               "0004-wayland-fix-broken-opengl-screenshots-on-mutter.patch"
               "0005-Add-warning-about-being-an-unofficial-patch.patch"
               ;; "0006-Don-t-crash-getting-scancode-name.patch" ;; BROKEN
               "0007-Platform-Prefer-Wayland-over-X11.patch"
                                 
               ;; https://github.com/Admicos/minecraft-wayland/pull/29
               "0008-libdecor-proper-decorations-with-title-and-window-bu.patch"
               "0009-Add-libdecoration-marker-to-stderr-warning.patch"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f ;; no test target
                 #:configure-flags
                 '("-DBUILD_SHARED_LIBS=ON"
                   "-DGLFW_USE_WAYLAND=ON"
                   "-DGLFW_USE_LIBDECOR=ON" ;; libdecoration
                   )))
    (native-inputs
     (list pkg-config
           doxygen
           unzip
           mesa
           extra-cmake-modules
           wayland-protocols
           libxrandr
           libxi
           libxinerama
           libxcursor
           libdecor))
    (inputs
     (list wayland
           libxkbcommon))
    (propagated-inputs
     (list
      ;; These are in 'Requires.private' of 'glfw3.pc'.
      ;;openjdk
      libx11
      libxxf86vm))
    (home-page "https://www.glfw.org")
    (synopsis "OpenGL application development library")
    (description
     "GLFW is a library for OpenGL, OpenGL ES and Vulkan development for
desktop computers.  It provides a simple API for creating windows, contexts
and surfaces, receiving input and events.")
    (license license:zlib)))
