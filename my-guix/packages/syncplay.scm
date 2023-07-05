;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages syncplay)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python-crypto))

(define-public syncplay
  (package
    (name "syncplay")
    (version "1.6.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Syncplay/syncplay/")
                    (commit "f7faa592466d283bc570b9a7eff14f9b7476c3d2")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qm3qn4a1nahhs7q81liz514n9blsi107g9s9xfw2i8pzi7v9v0v"))))
    (build-system gnu-build-system)
    (arguments
     (list #:imported-modules `(,@%gnu-build-system-modules
                                (guix build qt-utils)
                                (guix build utils))
           #:modules '((guix build gnu-build-system)
                       (guix build qt-utils)
                       (guix build utils))
           #:make-flags #~`("DESTDIR="
                            ,(string-append "PREFIX="
                                            (assoc-ref %outputs "out")))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (delete 'build)
                        (delete 'check)
                        (add-after 'install 'wrap-qt
                          (lambda* (#:key inputs #:allow-other-keys)
                            (wrap-qt-program "syncplay"
                                             #:output #$output
                                             #:inputs inputs))))))
    (native-inputs
     (list qtwayland-5))
    (inputs
     (list bash-minimal))
    (propagated-inputs
     (list python
           python-service-identity
           python-twisted
           python-pyside-2
           python-certifi
           python-idna))
    (home-page "https://syncplay.pl")
    (synopsis "Client/server to synchronize media playback on many computers")
    (description
     "Syncplay is a solution to synchronize video playback across multiple
instances of media players over the Internet.  When one person
pauses/unpauses playback or skips to a position in the video, this is
replicated across all media players connected to the same server and
in the same \"room\" (viewing session).  A built-in text chat for
discussing the synced media is also included for convenience.")
    (license asl2.0)))
