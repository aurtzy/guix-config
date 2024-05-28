;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages syncplay)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (my-guix utils))

(define-public syncplay
  (package
    (name "syncplay")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Syncplay/syncplay.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xw9rl4bpmcskxs9f8zlvhhi1xlqfzzrfgmb0r6n50y2fbg7ycmc"))))
    (build-system python-build-system)
    (arguments
     (list #:imported-modules `(,@%python-build-system-modules
                                (guix build qt-utils)
                                (guix build utils))
           #:modules '((guix build python-build-system)
                       (guix build qt-utils)
                       (guix build utils))
           #:phases #~(modify-phases %standard-phases
                        (delete 'check)
                        (replace 'install
                          (lambda _
                            (invoke "make" "install" "DESTDIR="
                                    (string-append "PREFIX="
                                                   (assoc-ref %outputs "out")))))
                        (add-after 'install 'wrap-qt
                          (lambda* (#:key inputs #:allow-other-keys)
                            (wrap-qt-program "syncplay"
                                             #:output #$output
                                             #:inputs inputs
                                             #:qt-major-version "6"))))))
    (native-inputs (list python-pyside-6))
    (inputs (list bash-minimal
                  python-certifi
                  python-idna
                  python-service-identity
                  python-twisted
                  qtwayland))
    (home-page "https://syncplay.pl")
    (synopsis "Client/server to synchronize media playback on many computers")
    (description
     "Syncplay is a solution to synchronize video playback across multiple
instances of media players over the Internet.  When one person pauses/unpauses
playback or skips to a position in the video, this is replicated across all
media players connected to the same server and in the same \"room\" (viewing
session).  A built-in text chat for discussing the synced media is also
included for convenience.")
    (license license:asl2.0)))
