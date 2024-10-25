;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (my-guix utils))

(define-public emacs-disproject
  (package
    (name "emacs-disproject")
    (version "0.1.0")
    (source
     (let ((local-disproject (path-append-my-home "/src/disproject")))
       (if (file-exists? local-disproject)
           (local-file local-disproject
                       #:recursive? #t
                       #:select?
                       (lambda (file stat)
                         (not (string-contains file "/.git/"))))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/aurtzy/disproject")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name "git"))
             (sha256
              (base32 "1whidgdahn3vnamhaqcb75ldrlib97k6qlvbbyd03j83hrlv7zv0"))))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient))
    (home-page "https://github.com/aurtzy/disproject")
    (synopsis "Transient interface for managing and interacting with projects")
    (description
     "Disproject is a package for Emacs that provides integration with
project.el via extendable Transient menus.")
    (license license:gpl3+)))

(define-public emacs-nftables-mode
  (package
    (name "emacs-nftables-mode")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/nftables-mode-"
                           version ".tar"))
       (sha256
        (base32 "1wjw6n60kj84j8gj62mr6s97xd0aqvr4v7npyxwmhckw9z13xcqv"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/nftables-mode.html")
    (synopsis "Major mode for editing nftables")
    (description
     "This package provides a major mode for editing nftables files.  It
supports basic highlighting and indentation.")
    (license license:gpl3+)))
