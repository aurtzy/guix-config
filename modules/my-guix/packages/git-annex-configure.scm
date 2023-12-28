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

(define-module (my-guix packages git-annex-configure)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control))

(define-public git-annex-configure
  (let ((commit "8699ba6e6b27d1bfd3302d371d7207786d33d096")
        (revision "0"))
    (package
      (name "git-annex-configure")
      (version (git-version "2.0.0" revision commit))
      (source (origin
                (uri
                 (git-reference
                  (url "https://github.com/aurtzy/git-annex-configure.git")
                  (commit commit)))
                (method git-fetch)
                (sha256
                 (base32
                  "1wpv614adyxjnryssram4gbv2g6dhzkbvklvj1m8lr2p0bvbka16"))
                (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "modules"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'add-build-configurations
              (lambda* (#:key outputs #:allow-other-keys)
                (symlink #$(scheme-file "version.scm" version)
                         "modules/git-annex-configure/version.scm")))
            (add-after 'install 'install-scripts
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir (string-append out "/bin"))
                  (symlink #$(program-file
                              "git-annex-configure"
                              '(begin
                                 (use-modules (git-annex-configure main))
                                 (main (command-line))))
                           (string-append out "/bin/git-annex-configure"))))))))
      (native-inputs
       ;; Specify guile-3.0 here for guile-build-system and also in
       ;; propagated-inputs for runtime features (git hook scripts)
       (list guile-3.0))
      (propagated-inputs
       (list guile-3.0
             git
             git-annex))
      (synopsis "Declarative git-annex configuration with Guile Scheme")
      (description
       "git-annex-configure is a git-annex addon command that enables
declarative configuration of git-annex repositories using Guile Scheme.  With
a few limitations, settings for repositories can be applied with just a single
execution of the command.")
      (home-page "https://github.com/aurtzy/git-annex-configure")
      (license license:gpl3+))))
