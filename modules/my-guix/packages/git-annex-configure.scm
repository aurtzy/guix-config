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
  (let ((commit "cadba76339ca3bb6f3844e3402dcea1e63fabe6e")
        (revision "1"))
    (package
      (name "git-annex-configure")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (uri
                 (git-reference
                  (url "https://github.com/aurtzy/git-annex-configure.git")
                  (commit commit)))
                (method git-fetch)
                (sha256
                 (base32
                  "1cjhzmdv9j6dabh3w5yjhd80x9anlnadyfl594jcgmmzj7p4pfgz"))
                (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "src"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'install-scripts
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir (string-append out "/bin"))
                  (symlink #$(program-file
                              "git-annex-configure"
                              '(begin
                                 (use-modules (git-annex-configure main))
                                 (main (command-line))))
                           (string-append out
                                          "/bin/git-annex-configure"))))))))
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
       "git-annex-configure is a git-annex addon command for reducing the maintenance burden of applying repository settings.  More specifically, git-annex-configure lets you specify settings for your repositories in Guile and can apply most of your configurations with a single execution of the command.")
      (home-page "https://github.com/aurtzy/git-annex-configure")
      (license license:gpl3+))))
