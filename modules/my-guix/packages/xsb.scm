;;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages xsb)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (my-guix utils))

(define-public xsb
  (let ((commit "660f0f79b1007f2f0be5bfa4e079abaf12b4f1d9")
        (revision "1"))
    (package
      (name "xsb")
      (version (git-version "5.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.code.sf.net/p/xsb/code")
               (commit commit)))
         (sha256
          (base32
           "0svb6hg7bv0y8x3qs8yhnrk6zsma12z5vmqw95vfw9rlsb0a91ns"))
         (patches
          (search-my-patches
           "0001-Avoid-recompiling-when-modification-times-are-equal.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases
              %standard-phases
            (add-before 'configure 'chdir-patch-calls
              (lambda _
                (chdir "XSB")))
            (add-after 'chdir-patch-calls 'patch-calls
              (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
                (let* ((coreutils-pref (assoc-ref inputs "coreutils"))
                       (sh (string-append (assoc-ref inputs "bash")
                                          "/bin/sh"))
                       (rm (string-append coreutils-pref
                                          "/bin/rm"))
                       (touch (string-append coreutils-pref
                                             "/bin/touch")))
                  (with-fluids ((%default-port-encoding #f))
                    (substitute*
                        (find-files "."
                                    (lambda (file stat)
                                      (and ((file-name-predicate "^[^\\.]*$|\\.sh$") file stat)
                                           (eq? 'regular (stat:type stat))))
                                    #:stat lstat)
                      (("(^.*\\s?)(SHELL=)/bin/sh" all begin shell)
                       (string-append begin shell sh))
                      (("(^.*\\s?)/bin/rm" all begin)
                       (string-append begin rm))
                      (("(^.*\\s?)/bin/touch" all begin)
                       (string-append begin touch)))))))
            (add-after 'patch-calls 'trigger-consult-recompilation
              ;; Since the recompilation patch is not upstreamed and the
              ;; precompiled binary that needs to be recompiled is the module
              ;; that decides recompilation, we forcefully trigger
              ;; recompilation by setting its modtime.
              (lambda _
                (utime "syslib/consult.P" 2 2)))
            (add-after 'patch-calls 'patch-configure
              (lambda _
                (substitute*
                    "build/configure"
                  ;; Modify args to ignore some options that cause issues
                  (("^#!/gnu/store/.*/bin/sh" shebang)
                   (string-join
                    (list
                     shebang
                     "for arg; do"
                     ;; Not supported, so we disable fast-install
                     "  [ \"$arg\" != --enable-fast-install ] &&"
                     ;; Build type messes up bin path?
                     "    [ \"${arg::8}\" != --build= ] &&"

                     "    newargs+=(\"$arg\")"
                     "done"
                     "set -- \"${newargs[@]}\"")
                    "\n"
                    'suffix)))))
            (add-before 'configure 'chdir-configure
              (lambda _
                (chdir "build")))
            (add-before 'build 'patch-generated-configuration
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (find-files "../config")
                  (("/bin/sh")
                   (string-append (assoc-ref inputs "bash")
                                  "/bin/sh")))))
            (replace 'build
              (lambda _
                (invoke "./makexsb")))
            (delete 'check)
            (replace 'install
              (lambda _
                (invoke "./makexsb" "install")))
            (add-after 'install 'chdir-out
              (lambda* (#:key outputs #:allow-other-keys)
                (chdir (assoc-ref outputs "out"))))
            (add-after 'chdir-out 'install-bin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((out (getcwd)))
                  (mkdir-p "bin")
                  (call-with-output-file "bin/xsb"
                    (lambda (port)
                      (let ((sh
                             (string-append (assoc-ref inputs "bash")
                                            "/bin/sh"))
                            (put-string
                             (@ (ice-9 textual-ports) put-string)))
                        (put-string port
                                    (string-append
                                     "#!" sh "\n"
                                     out "/xsb-5.0.0/bin/xsb \"$@\"\n")))))
                  (chmod "bin/xsb" #o777)))))))
      (native-inputs
       (list makedepend))
      (inputs
       (list curl
             mysql
             pcre
             unixodbc
             coreutils
             grep
             python
             sed))
      (synopsis "Logic Programming and Deductive Database system")
      (description "")
      (home-page "https://xsb.sourceforge.net/")
      (license license:lgpl2.0))))
