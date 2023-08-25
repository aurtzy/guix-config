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
  #:use-module (guix profiles))

(define-public xsb
  (let ((commit "8631adcab4de5503e3376d0ca48a51c4958f1f5a")
        (revision "0"))
    (package
      (name "xsb")
      (version (git-version "5.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.code.sf.net/p/xsb/code")
                      (commit commit)))
                (sha256
                 (base32
                  "0j6b5ycq1dv86cycg74p7r5yvsh73h4lf1cyn5il6v6kzpfmyjy1"))))
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
                     (string-append begin touch))))))
            (add-after 'patch-calls 'patch-consult-recompilation
              ;; This patches the needs_recompile condition so that
              ;; rebuilds do not happen when Guix resets timestamps to
              ;; Unix epoch, since the newerthan path_sysop considers
              ;; equal timestamps to be newer than each other
              (lambda _
                (substitute*
                    "syslib/consult.P"
                  (("path_sysop\\(newerthan,PFileName,Obj\\)" all)
                   (string-append
                    all
                    ", path_sysop(modtime, PFileName, PFileNameMod)"
                    ", path_sysop(modtime, Obj, ObjMod)"
                    ", PFileNameMod \\== ObjMod")))))
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
             unixodbc))
      (propagated-inputs
       (list coreutils
             grep
             python
             sed))
      (synopsis "Logic Programming and Deductive Database system")
      (description "")
      (home-page "https://xsb.sourceforge.net/")
      (license license:lgpl2.0))))
