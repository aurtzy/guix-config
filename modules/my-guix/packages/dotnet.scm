;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Jelle Licht <jlicht@fsfe.org>

(define-module (my-guix packages dotnet)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages tls)
  #:use-module (nongnu packages dotnet))

(define-public omnisharp/newer
  ;; This uses the "-net6.0" binary version of OmniSharp (as opposed to what
  ;; Nonguix currently packages), which is apparently needed for projects that
  ;; use more recent versions of dotnet.
  (package
    (name "omnisharp")
    (version "1.39.14")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri
        (string-append "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v"
                       version "/omnisharp-linux-x64-net6.0.tar.gz"))
       (sha256
        (base32 "094whfcp8fgq8630ykjsjbbh0ch3347siwmyxlfvi9wwm7jkwf89"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan
      #~'(("OmniSharp" ("gcc" "zlib")))
      #:install-plan
      #~'(("./" "share/omnisharp"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'symlink-bin
            (lambda _
              (let* ((target-bin (string-append #$output "/bin"))
                     (source (string-append #$output "/share/omnisharp/OmniSharp"))
                     (target (string-append target-bin "/omnisharp")))
                (mkdir-p target-bin)
                (symlink source target)))))))
    (inputs (list `(,gcc "lib") mit-krb5 zlib))
    (home-page "https://github.com/OmniSharp/omnisharp-roslyn")
    (supported-systems '("x86_64-linux"))
    (synopsis "Implementation of Language Server Protocol based on Roslyn workspaces")
    (description "OmniSharp is a @code{.NET} development platform based on
Roslyn workspaces.  It provides project dependencies and C# language services to
various IDEs and plugins.")
    (license license:expat)))
