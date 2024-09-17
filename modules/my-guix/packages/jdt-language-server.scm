
(define-module (my-guix packages jdt-language-server)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (my-guix utils))

(define-public jdt-language-server
  (let ((version-number "1.29.0")
        (version-suffix "-202310261436"))
    (package
      (name "jdt-language-server")
      (version (string-append version-number version-suffix))
      (source
       (origin
         (method url-fetch)
         (uri (format
               #f
               "https://download.eclipse.org/jdtls/milestones/~a/~a-~a.tar.gz"
               version-number name version))
         (sha256 (base32
                  "13a7xc360iic7ik5ffsajkzhfc7hyizywpzqidvf5zwk701rayc6"))))
      (build-system copy-build-system)
      (arguments (list #:install-plan #~'(("../" "/"))))
      (home-page "https://projects.eclipse.org/projects/eclipse.jdt.ls")
      (synopsis "LSP server for Java")
      (description "")
      (license license:epl2.0))))
