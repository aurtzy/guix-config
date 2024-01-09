(define-module (my-guix packages linux)
  #:use-module (gnu packages linux)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define-public linux-6.7-rc
  (package
    (inherit
     (customize-linux
      #:name "linux"
      #:source (origin
                 (method url-fetch)
                 (uri
                  "https://git.kernel.org/torvalds/t/linux-6.7-rc8.tar.gz")
                 (sha256
                  (base32
                   "02k4l007is8yk5bwznf3k0rl35cspbxf98yrhrxdcbrvnfqwn1j3")))))
    (version "6.7-rc8")))
