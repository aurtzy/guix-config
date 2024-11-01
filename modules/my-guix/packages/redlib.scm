;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages redlib)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (my-guix utils))

(define-public rust-byte-unit-4
  (package
    (name "rust-byte-unit")
    (version "4.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "byte-unit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v4v4z8qilnbg0lv16icbbbdq5kjpbp8yw044lszrzdqawhb6y6s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-utf8-width" ,rust-utf8-width-0.1))))
    (home-page "https://magiclen.org/byte-unit")
    (synopsis "library for interacting with units of bytes.")
    (description
     "This package provides a library for interacting with units of bytes.")
    (license license:expat)))

(define-public rust-cmd-lib-macros-1
  (package
    (name "rust-cmd-lib-macros")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cmd_lib_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g3a0qnsqacy2z1h54769lzvb44v1j8xvpxmf1xfjbyibv0gm04s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rust-shell-script/rust_cmd_lib")
    (synopsis
     "Common rust commandline macros and utils, to write shell script like tasks easily")
    (description
     "This package provides Common rust commandline macros and utils, to write shell script like tasks
easily.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cmd-lib-1
  (package
    (name "rust-cmd-lib")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cmd_lib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s7zavyvwzwhckkn7gwkzfy8qsaqmr803np9ixj4s7pr19hpg3vi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cmd-lib-macros" ,rust-cmd-lib-macros-1)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-faccess" ,rust-faccess-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-os-pipe" ,rust-os-pipe-1))
       #:cargo-development-inputs
       (("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-structopt" ,rust-structopt-0.3))))
    (home-page "https://github.com/rust-shell-script/rust_cmd_lib")
    (synopsis
     "Common rust commandline macros and utils, to write shell script like tasks easily")
    (description
     "This package provides Common rust commandline macros and utils, to write shell script like tasks
easily.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sealed-test-derive-1
  (package
    (name "rust-sealed-test-derive")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sealed_test_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dlx0042krhcly4rd1aq7qny8vmrphl610i5f381h525sjr3y9bp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/oknozor/sealed-test")
    (synopsis
     "procmacro attribute to run your test in an isolated environment")
    (description
     "This package provides a procmacro attribute to run your test in an isolated
environment.")
    (license license:expat)))

(define-public rust-rusty-forkfork-0.4
  (package
    (name "rust-rusty-forkfork")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rusty-forkfork" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0662hjcvaxw7kcawjmvkhs9sc7n74kjba6hj8c0hryx2vzs5ms3w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fnv" ,rust-fnv-1)
        ("rust-quick-error" ,rust-quick-error-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2))))
    (home-page "https://github.com/oknozor/rusty-forkfork")
    (synopsis
     "Cross-platform library for running Rust tests in sub-processes using a
fork-like interface.")
    (description
     "This package provides Cross-platform library for running Rust tests in sub-processes using a fork-like
interface.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sealed-test-1
  (package
    (name "rust-sealed-test")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sealed_test" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08m2g51g1jsrzsbpb8icda81g132vm2jwzin7jvpzg85y3w6f61a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fs-extra" ,rust-fs-extra-1)
        ("rust-rusty-forkfork" ,rust-rusty-forkfork-0.4)
        ("rust-sealed-test-derive" ,rust-sealed-test-derive-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:cargo-development-inputs
       (("rust-cmd-lib" ,rust-cmd-lib-1))))
    (home-page "https://github.com/oknozor/sealed-test")
    (synopsis
     "procmacro attribute to run your test in an isolated environment")
    (description
     "This package provides a procmacro attribute to run your test in an isolated
environment.")
    (license license:expat)))

(define-public rust-lipsum-0.9
  (package
    (name "rust-lipsum")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lipsum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r40mf2cwh4fp9pdfcc1n8hjxw05w7galjvb1z23r5pq38jn0s33"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.8)
        ("rust-version-sync" ,rust-version-sync-0.9))))
    (home-page "https://github.com/mgeisler/lipsum/")
    (synopsis
     "Lipsum is a lorem ipsum text generation library. It generates
pseudo-random Latin text. Use this if you need filler or dummy text
for your application.

The text is generated using a simple Markov chain, which you can
instantiate to generate your own pieces of pseudo-random text.")
    (description
     "This package provides Lipsum is a lorem ipsum text generation library.  It generates pseudo-random
Latin text.  Use this if you need filler or dummy text for your application.
The text is generated using a simple Markov chain, which you can instantiate to
generate your own pieces of pseudo-random text.")
    (license license:expat)))

(define-public rust-route-recognizer-0.3
  (package
    (name "rust-route-recognizer")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "route-recognizer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ikp3blbina00jdbifxw1c9whg6mljli24lq5pv82iar53xr9axg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustasync/route-recognizer")
    (synopsis
     "Recognizes URL patterns with support for dynamic and wildcard segments")
    (description
     "This package provides Recognizes URL patterns with support for dynamic and wildcard segments.")
    (license license:expat)))

(define-public rust-hyper-rustls-0.25
  (package
    (name "rust-hyper-rustls")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w5h46xzgy8i88p03vmcsmmkr638faq7910ccdzcp0w46gwpi71r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
        ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
        ("rust-webpki-roots" ,rust-webpki-roots-0.26))
       #:cargo-development-inputs
       (("rust-hyper" ,rust-hyper-0.14)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/rustls/hyper-rustls")
    (synopsis "Rustls+hyper integration for pure rust HTTPS")
    (description
     "This package provides Rustls+hyper integration for pure rust HTTPS.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-hyper-0.14
  (package
    (name "rust-hyper")
    (version "0.14.30")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jayxag79yln1nzyzx652kcy1bikgwssn6c4zrrp5v7s3pbdslm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-h2" ,rust-h2-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.4)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-httpdate" ,rust-httpdate-1)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-socket2" ,rust-socket2-0.4)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower-service" ,rust-tower-service-0.3)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-want" ,rust-want-0.3))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-matches" ,rust-matches-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pnet-datalink" ,rust-pnet-datalink-0.27)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-test" ,rust-tokio-test-0.4)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-tower" ,rust-tower-0.4)
        ("rust-url" ,rust-url-2))))
    (home-page "https://hyper.rs")
    (synopsis "fast and correct HTTP library.")
    (description "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-googletest-macro-0.11
  (package
    (name "rust-googletest-macro")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "googletest_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1addhzi4h6wjxps518ykmnik9l2hg0dcp30qjc0lha76s6vmfc7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/google/googletest-rust")
    (synopsis "Procedural macros for GoogleTest Rust")
    (description
     "This package provides Procedural macros for @code{GoogleTest} Rust.")
    (license license:asl2.0)))

(define-public rust-googletest-0.11
  (package
    (name "rust-googletest")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "googletest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wrngx8y30b659cahqr9hga71kwyxa9dxmbyyw728qg6w8rs4i7f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-googletest-macro" ,rust-googletest-macro-0.11)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustversion" ,rust-rustversion-1))
       #:cargo-development-inputs
       (("rust-indoc" ,rust-indoc-2)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-serial-test" ,rust-serial-test-2))))
    (home-page "https://github.com/google/googletest-rust")
    (synopsis
     "rich assertion and matcher library inspired by GoogleTest for C++")
    (description
     "This package provides a rich assertion and matcher library inspired by
@code{GoogleTest} for C++.")
    (license license:asl2.0)))

(define-public rust-copy-dir-0.1
  (package
    (name "rust-copy-dir")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "copy_dir" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18rckga6rf3nzlw8avyi6chdlid0kp7lhfjyy0pnw27g738isgal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/mdunsmuir/copy_dir")
    (synopsis
     "Copy directories recursively in a straightforward and predictable way")
    (description
     "This package provides Copy directories recursively in a straightforward and predictable way.")
    (license license:expat)))

(define-public rust-extreme-666
  (package
    (name "rust-extreme")
    (version "666.666.666666")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "extreme" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yrig1ky9a6xpcl5nqnkk9wyis7k7wjzb8bbr4n810apfhq5nq0n"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/spacejam/extreme")
    (synopsis
     "Extremely boring async function runner. MIT/Apache-2.0 license is available for spacejam's github sponsors")
    (description
     "This package provides Extremely boring async function runner.  MIT/Apache-2.0 license is available for
spacejam's github sponsors.")
    (license license:gpl3)))

(define-public rust-rio-0.9
  (package
    (name "rust-rio")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nv8wrnkd41flb32lmxb412l6m1790j12c3lg305764hcmbc564y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-extreme" ,rust-extreme-666))))
    (home-page "https://github.com/spacejam/rio")
    (synopsis
     "GPL-3.0 nice bindings for io_uring. MIT/Apache-2.0 license is available for spacejam's github sponsors")
    (description
     "This package provides GPL-3.0 nice bindings for io_uring.  MIT/Apache-2.0 license is available for
spacejam's github sponsors.")
    (license license:gpl3)))

(define-public rust-sled-0.34
  (package
    (name "rust-sled")
    (version "0.34.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sled" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dcr2s7cylj5mb33ci3kpx7fz797jwvysnl5airrir9cgirv95kz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-color-backtrace" ,rust-color-backtrace-0.5)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-fxhash" ,rust-fxhash-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-rio" ,rust-rio-0.9)
        ("rust-zstd" ,rust-zstd-0.9))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-chacha" ,rust-rand-chacha-0.2)
        ("rust-rand-distr" ,rust-rand-distr-0.3)
        ("rust-zerocopy" ,rust-zerocopy-0.3))))
    (home-page "https://github.com/spacejam/sled")
    (synopsis
     "Lightweight high-performance pure-rust transactional embedded database")
    (description
     "This package provides Lightweight high-performance pure-rust transactional embedded database.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mm6n3ijfsnk7grbbws3fc9qy4y5n3pshixa19wmhzimfqj47h1i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-fastrand" ,rust-fastrand-1)
        ("rust-redox-syscall" ,rust-redox-syscall-0.3)
        ("rust-rustix" ,rust-rustix-0.37)
        ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-time-3
  (package
    (name "rust-futures-time")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12km02vdlw9a4y7kg84v4p9fr12fvi3x2qnnyzjiz214d0x8a134"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-channel" ,rust-async-channel-1)
        ("rust-async-io" ,rust-async-io-1)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2))
       #:cargo-development-inputs
       (("rust-futures-lite" ,rust-futures-lite-1))))
    (home-page "https://github.com/yoshuawuyts/futures-time")
    (synopsis "async time combinators")
    (description "This package provides async time combinators.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-retry-0.3
  (package
    (name "rust-tokio-retry")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-retry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kr1hnm5dmb9gfkby88yg2xj8g6x4i4gipva0c8ca3xyxhvfnmvz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-pin-project" ,rust-pin-project-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/srijs/rust-tokio-retry")
    (synopsis "Extensible, asynchronous retry behaviours for futures/tokio")
    (description
     "This package provides Extensible, asynchronous retry behaviours for futures/tokio.")
    (license license:expat)))

(define-public rust-futures-rustls-0.25
  (package
    (name "rust-futures-rustls")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0km0dhsc6ji1ix9d52h2w5spinlj9smpxr5k5r4w1v0gkx4s5n68"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-pki-types" ,rust-rustls-pki-types-1))
       #:cargo-development-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
        ("rust-smol" ,rust-smol-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/quininer/futures-rustls")
    (synopsis "Asynchronous TLS/SSL streams for futures using Rustls")
    (description
     "This package provides Asynchronous TLS/SSL streams for futures using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crc16-0.4
  (package
    (name "rust-crc16")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crc16" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zzwb5iv51wnh96532cxkk4aa8ys47rhzrjy98wqcys25ks8k01k"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/blackbeam/rust-crc16")
    (synopsis "CRC16 implementation")
    (description "This package provides a CRC16 implementation.")
    (license license:expat)))

(define-public rust-async-native-tls-0.4
  (package
    (name "rust-async-native-tls")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhkka5azpr03wg2bswabmwcwcqbdia17h2d17hk4wk47kn4qzfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures")
    (description "This package provides Native TLS using futures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-redis-0.25
  (package
    (name "rust-redis")
    (version "0.25.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "redis" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v5vp150f62f3y6jhjy2z12g3hxk09nnrs597c7zc4bmbjasdmz0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ahash" ,rust-ahash-0.8)
        ("rust-arc-swap" ,rust-arc-swap-1)
        ("rust-async-native-tls" ,rust-async-native-tls-0.4)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-bigdecimal" ,rust-bigdecimal-0.4)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-combine" ,rust-combine-4)
        ("rust-crc16" ,rust-crc16-0.4)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-rustls" ,rust-futures-rustls-0.25)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-r2d2" ,rust-r2d2-0.8)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rust-decimal" ,rust-rust-decimal-1)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
        ("rust-ryu" ,rust-ryu-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha1-smol" ,rust-sha1-smol-1)
        ("rust-socket2" ,rust-socket2-0.5)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
        ("rust-tokio-retry" ,rust-tokio-retry-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
        ("rust-tokio-util" ,rust-tokio-util-0.7)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.26))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-assert-approx-eq" ,rust-assert-approx-eq-1)
        ("rust-criterion" ,rust-criterion-0.4)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-time" ,rust-futures-time-3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-partial-io" ,rust-partial-io-0.5)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-socket2" ,rust-socket2-0.5)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/redis-rs/redis-rs")
    (synopsis "Redis driver for Rust")
    (description "This package provides Redis driver for Rust.")
    (license license:bsd-3)))

(define-public rust-cached-proc-macro-types-0.1
  (package
    (name "rust-cached-proc-macro-types")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cached_proc_macro_types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h3gw61v1inay4g3b8pirxlz18m81k63dw2q18zj9fnmidmkds5d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/jaemk/cached")
    (synopsis
     "Generic cache implementations and simplified function memoization")
    (description
     "This package provides Generic cache implementations and simplified function memoization.")
    (license license:expat)))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.86")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs
       (("rust-flate2" ,rust-flate2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-tar" ,rust-tar-0.4))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.77")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vbkwfp9ymmi0fsyyjsqfvnv7gm8vjgl4pzprbk7p3pxc7gvqdcz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-automod" ,rust-automod-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-insta" ,rust-insta-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-ref-cast" ,rust-ref-cast-1)
        ("rust-reqwest" ,rust-reqwest-0.12)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "This package provides Parser for Rust source code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-darling-macro-0.20
  (package
    (name "rust-darling-macro")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling-core" ,rust-darling-core-0.20)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Internal support for a proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
     "This package provides Internal support for a proc-macro library for reading attributes into structs
when implementing custom derives.  Use https://crates.io/crates/darling in your
code.")
    (license license:expat)))

(define-public rust-strsim-0.11
  (package
    (name "rust-strsim")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strsim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis
     "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (description
     "This package provides Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (license license:expat)))

(define-public rust-darling-core-0.20
  (package
    (name "rust-darling-core")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fnv" ,rust-fnv-1)
        ("rust-ident-case" ,rust-ident-case-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-strsim" ,rust-strsim-0.11)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis
     "Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives. Use https://crates.io/crates/darling in your code.")
    (description
     "This package provides Helper crate for proc-macro library for reading attributes into structs when
implementing custom derives.  Use https://crates.io/crates/darling in your code.")
    (license license:expat)))

(define-public rust-darling-0.20
  (package
    (name "rust-darling")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling-core" ,rust-darling-core-0.20)
        ("rust-darling-macro" ,rust-darling-macro-0.20))
       #:cargo-development-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-syn" ,rust-syn-2)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/TedDriggs/darling")
    (synopsis "proc-macro library for reading attributes into structs when
implementing custom derives.")
    (description
     "This package provides a proc-macro library for reading attributes into structs
when implementing custom derives.")
    (license license:expat)))

(define-public rust-cached-proc-macro-0.21
  (package
    (name "rust-cached-proc-macro")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cached_proc_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sd6a4b0x7ijgkk448z7c20irj4vxjwpn66bra5nrnhp7dzsa6kp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling" ,rust-darling-0.20)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/jaemk/cached")
    (synopsis
     "Generic cache implementations and simplified function memoization")
    (description
     "This package provides Generic cache implementations and simplified function memoization.")
    (license license:expat)))

(define-public rust-cached-0.51
  (package
    (name "rust-cached")
    (version "0.51.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cached" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15d2wnzil7c4bk2q8dz4fjyrsjyalqbxglnxgw8acggd3qan9sqg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ahash" ,rust-ahash-0.8)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-cached-proc-macro" ,rust-cached-proc-macro-0.21)
        ("rust-cached-proc-macro-types" ,rust-cached-proc-macro-types-0.1)
        ("rust-directories" ,rust-directories-5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-hashbrown" ,rust-hashbrown-0.14)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-r2d2" ,rust-r2d2-0.8)
        ("rust-redis" ,rust-redis-0.25)
        ("rust-rmp-serde" ,rust-rmp-serde-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sled" ,rust-sled-0.34)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-copy-dir" ,rust-copy-dir-0.1)
        ("rust-googletest" ,rust-googletest-0.11)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-smartstring" ,rust-smartstring-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/jaemk/cached")
    (synopsis
     "Generic cache implementations and simplified function memoization")
    (description
     "This package provides Generic cache implementations and simplified function memoization.")
    (license license:expat)))

(define-public rust-build-html-2
  (package
    (name "rust-build-html")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "build_html" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p4k25yk3v0wf720wl5zcghvc9ik6l7lsh3fz86cq3g7x4nbhpi2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/skubalj/build_html")
    (synopsis "Library for basic server-side rendering written in pure rust")
    (description
     "This package provides Library for basic server-side rendering written in pure rust.")
    (license license:expat)))

(define-public rust-brotli-decompressor-4
  (package
    (name "rust-brotli-decompressor")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli-decompressor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qn39c7n6wm40i2bm0d3q2qslmaavlh804iv0ccbba4m80pbsics"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
        ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2))))
    (home-page "https://github.com/dropbox/rust-brotli-decompressor")
    (synopsis
     "brotli decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib's allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. Alternatively, --features=unsafe turns off array bounds checks and memory initialization but provides a safe interface for the caller.  Without adding the --features=unsafe argument, all included code is safe. For compression in addition to this library, download https://github.com/dropbox/rust-brotli")
    (description
     "This package provides a brotli decompressor that with an interface avoiding the
rust stdlib.  This makes it suitable for embedded devices and kernels.  It is
designed with a pluggable allocator so that the standard lib's allocator may be
employed.  The default build also includes a stdlib allocator and stream
interface.  Disable this with --features=no-stdlib.  Alternatively,
--features=unsafe turns off array bounds checks and memory initialization but
provides a safe interface for the caller.  Without adding the --features=unsafe
argument, all included code is safe.  For compression in addition to this
library, download https://github.com/dropbox/rust-brotli.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-brotli-7
  (package
    (name "rust-brotli")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "brotli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g99xay61mds9d23fnfj5gfbd6g11gihfgs3y1abljwldzqvi5yc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alloc-no-stdlib" ,rust-alloc-no-stdlib-2)
                       ("rust-alloc-stdlib" ,rust-alloc-stdlib-0.2)
                       ("rust-brotli-decompressor" ,rust-brotli-decompressor-4)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page "https://github.com/dropbox/rust-brotli")
    (synopsis
     "brotli compressor and decompressor that with an interface avoiding the rust stdlib. This makes it suitable for embedded devices and kernels. It is designed with a pluggable allocator so that the standard lib's allocator may be employed. The default build also includes a stdlib allocator and stream interface. Disable this with --features=no-stdlib. All included code is safe.")
    (description
     "This package provides a brotli compressor and decompressor that with an
interface avoiding the rust stdlib.  This makes it suitable for embedded devices
and kernels.  It is designed with a pluggable allocator so that the standard
lib's allocator may be employed.  The default build also includes a stdlib
allocator and stream interface.  Disable this with --features=no-stdlib.  All
included code is safe.")
    (license (list license:bsd-3 license:expat))))

(define-public rust-arc-swap-1
  (package
    (name "rust-arc-swap")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "arc-swap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-adaptive-barrier" ,rust-adaptive-barrier-1)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/vorner/arc-swap")
    (synopsis "Atomically swappable Arc")
    (description "This package provides Atomically swappable Arc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-recursion-1
  (package
    (name "rust-async-recursion")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-recursion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-futures-executor" ,rust-futures-executor-0.3)
                                   ("rust-macrotest" ,rust-macrotest-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dcchut/async-recursion")
    (synopsis "Recursion for async functions")
    (description "This package provides Recursion for async functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-quick-xml-0.36
  (package
    (name "rust-quick-xml")
    (version "0.36.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quick-xml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zj3sjcjk6sn544wb2wvhr1km5f9cy664vzclygfsnph9mxrlr7p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-value" ,rust-serde-value-0.7)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4))))
    (home-page "https://github.com/tafia/quick-xml")
    (synopsis "High performance xml reader and writer")
    (description
     "This package provides High performance xml reader and writer.")
    (license license:expat)))

(define-public rust-never-0.1
  (package
    (name "rust-never")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "never" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "149whplrasa92hdyg0bfcih2xy71d6ln6snxysrinq3pm1dblsn9"))))
    (build-system cargo-build-system)
    (home-page
     "https://fuchsia.googlesource.com/fuchsia/+/master/garnet/lib/rust/never")
    (synopsis "stable version of the unstable never type (!)")
    (description
     "This package provides a stable version of the unstable never type (!).")
    (license license:bsd-3)))

(define-public rust-diligent-date-parser-0.1
  (package
    (name "rust-diligent-date-parser")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diligent-date-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10hidg41gzgqykqlqfs4l1wjjxparmiwnjzqccij4ji7jki7zkzn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/rust-syndication/diligent-date-parser")
    (synopsis "Library for parsing datetime in unknown format")
    (description
     "This package provides Library for parsing datetime in unknown format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-core-0.20
  (package
    (name "rust-derive-builder-core")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f5fg9cc9y53fx1fm4bh0s1yxwvc7v1zsircy8s054hkjq2glcbl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-darling" ,rust-darling-0.20)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis "Internal helper library for the derive_builder crate")
    (description
     "This package provides Internal helper library for the derive_builder crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-macro-0.20
  (package
    (name "rust-derive-builder-macro")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p2ijj442j30cvd1x5231jw8b4klyf65sl3rnxvri6zpbc1ygfja"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-derive-builder-core" ,rust-derive-builder-core-0.20)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs")
    (description
     "This package provides Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-derive-builder-0.20
  (package
    (name "rust-derive-builder")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yw7frfcgza5rm7f1ckrsy1qy0i6gim3blw12xm186d1wrzg6cyd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-derive-builder-macro" ,rust-derive-builder-macro-0.20))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs")
    (description
     "This package provides Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atom-syndication-0.12
  (package
    (name "rust-atom-syndication")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atom_syndication" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hz0kr3hrjv7dmlhv8mdm2ldpjbxlp2n0l543a6nbx8x439mwfia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-diligent-date-parser" ,rust-diligent-date-parser-0.1)
                       ("rust-never" ,rust-never-0.1)
                       ("rust-quick-xml" ,rust-quick-xml-0.36)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-syndication/atom")
    (synopsis
     "Library for serializing the Atom web content syndication format")
    (description
     "This package provides Library for serializing the Atom web content syndication format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rss-2
  (package
    (name "rust-rss")
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rss" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0myrjqfw7x7lxfyg249z87zsj3k1068syw6qdmnqrna0z1421s97"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-atom-syndication" ,rust-atom-syndication-0.12)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-never" ,rust-never-0.1)
                       ("rust-quick-xml" ,rust-quick-xml-0.36)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1))))
    (home-page "https://github.com/rust-syndication/rss")
    (synopsis "Library for serializing the RSS web content syndication format")
    (description
     "This package provides Library for serializing the RSS web content syndication format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-similar-2
  (package
    (name "rust-similar")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "similar" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vk89dx2mmjp81pmszsa1s3mpzvbiy4krvfbq3s3mc3k27wd9q8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))
       #:cargo-development-inputs (("rust-console" ,rust-console-0.15)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/mitsuhiko/similar")
    (synopsis "diff library for Rust")
    (description "This package provides a diff library for Rust.")
    (license license:asl2.0)))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.86")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k42k01nj5jbpxa2h7spcb6dyd77jws0xrm6h7xkw0rq3lq7b4p8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-automod" ,rust-automod-1)
                                   ("rust-flate2" ,rust-flate2-1)
                                   ("rust-insta" ,rust-insta-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-syn-test-suite" ,rust-syn-test-suite-0.0.0)
                                   ("rust-tar" ,rust-tar-0.4)
                                   ("rust-termcolor" ,rust-termcolor-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "This package provides Parser for Rust source code.")
    (license (list license:expat license:asl2.0))))

(define-public rust-prettyplease-0.2
  (package
    (name "rust-prettyplease")
    (version "0.2.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prettyplease" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cw0n68wb2d0qgcqm2w00af3zbidkclyrd2darylbl34bj4frlb4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-2)
                                   ("rust-proc-macro2" ,rust-proc-macro2-1)
                                   ("rust-quote" ,rust-quote-1)
                                   ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/prettyplease")
    (synopsis "minimal `syn` syntax tree pretty-printer")
    (description
     "This package provides a minimal `syn` syntax tree pretty-printer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-console-0.15
  (package
    (name "rust-console")
    (version "0.15.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sz4nl9nz8pkmapqni6py7jxzi7nzqjxzb3ya4kxvmkb0zy867qf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-encode-unicode" ,rust-encode-unicode-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/console-rs/console")
    (synopsis "terminal and console abstraction for Rust")
    (description
     "This package provides a terminal and console abstraction for Rust.")
    (license license:expat)))

(define-public rust-rustc-hash-2
  (package
    (name "rust-rustc-hash")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lni0lf846bzrf3jvci6jaf4142n1mdqxvcpczk5ch9pfgyk8c2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/rustc-hash")
    (synopsis "speedy, non-cryptographic hashing algorithm used by rustc")
    (description
     "This package provides a speedy, non-cryptographic hashing algorithm used by
rustc.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rinja-parser-0.3
  (package
    (name "rust-rinja-parser")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rinja_parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "046602hy9x1q3np3qm64xpkw1xx64kiyjikyn8gpl2p0w9kaiyck"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://github.com/rinja-rs/rinja")
    (synopsis "Parser for Rinja templates")
    (description "This package provides Parser for Rinja templates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pulldown-cmark-escape-0.11
  (package
    (name "rust-pulldown-cmark-escape")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulldown-cmark-escape" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bp13akkz52p43vh2ffpgv604l3xd9b67b4iykizidnsbpdqlz80"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/raphlinus/pulldown-cmark")
    (synopsis
     "An escape library for HTML created in the pulldown-cmark project")
    (description
     "This package provides An escape library for HTML created in the pulldown-cmark project.")
    (license license:expat)))

(define-public rust-pulldown-cmark-0.12
  (package
    (name "rust-pulldown-cmark")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulldown-cmark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "055v2bzzrkrbrc1s6l9mbkvpdkhkid5j7vdkpcnc9k7b582s4szq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-pulldown-cmark-escape" ,rust-pulldown-cmark-escape-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicase" ,rust-unicase-2))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "pull parser for CommonMark")
    (description "This package provides a pull parser for @code{CommonMark}.")
    (license license:expat)))

(define-public rust-rinja-derive-0.3
  (package
    (name "rust-rinja-derive")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rinja_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12x1dfrjxhzfai2izmrqpbplj1aifkq1a58vby1f5xmf8q0yvn88"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-basic-toml" ,rust-basic-toml-0.1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.12)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-rinja-parser" ,rust-rinja-parser-0.3)
                       ("rust-rustc-hash" ,rust-rustc-hash-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-console" ,rust-console-0.15)
                                   ("rust-prettyplease" ,rust-prettyplease-0.2)
                                   ("rust-similar" ,rust-similar-2)
                                   ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/rinja-rs/rinja")
    (synopsis "Procedural macro package for Rinja")
    (description "This package provides Procedural macro package for Rinja.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rinja-0.3
  (package
    (name "rust-rinja")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rinja" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "198ppf5bnm6q53dhn4nijl9vbrdm49i1z86msyrk0m2r006r9i1x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-humansize" ,rust-humansize-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rinja-derive" ,rust-rinja-derive-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://rinja.readthedocs.io/")
    (synopsis "Type-safe, compiled Jinja-like templates for Rust")
    (description
     "This package provides Type-safe, compiled Jinja-like templates for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-path-macros-internal-0.1
  (package
    (name "rust-serde-json-path-macros-internal")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json_path_macros_internal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p1ijw024gfb7rl56csda8v48sfkf8pmjigw272dyy7dsahybpbm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/hiltontj/serde_json_path")
    (synopsis "Internal macro implementation for the serde_json_path crate")
    (description
     "This package provides Internal macro implementation for the serde_json_path crate.")
    (license license:expat)))

(define-public rust-serde-json-path-macros-0.1
  (package
    (name "rust-serde-json-path-macros")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json_path_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08xpphpj0pcckgkwbm76asvdz4bqmr32kw8p97lx6gs4g8byhc9a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-inventory" ,rust-inventory-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde-json-path-core" ,rust-serde-json-path-core-0.1)
                       ("rust-serde-json-path-macros-internal" ,rust-serde-json-path-macros-internal-0.1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/hiltontj/serde_json_path")
    (synopsis "Macros for the serde_json_path crate")
    (description "This package provides Macros for the serde_json_path crate.")
    (license license:expat)))

(define-public rust-serde-json-path-core-0.1
  (package
    (name "rust-serde-json-path-core")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json_path_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ybx7y7ihkhpkyyxrgjl33k3fsmbnv9lcarbx8ds7ap17kjlzmm3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-inventory" ,rust-inventory-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/hiltontj/serde_json_path")
    (synopsis "Core types for the serde_json_path crate")
    (description
     "This package provides Core types for the serde_json_path crate.")
    (license license:expat)))

(define-public rust-serde-json-path-0.6
  (package
    (name "rust-serde-json-path")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json_path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fikgag10adg9zb2vc57bi1bdasjljpak6p3l6pkx2aicdxj1h0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-inventory" ,rust-inventory-0.3)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-json-path-core" ,rust-serde-json-path-core-0.1)
                       ("rust-serde-json-path-macros" ,rust-serde-json-path-macros-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1))
       #:cargo-development-inputs (("rust-test-log" ,rust-test-log-0.2)
                                   ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))))
    (home-page "https://github.com/hiltontj/serde_json_path")
    (synopsis "Query serde_json Values using JSONPath")
    (description
     "This package provides Query serde_json Values using JSONPath.")
    (license license:expat)))

(define-public redlib
  (package
    (name "redlib")
    (version "0.35.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/redlib-org/redlib")
         (commit "d17d097b12b227f2e783a05cbd1e37c80f9ebe0b")))
       (sha256
        (base32
         "0h07ql3ci7flxg34pynar5w4xv6337fy533662a0n59k3fslag9i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f            ; FIXME: some tests fail due to lack of internet
       #:cargo-inputs
       (("rust-async-recursion" ,rust-async-recursion-1)
        ("rust-arc-swap" ,rust-arc-swap-1)
        ("rust-askama" ,rust-askama-0.12)
        ("rust-base64" ,rust-base64-0.22)
        ("rust-brotli" ,rust-brotli-7)
        ("rust-build-html" ,rust-build-html-2)
        ("rust-cached" ,rust-cached-0.51)
        ("rust-clap" ,rust-clap-4)
        ("rust-cookie" ,rust-cookie-0.18)
        ("rust-dotenvy" ,rust-dotenvy-0.15)
        ("rust-fastrand" ,rust-fastrand-2)
        ("rust-futures-lite" ,rust-futures-lite-2)
        ("rust-hyper" ,rust-hyper-0.14)
        ;; Use hyper-rustls 0.24 to fix issue with loading pages; see:
        ;; https://www.github.com/redlib-org/redlib/issues/229
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.24)
        ("rust-libflate" ,rust-libflate-2)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
        ("rust-regex" ,rust-regex-1)
        ("rust-rinja" ,rust-rinja-0.3)
        ("rust-rss" ,rust-rss-2)
        ("rust-route-recognizer" ,rust-route-recognizer-0.3)
        ("rust-rust-embed" ,rust-rust-embed-8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-json-path" ,rust-serde-json-path-0.6)
        ("rust-serde-yaml" ,rust-serde-yaml-0.9)
        ("rust-time" ,rust-time-0.3)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs
       (("rust-lipsum" ,rust-lipsum-0.9)
        ("rust-sealed-test" ,rust-sealed-test-1))))
    (home-page "https://github.com/redlib-org/redlib")
    (synopsis "Alternative private front-end to Reddit")
    (description
     "This package provides Alternative private front-end to Reddit.")
    (license license:agpl3)))
