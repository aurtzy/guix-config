;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode
  .
  ((eval . (guix-devel-mode t))
   (eval . (put 'mod 'scheme-indent-function 0))
   (eval . (put 'compose-lambda 'scheme-indent-function 1)))))
