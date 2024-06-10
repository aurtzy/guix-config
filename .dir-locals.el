;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((magit-status-mode . ((magit-todos-exclude-globs . (".git/"
                                                     ".direnv/"
                                                     "/files/patches"))))
 (scheme-mode . ((eval . (guix-devel-mode t))
                 (eval . (put 'compose-lambda 'scheme-indent-function 1))
                 (eval . (put 'mod 'scheme-indent-function 0))
                 (eval . (put 'modded-system 'scheme-indent-function 0)))))
