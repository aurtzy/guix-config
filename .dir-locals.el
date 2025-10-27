;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  .
  ((disproject-custom-suffixes
    .
    (["Guix options"
      ("-a" "Allow downgrades" "--allow-downgrades")
      ("-da" "Disable authentication (pull)" "--disable-authentication")
      ("-df" "Disable flatpak" :cons 'disable-flatpak? :reader always)]
     ["Run guix..."
      :pad-keys t
      ("h r" "home reconfigure" disproject-compile
       :cmd (lambda (args disable-flatpak?)
              (interactive
               (let ((targs (transient-args transient-current-command)))
                 (list (seq-filter #'stringp targs)
                       (alist-get 'disable-flatpak? targs))))
              (concat
               (if disable-flatpak? "GUIX_FLATPAK_DISABLE=1 " "")
               "guix home reconfigure home.scm "
               (string-join args " ")))
       :buffer-id "guix-home")
      ("p" "pull" disproject-compile
       :cmd (lambda (args)
              (interactive
               (list (seq-filter #'stringp
                                 (transient-args transient-current-command))))
              (concat
               "guix pull "
               (string-join args " ")))
       :buffer-id "guix-pull")
      ("s r" "system reconfigure" disproject-compile
       :cmd (lambda (args)
              (interactive
               (list (seq-filter #'stringp
                                 (transient-args transient-current-command))))
              (concat
               "sudo guix system reconfigure system.scm "
               (string-join args " ")))
       :buffer-id "guix-system"
       :comint? t)]))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/"
                                                     ".direnv/"
                                                     "/files/patches"))))
 (scheme-mode . ((eval . (guix-devel-mode t))
                 (eval . (put 'mod-argument 'scheme-indent-function 0))
                 (eval . (put 'let-mod-arguments 'scheme-indent-function 2))
                 (eval . (put 'modded-configuration 'scheme-indent-function 0))
                 (eval . (put 'operating-system-mod 'scheme-indent-function 0))
                 (eval . (put 'home-environment-mod 'scheme-indent-function 0))
                 (eval . (put 'compose-lambda 'scheme-indent-function 1))
                 (eval . (put 'mod 'scheme-indent-function 0))
                 (eval . (put 'modded-system 'scheme-indent-function 0))
                 (eval . (put 'home-flatpak-configuration 'scheme-indent-function 0))
                 (eval . (put 'flatpak-overrides-configuration 'scheme-indent-function 0))
                 (eval . (put 'flatpak-app 'scheme-indent-function 0)))))
