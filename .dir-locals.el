;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  .
  ((disproject-custom-suffixes
    .
    (["Guix options"
      ("-a" "Allow downgrades" "--allow-downgrades")
      ("-da" "Disable authentication (pull)" "--disable-authentication")
      ("-df" "Disable flatpak" "--disable-flatpak")]
     ["Run guix..."
      :pad-keys t
      ("h r" "home reconfigure" disproject-compile
       :cmd (lambda (args)
              (interactive (list (transient-args transient-current-command)))
              (let ((allow-downgrades?
                     (transient-arg-value "--allow-downgrades" args))
                    (disable-flatpak?
                     (transient-arg-value "--disable-flatpak" args)))
                (concat
                 (if disable-flatpak? "GUIX_FLATPAK_DISABLE=1 " "")
                 "guix home reconfigure config.scm"
                 (if allow-downgrades? " --allow-downgrades" ""))))
       :buffer-id "guix-home"
       :comint? t)
      ("p" "pull" disproject-compile
       :cmd (lambda (args)
              (interactive (list (transient-args transient-current-command)))
              (let ((allow-downgrades?
                     (transient-arg-value "--allow-downgrades" args))
                    (disable-authentication?
                     (transient-arg-value "--disable-authentication" args)))
                (concat
                 "guix pull"
                 (if allow-downgrades? " --allow-downgrades" "")
                 (if disable-authentication?
                     " --disable-authentication"
                   ""))))
       :buffer-id "guix-pull"
       :comint? t)
      ("s r" "system reconfigure" disproject-compile
       :cmd (lambda (args)
              (interactive (list (transient-args transient-current-command)))
              (let ((allow-downgrades?
                     (transient-arg-value "--allow-downgrades" args)))
                (concat
                 "sudo guix system reconfigure config.scm"
                 (if allow-downgrades? " --allow-downgrades" ""))))
       :buffer-id "guix-system"
       :comint? t)]))))
 (magit-status-mode . ((magit-todos-exclude-globs . (".git/"
                                                     ".direnv/"
                                                     "/files/patches"))))
 (scheme-mode . ((eval . (guix-devel-mode t))
                 (eval . (put 'compose-lambda 'scheme-indent-function 1))
                 (eval . (put 'mod 'scheme-indent-function 0))
                 (eval . (put 'modded-system 'scheme-indent-function 0)))))
