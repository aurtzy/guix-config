#! /usr/bin/env -S guix repl --
!#

(define $config-dir (dirname (current-filename)))

;; Add relevant load paths from config before loading other modules
(add-to-load-path (string-append $config-dir "/modules"))
(setenv "GUIX_CONFIG_DIR" $config-dir)
(use-modules (my-guix config))
(map (lambda (path)
       (add-to-load-path path))
     $package-paths)

(use-modules (guix scripts home)
             (ice-9 getopt-long)
             (ice-9 match))

(define reconfigure-help
  (string-join
   (list
    "This is a simple bootstrap script for setting up configs."
    "It should be located in the same directory as $CONFIG_DIR."
    "The following commands are available:"
    "  home"
    "  system (wip)")
   "\n"
   'suffix))

(define (getopts args)
  (getopt-long args
               '((help (single-char #\h)))))

(define (main args)
  (let* ((options (getopts args))
         (cmd-args (option-ref options '() #f))
         (cmd (if (null? cmd-args)
                  #f
                  (car cmd-args))))
    (if (or (not cmd)
            (option-ref options 'help #f))
        (display reconfigure-help)
        (match (car cmd-args)
          ("home"
           (apply guix-home "reconfigure"
                  (string-append $config-dir "/home.scm")
                  (cdr cmd-args)))
          ("system"
           (display "this command does nothing... for now.\n"))
          (_
           (display-help)
           (exit 1))))))

(main (command-line))
