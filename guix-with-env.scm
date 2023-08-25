#!/usr/bin/env -S guix repl --
!#

;;; The location of this script is important - it expects the directory it is
;;; in to have a ./modules which should store the Guile modules for Guix to
;;; find.

(setenv "GUIX_PACKAGE_PATH"
        (format #f
                "~a/modules:~a"
                (dirname (current-filename))
                (getenv "GUIX_PACKAGE_PATH")))

(apply system* "guix" (cdr (command-line)))
