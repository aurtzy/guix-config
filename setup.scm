#! /usr/bin/env -S guix repl --
!#
;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This script is useful for setting up initial installation tasks for Guix
;; System.

;; TODO this script has not been thoroughly tested yet. remove this comment
;; once it has been confirmed to be robust enough.

(use-modules (guix build utils)
             (ice-9 exceptions)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 regex)
             (ice-9 textual-ports))

(define (display-file-device-info path)
  (format #t
          "File info: ~s\n device: ~s\n offset: ~s\n"
          path
          (match:substring
           (string-match
            "/[^\n]*"
            (let* ((port (open-input-pipe
                          (string-append
                           "df --output=source "path)))
                   (str (get-string-all port)))
              (close-pipe port)
              str)))
          (match:substring
           (string-match
            "\n *0:[^:]*: *([0-9]+)\\."
            (let* ((port (open-input-pipe
                          (string-append
                           "filefrag -e "path)))
                   (str (get-string-all port)))
              (close-pipe port)
              str))
           1)))

(define (format-help . lines)
  (string-join lines "\n" 'suffix))

(define (display-setup-help)
  (display
   (format-help
    "This is a setup script for handling tasks during a Guix System"
    "installation."
    ""
    "Commands:"
    "  swapfile [--type=FILESYSTEM] PATH SIZE"
    "      Sets up swapfile (with SIZE in MiB) at PATH for FILESYSTEM"
    "      type (default=btrfs). Outputs information on completion"
    "      that should be included in system config."
    "      Currently supported types: btrfs"))
  (exit 1))

(define (setup-swapfile-getopts args)
  (getopt-long args
               `((type (value
                        #t)
                       (predicate
                        ,(lambda (value)
                           (member value
                                   '("btrfs"))))))))

(define (setup-swapfile args)
  (let* ((options (setup-swapfile-getopts args))
         (cmd-args (option-ref options '() #f))
         (type (option-ref options 'type "btrfs")))
    ;; type isn't actually used here since btrfs is the only type so far, but
    ;; in the future this setup should make it easier to add more types
    (match cmd-args
      ((path size)
       (when (file-exists? path)
         (delete-file path))
       (invoke "touch" path)
       (invoke "chattr" "+C" path)
       (invoke "dd"
               "if=/dev/zero"
               (string-append "of="path)
               "bs=1MiB"
               (string-append "count="size))
       (chmod path #o600)
       (invoke "mkswap" path)
       (invoke "swapon" path)
       (format (current-error-port) "\n")
       (display-file-device-info path))
      (else
       (display-setup-help)))))

(define (main args)
  (let ((cmd-args (if (null? args)
                      args
                      (cdr args))))
    (match cmd-args
      (("swapfile" _ ...)
       (setup-swapfile cmd-args))
      (else
       (display-setup-help)))))

(main (command-line))
