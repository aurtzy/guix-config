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

(use-modules (guix build utils)
             (guix scripts system)
             (ice-9 exceptions)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 pretty-print)
             (ice-9 regex)
             (ice-9 textual-ports))

;; TODO this script been recently refactored. remove this comment once
;; everything looks good.

(define PROGRAM (car (program-arguments)))

(define CONFIG-FILE (string-append (dirname PROGRAM)
                                   "/system.scm"))

(define (format-usage . args)
  (format #f
          "Usage: ~a ~a"
          (basename PROGRAM)
          (string-join args " ")))

(define (display-help . lines)
  (display (string-join lines "\n" 'suffix))
  (exit 1))

(define (need-help? args)
  (or (<= (length args) 1)
      (member "--help" args)
      (member "-h" args)))

(define (display-swapfile-configuration path)
  (pretty-print
   `(swapfile-configuration
     (file ,path)
     (device ,(match:substring
               (string-match
                "/[^\n]*"
                (let* ((port (open-input-pipe
                              (string-append
                               "df --output=source "path)))
                       (str (get-string-all port)))
                  (close-pipe port)
                  str))))
     (offset ,(match:substring
               (string-match
                "\n *0:[^:]*: *([0-9]+)\\."
                (let* ((port (open-input-pipe
                              (string-append
                               "filefrag -e "path)))
                       (str (get-string-all port)))
                  (close-pipe port)
                  str))
               1)))))

(define (setup-swapfile-help)
  (display-help
   (format-usage "install [OPTION ...] PATH")
   "Set up swapfile for Guix System. Outputs information to be included in"
   "system configuration."
   ""
   "Options:"
   "  --type=FILESYSTEM"
   "      Set up swapfile for FILESYSTEM type. This option is required."
   "      Supported file systems: btrfs"
   "  --size=SIZE"
   "      Size of the swapfile in MiB. This option is required."))

(define (setup-swapfile-parse-options args)
  (getopt-long args
               `((type (value #t)
                       (predicate ,(lambda (value)
                                     (member value
                                             '("btrfs"))))
                       (required? #t))
                 (size (value #t)
                       (predicate ,(lambda (value)
                                     (false-if-exception
                                      (exact? (string->number value)))))
                       (required? #t)))))

(define (setup-swapfile args)
  (when (need-help? args)
    (setup-swapfile-help))
  (let* ((options (setup-swapfile-parse-options args))
         (cmd-args (option-ref options '() #f))
         (type (option-ref options 'type #f))
         (size (option-ref options 'size #f)))
    ;; type isn't actually used here since btrfs is the only type so far, but
    ;; in the future this setup should make it easier to add more types
    (match cmd-args
      ((path)
       (when (file-exists? path)
         (delete-file path))
       (invoke "touch" path)
       (invoke "chattr" "+C" path)
       (invoke "dd"
               "if=/dev/zero"
               (string-append "of=" path)
               "bs=1MiB"
               (string-append "count=" size))
       (chmod path #o600)
       (invoke "mkswap" path)
       (invoke "swapon" path)
       (format (current-error-port) "\n")
       (display-swapfile-configuration path))
      (else
       (setup-swapfile-help)))))

(define (setup-install-help)
  (display-help
   (format-usage "install [OPTION ...] TARGET")
   "Sets up Guix System at TARGET root using the system configuration from"
   "the guix-config that this script is located in."
   (string-append "(" (format #f "~s" CONFIG-FILE) ")")
   ""
   "Options: none"))

(define (setup-install-parse-options args)
  (getopt-long args
               '()))

(define (setup-install args)
  (when (need-help? args)
    (setup-install-help))
  (let* ((options (setup-install-parse-options args))
         (cmd-args (option-ref options '() #f)))
    (unless (file-exists? CONFIG-FILE)
      (format (current-error-port)
              "error: config file does not exist: ~s"
              CONFIG-FILE))
    (match cmd-args
      ((target)
       (invoke "herd" "start" "cow-store" (canonicalize-path target))
       (guix-system "init"
                    "-L" (dirname PROGRAM)
                    CONFIG-FILE
                    target))
      (else
       (setup-install-help)))))

(define (setup-help)
  (display-help
   (format-usage "COMMAND ARG ...")
   "Set up various tasks during a Guix System installation."
   ""
   "Commands:"
   "  swapfile"
   "  install"))

(define (main args)
  (let ((cmd-args (if (null? args)
                      args
                      (cdr args))))
    (match cmd-args
      (("swapfile" _ ...)
       (setup-swapfile cmd-args))
      (("install" _ ...)
       (setup-install cmd-args))
      (else
       (setup-help)))))

(main (program-arguments))
