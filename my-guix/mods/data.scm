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

;;; Commentary:
;;;
;;; This module includes mods relating to my "data infrastructure".

(define-module (my-guix mods data)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services syncthing)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (my-guix home services)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (<data-entry>
            data-entry data-entry?
            this-data-entry
            data-entry-source
            data-entry-borg-repository

            data-entries
            data-mod))

(use-package-modules backup haskell-apps)

(define-record-type* <data-entry>
  data-entry make-data-entry
  data-entry?
  this-data-entry
  ;; Path to main data source for this data entry that is written to and read
  ;; from, relative to $HOME if not absolute.
  (source data-entry-source
          (sanitize (lambda (value)
                      (rnrs:assert (string? value))
                      (if (absolute-file-name? value)
                          value
                          (path-append-my-home value)))))
  ;; Path to borg repository that data source is backed up to, if any.  Relative
  ;; to $HOME if path is not absolute.
  (borg-repository data-entry-borg-repository
                   (default #f)
                   (sanitize (lambda (value)
                               (rnrs:assert (or (eq? #f value)
                                                (string? value)))
                               (cond
                                ((eq? #f value) #f)
                                ((absolute-file-name? value) value)
                                (else (path-append-my-home value)))))))

;; data-entries: Parameter specifying user data entries.
;;
;; This is a list of <data-entry> records.
;;
;; When setting this parameter, entries may also be specified as strings
;; representing the data-entry source as a shorthand.
(define data-entries
  (make-parameter '()
                  (lambda (entries)
                    (map
                     (match-lambda
                       ((? data-entry? data-entry)
                        data-entry)
                       ((? string? string)
                        (data-entry (source string))))
                     entries))))

(define (data-backup-create-script)
  "Return a script that creates backups of data from the data-entries
parameter."
  (define borg-repos
    (fold
     (lambda (data-entry borg-repos)
       (match-record data-entry <data-entry> (source borg-repository)
         (if borg-repository
             (assoc-set! borg-repos
                         borg-repository
                         (cons source
                               (or (assoc-ref borg-repos borg-repository)
                                   '())))
             borg-repos)))
     '()
     (data-entries)))
  (program-file
   "data-backup-create"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (srfi srfi-26))
         (when #$(null? borg-repos)
           (display "No data sources to back up.")
           (exit #f))
         (for-each
          (match-lambda
            ((borg-repo sources ..1)
             (chdir (getenv "HOME"))
             (format #t "Creating backups for: ~s\n" borg-repo)
             (for-each (cut format #t "<- ~s\n" <>)
                       sources)
             (unless (false-if-exception
                      (apply
                       invoke
                       `(#$(file-append borg "/bin/borg")
                         "create"
                         "--stats"
                         ,@(let ((patterns-file
                                  (string-append borg-repo "/patterns")))
                             (if (file-exists? patterns-file)
                                 (list "--exclude-from" patterns-file)
                                 '()))
                         ;; Optimize for storage on an HDD
                         "--compression" "zstd,6"
                         ,(string-append borg-repo "::{utcnow}-auto")
                         "--"
                         ,@sources)))
               (display "Error encountered while backing up.\n")
               (exit #f))
             (sync)))
          '#$borg-repos)))))

(define data-mod
  (mod
    (name 'data)
    (description
     "Sets up my \"data infrastructure\" and provides additional utilities for
managing it.")
    (os-extension
     ;; Make packages available to ssh.
     (mod-os-packages
      (list git-annex)))
    (he-extension
     (compose
      (mod-he-packages
       (list borg
             git-annex
             git-annex-configure))
      (mod-he-services
       (list (service home-syncthing-service-type
                      (for-home
                       (syncthing-configuration
                        (user (getenv "USER")))))
             (simple-service name
                             home-files-service-type
                             `((".local/bin/data-backup-create"
                                ,(data-backup-create-script))))
             (simple-service name
                             home-impure-symlinks-service-type
                             (fold
                              (lambda (entry symlinks)
                                (match-record entry <data-entry> (source)
                                  (if (equal? (getenv "HOME")
                                              (dirname source))
                                      symlinks
                                      (cons (list (basename source) source)
                                            symlinks))))
                              '()
                              (data-entries)))))))))
