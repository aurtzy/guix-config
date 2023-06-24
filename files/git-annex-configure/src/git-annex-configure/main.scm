;; Copyright (C) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is part of git-annex-configure.
;;
;; git-annex-configure is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; git-annex-configure is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; git-annex-configure. If not, see <https://www.gnu.org/licenses/>.

(define-module (git-annex-configure main)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure git config)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex config)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure git annex matchexpression)
  #:use-module (git-annex-configure git annex remote)
  #:use-module (git-annex-configure git annex remotes)
  #:use-module (git-annex-configure logging)
  #:use-module (git-annex-configure spec)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (oop goops)
  #:use-module (rnrs conditions)
  #:export (VERSION
            CONFIGURATION-FILE
            load-configuration
            annex-configure))

;; TODO make it so that I only have to edit version in one place? (right now
;; needs to be edited here and guix.scm)
(define VERSION "1.0")

;; Relative path from git-annex repository to configuration file.
(define CONFIGURATION-FILE (or (getenv "ANNEX_CONFIG_FILE")
                               ".annex.scm"))

(define-method (load-configuration (self <annex-repository>))
  "Load the configuration file from a git-annex repository and return the
 resulting evaluation."
  (with-exception-handler
   (lambda (exn)
     (raise-exception
      (make-exception
       (make-external-error)
       (make-exception-with-message
        "~a: ~s")
       (make-exception-with-irritants
        (list "Unable to load configuration from repository"
              (repository-path-ref self)))
       exn)))
   (lambda ()
     ;; Load the most recent possible config - file may be unadded in
     ;; non-bare repositories, in which case we primitive-load instead
     ;; of using cat-file
     (let ((toplevel (repository-toplevel-ref self)))
       (save-module-excursion
        (lambda ()
          ;; spec module will always be needed to construct configuration, so we
          ;; can have this be the current module during configuration loading
          (set-current-module (resolve-module '(git-annex-configure spec)))
          (let ((result
                 (if toplevel
                     (primitive-load (string-append
                                      toplevel"/"CONFIGURATION-FILE))
                     (eval-string (cat-file self CONFIGURATION-FILE)))))
            (cond
             ((configuration? result)
              result)
             (else
              (raise-exception
               (make-exception
                (make-external-error)
                (make-exception-with-message
                 "~a: ~s")
                (make-exception-with-irritants
                 (list "Expected evaluation to be a <configuration> record"
                       result)))))))))))))

(define-method (annex-configure (self <annex-repository>)
                                . options)
  "Load the configuration file from a git-annex repository and apply it."
  (format-log $info "Repository to configure: ~s\n"
              (or (repository-toplevel-ref self)
                  (repository-git-dir-ref self)))
  (let ((configuration (load-configuration self))
        (self-uuid (config-ref self "annex.uuid")))

    ;; Apply global configurations
    (let ((annex-config-items (configuration-annex-config configuration)))
      (when annex-config-items
        (format-log $info "Configuring git-annex config...")
        (for-each
         (lambda (item)
           (let ((key (car item))
                 (value (cdr item)))
             (unless (equal? value
                             (annex-config-ref self key))
               (annex-config-set! self key value))))
         annex-config-items)
        (newline)))
    (let ((groupwanted-items (configuration-groupwanted configuration)))
      (when groupwanted-items
        (format-log $info "Configuring git-annex groupwanted...")
        (for-each
         (lambda (item)
           (let ((group (car item))
                 (expr (cdr item)))
             (groupwanted-set! self group expr)))
         groupwanted-items)
        (newline)))

    ;; Apply non-disabled repo-specific configs for each uuid specified in
    ;; config
    (let ((repo-configs (filter
                         (lambda (repo-config)
                           (not (configuration-disabled? repo-config)))
                         (configuration-repositories configuration))))
      (for-each
       (lambda (repo-config)
         (let ((uuid (configuration-uuid repo-config)))
           (format-log $info
                       (string-append "~a: ~s~a\n")
                       "Configuring repository uuid"
                       uuid
                       (if (equal? self-uuid uuid)
                           " [self]"
                           ""))
           
           (let ((description (configuration-description repo-config)))
             (when description
               (format-log $info "Configuring git-annex description...")
               (description-set! self
                                 description
                                 #:remote uuid)
               (newline)))
           (let ((wanted (configuration-wanted repo-config)))
             (when wanted
               (format-log $info "Configuring git-annex wanted...")
               (wanted-set! self
                            wanted
                            #:remote uuid)
               (newline)))
           (let ((required (configuration-required repo-config)))
             (when required
               (format-log $info "Configuring git-annex required...")
               (required-set! self
                              required
                              #:remote uuid)
               (newline)))
           (let ((groups (configuration-groups repo-config)))
             (when groups
               (format-log $info "Configuring git-annex groups...")
               (groups-set! self
                            groups
                            #:remote uuid)
               (newline)))

           ;; The following configurations cannot be set from another
           ;; repository, thus we only run them on the current one.
           (when (equal? self-uuid uuid)
             (let ((config-items (configuration-config repo-config)))
               (when config-items
                 (format-log $info "Configuring git config...")
                 (for-each
                  (lambda (item)
                    (let ((key (car item))
                          (value (cdr item)))
                      (config-set! self key value)))
                  config-items)
                 (newline)))
             (let ((remotes (configuration-remotes repo-config)))
               (when remotes
                 (format-log $info "Configuring git remotes...")
                 (remotes-set! self remotes)
                 (newline)))
             ;; TODO perhaps write compiled hook scripts instead to remove need
             ;; for propagated guile input?
             (let ((hooks (configuration-hooks repo-config)))
               (when hooks
                 (format-log $info "Configuring hooks...")
                 (for-each
                  (lambda (hook)
                    (let* ((name (car hook))
                           (script (cdr hook))
                           (file-path (string-join
                                       (list
                                        (repository-git-dir-ref self)
                                        "hooks"
                                        name)
                                       "/")))
                      ;; Only write if hook script has changed
                      (unless (and (file-exists? file-path)
                                   (equal? script
                                           (call-with-port (open-input-file
                                                            file-path)
                                             (lambda (port)
                                               (read port)))))
                        (call-with-port (open-output-file file-path)
                          (lambda (port)
                            (format port "~a\n~s\n"
                                    "#! /usr/bin/env -S guile -s\n!#"
                                    script)
                            (chmod port
                                   (logior #o111 (stat:perms
                                                  (stat port)))))))))
                  hooks)
                 (newline))))))
       repo-configs))))

(define (format-usage usage . rest)
  (string-join
   (cons
    (string-append "Usage: "$prog-name" "usage)
    rest)
   "\n"))

(define main-help
  (format-usage
   "--help | PATH PATH..."
   "Read the configuration file from a git-annex repository path and apply the"
   "configuration."
   ""
   "Options:"
   "  -h, --help"
   "      Display this help message."
   "  -q, --quiet"
   "      Suppress logs. Only warnings and error messages will be printed."
   "  --version"
   "      Display the current version of this program."))

(define (main-getopts args)
  (getopt-long args
               '((help (single-char #\h))
                 (quiet (single-char #\q))
                 (version))))

(define-public (git-annex-configure args)
  (let* ((options (main-getopts args))
         (paths (option-ref options '() #f)))
    ;; Order of conditions for setting log level is important here; prioritize
    ;; more verbosity
    (log-level-set! (cond
                     ((getenv "DEBUG") $debug)
                     ((option-ref options 'quiet #f) $warning)
                     (else $info)))
    (cond
     ((option-ref options 'version #f)
      (display VERSION)
      (newline))
     ((or (option-ref options 'help #f)
          (null? paths))
      (display main-help)
      (newline))
     (else
      (for-each
       annex-configure
       ;; Uniquify repository paths so we don't configure the same one multiple
       ;; times
       (map
        annex-repository
        (delete-duplicates paths)))))))

(define-public (main args)
  "Entry point."
  (log-level-set! $debug)
  (let ((exception-with-kind-and-args?
         (exception-predicate &exception-with-kind-and-args))
        (last-exn
         (make-exception-with-message "this message should never be seen")))
    (guard
     (_ ;; for non-continuable exceptions
      ((or (programming-error? last-exn)
           (equal? $debug (log-level-ref)))
       (raise-exception last-exn))
      (else
       (exit 1)))
     (with-exception-handler
      ;; for continuable exceptions
      (lambda (exn)
        ;; if non-continuable is intercepted here, pass it up
        (when (non-continuable-error? exn)
          (raise-exception exn))
        
        (when (exception-with-message? exn)
          (let ((exns (simple-exceptions exn)))
            (apply format-log
                   $error
                   (string-join
                    (map exception-message
                         (filter exception-with-message? exns))
                    "\n |- ")
                   (apply append
                          (map exception-irritants
                               (filter exception-with-irritants?
                                       exns))))))
        (set! last-exn exn))
      (lambda ()
        (git-annex-configure args))))))
