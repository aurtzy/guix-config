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

(define-module (git-annex-configure git repository)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:export (<repository>
            repository-path-ref
            repository-git-dir-ref
            repository-toplevel-ref
            repository

            invoke-git
            cat-file))

;;; BEGIN Classes

(define-class <repository> ()
  ;; Path specifying some location of a git repository.
  (path #:init-keyword #:path
        #:getter repository-path-ref)
  ;; Path to git directory.
  (git-dir #:allocation #:virtual
           #:getter repository-git-dir-ref
           #:slot-ref
           (lambda (self)
             (with-chdir
              (repository-path-ref self)
              (lambda ()
                (canonicalize-path
                 (capture-output*
                  (lambda ()
                    (invoke "git"
                            "rev-parse"
                            "--git-dir")))))))
           #:slot-set!
           (lambda (self path)
             (slot-set! self 'path path)))
  ;; Toplevel path of a repository. Value may be #f if path is not in a work
  ;; tree; i.e. repository is *likely* bare (commands get funky if run in the
  ;; git dir of a non-bare repo, and there may be other edge cases)
  (toplevel #:allocation #:virtual
            #:getter repository-toplevel-ref
            #:slot-ref
            (lambda (self)
              (with-chdir
               (repository-path-ref self)
               (lambda ()
                 (if (equal? "true"
                             (capture-output*
                              (lambda ()
                                (false-if-exception
                                 (with-error-to-void
                                  (lambda ()
                                    (invoke "git"
                                            "rev-parse"
                                            "--is-inside-work-tree")))))))
                     (canonicalize-path
                      (capture-output*
                       (lambda ()
                         (invoke "git"
                                 "rev-parse"
                                 "--show-toplevel"))))
                     #f))))
            #:slot-set!
            (lambda (self path)
              (slot-set! self 'path path))))

(define-method (repository path)
  (make <repository> #:path path))

(define-method (initialize (self <repository>)
                           initargs)
  (next-method)
  (let ((path (repository-path-ref self)))
    (with-chdir
     path
     (lambda ()
       (unless (false-if-exception
                (with-error-to-void
                 (lambda ()
                   (invoke "git" "rev-parse"))))
         (raise-exception
          (make-exception
           (make-external-error)
           (make-exception-with-message
            "~a: ~s")
           (make-exception-with-irritants
            (list "Not in a git repository"
                  path)))))))
    (slot-set! self 'path
               (canonicalize-path path))))

;;; END Classes

(define-method (invoke-git (self <repository>)
                           . args)
  (with-chdir
   (repository-path-ref self)
   (lambda ()
     (apply invoke
            "git"
            args))))

(define-method (cat-file (self <repository>)
                         (subpath <string>)
                         . options)
  "Agnostic method of retrieving files from a git repository, which works even
when it is bare. When the file does not exist, an external error will be raised.

Note that only committed files are retrieved. If the repository is non-bare and
there are uncommitted files, those files will /not/ be the ones that are read."
  (let-keywords
      options #f ((branch "HEAD"))
    (capture-output
     (lambda ()
       (invoke-git self
                   "cat-file"
                   "-p"
                   "--"
                   (string-append
                    branch":"subpath))))))
