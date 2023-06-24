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

(define-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:export (<annex-repository>
            annex-repository

            invoke-git-annex
            description-set!
            annex-index-add!
            annex-index-sync!))

;;; BEGIN Classes

(define-class <annex-repository> (<repository>))

(define-method (annex-repository path)
  (make <annex-repository> #:path path))

(define-method (initialize (self <annex-repository>)
                           initargs)
  (next-method)
  (let ((path (repository-path-ref self)))
    (with-chdir
     path
     (lambda ()
       ;; Check if path is valid by running a command that only succeeds when
       ;; inside a git-annex repository
       (unless (false-if-exception
                (invoke "git-annex"
                        "matchexpression"
                        ""))
         (raise-exception
          (make-exception
           (make-external-error)
           (make-exception-with-message
            "~a: ~s")
           (make-exception-with-irritants
            (list "Not in a git-annex repository"
                  path)))))))))

;;; END Classes

(define-method (invoke-git-annex (self <annex-repository>)
                                 . args)
  (with-chdir
   (repository-path-ref self)
   (lambda ()
     (apply invoke
            "git-annex"
            "--quiet"
            args))))

;; TODO It seems like the only way to get description is to parse json
;; from "git-annex info --json"... Unsure if I should get a parser
;; just for this, so for now querying description will not be possible
;; until I discover something new or decide to bring a parser in.
(define-method (description-set! (self <annex-repository>)
                                 (desc <string>)
                                 . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "describe"
                      "--"
                      remote
                      desc)))

(define-method (annex-index-add! (self <annex-repository>)
                                 (path <string>))
  (invoke-git-annex self
                    "add"
                    "--"
                    path))

(define-method (annex-index-sync! (self <annex-repository>)
                                  . options)
  (let-keywords
      options #f ((content? '())
                  (remotes '()))
    (apply invoke-git-annex
           self
           "sync"
           (append
            remotes
            (match content?
              (() '())
              (#t '("--content"))
              (#f '("--no-content")))))))
