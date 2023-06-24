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

(define-module (git-annex-configure git annex remotes)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex remote)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (rnrs conditions)
  #:export (<borg-remote>
            remote-subdir-ref
            remote-appendonly?
            borg-remote))

;;; BEGIN borg remote

(define-class <borg-remote> (<special-remote>)
  (subdir #:init-keyword #:subdir
          #:getter remote-subdir-ref)
  (appendonly? #:init-keyword #:appendonly
               #:getter remote-appendonly?))

(define-method (borg-remote name . params)
  (apply make
         <borg-remote>
         #:name name
         (map
          (lambda (param)
            (match param
              (#:borgrepo
               #:url)
              (_
               param)))
          params)))

(define-method (initialize (self <borg-remote>)
                           initargs)
  (next-method)
  (let-keywords
   initargs #t ((subdir #f)
                (appendonly? '()))
    (when subdir
      (cond
       ((is-a? subdir <string>)
        (slot-set! self 'subdir subdir))
       (else
        (raise-exception
         (make-exception
          (make-assertion-violation)
          (make-exception-with-message
           "~a: ~s")
          (make-exception-with-irritants
           (list "Borg remote subdir not a string"
                 subdir)))))))
    (unless (null? appendonly?)
      (cond
       ((is-a? appendonly? <boolean>)
        (slot-set! self 'appendonly? appendonly?))
       (else
        (raise-exception
         (make-exception
          (make-assertion-violation)
          (make-exception-with-message
           "~a: ~s")
          (make-exception-with-irritants
           (list "Borg remote appendonly? flag not a boolean"
                 appendonly?)))))))))

(define-method (%remote-add! (self <annex-repository>)
                             (remote <borg-remote>))
  (apply invoke-git-annex
         self
         "enableremote"
         "--"
         (remote-name-ref (remote-name-ref remote))
         (append
          (if (slot-bound? remote 'url)
              (list (string-append "borgrepo="(remote-url-ref remote)))
              '())
          (if (slot-bound? remote 'subdir)
              (list (string-append "subdir="(remote-subdir-ref remote)))
              '())
          (if (slot-bound? remote 'appendonly?)
              (list (string-append "appendonly="(if (remote-appendonly? remote)
                                                    "yes"
                                                    "no")))
              '()))))

;;; END borg remote
