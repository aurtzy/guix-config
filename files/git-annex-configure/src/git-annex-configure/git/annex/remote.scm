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

(define-module (git-annex-configure git annex remote)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (rnrs conditions)
  #:export (<special-remote>))

;; git-annex special remote interface and specifications
;;
;; With the exception of the name slot - which must always be a valid
;; <remote-name> - all other slots are expected to be possibly unbound to handle
;; cases where no parameter is specified, since all parameters are optional

(define-class <special-remote> (<remote>))

(define-method (initialize (self <special-remote>)
                           initargs)
  ;; URLs are optional for enabling remotes, so next-method will not be called
  (let-keywords
   initargs #t ((name '())
                (url '()))
   (when (null? name)
     (raise-exception
      (make-exception
       (make-assertion-violation)
       (make-exception-with-message
        "Special remote requires name to be set"))))
   (slot-set! self 'name (remote-name name))
   (unless (null? url)
     (slot-set! self 'url url))))

(define-method (display (self <special-remote>)
                        port)
  (format port
          "~s"
          self))

(define-method (write (self <special-remote>)
                      port)
  (format port
          "#<~a ~s>"
          (class-name (class-of self))
          (remote-name-ref self)))

(define-method (%add-remote! (self <annex-repository>)
                             (remote <special-remote>))
  (raise-exception
   (make-exception
    (make-implementation-restriction-violation)
    (make-exception-with-message
     "~a: ~a")
    (make-exception-with-irritants
     (list "%add-remote! not implemented for class"
           (class-name (class-of remote)))))))
