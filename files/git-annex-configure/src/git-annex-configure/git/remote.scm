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

(define-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<remote-name>
            remote-name-ref
            remote-name

            <remote>
            remote-name-ref
            remote-url-ref
            remote

            <remotes>
            remotes-ref

            remote-url-ref
            remotes
            remote-ref
            remotes-ref
            %remote-remove!
            remote-remove!
            %remote-add!
            remote-set!
            remotes-set!))

;;; BEGIN Classes

(define-class <remote-name> ()
  (name #:init-keyword #:name
        #:getter remote-name-ref))

(define-method (remote-name name)
  (make <remote-name> #:name name))

(define (sanitize-remote-name name)
  (cond
   ((is-a? name <remote-name>)
    (remote-name-ref name))
   ((and (is-a? name <string>)
         (false-if-exception
          ;; Remote name validity does not seem to be documented, but using
          ;; check-ref-format should suffice according to
          ;; https://stackoverflow.com/a/41462742
          (invoke "git"
                  "check-ref-format"
                  ;; "--" is not supported here, so instead we append somewhat
                  ;; arbitrary strings to mimic a refname with the given
                  ;; remote name (which might look something like
                  ;; "refs/remotes/<remote-name>/...") and then pass it to
                  ;; check-ref-format
                  (string-append
                   "refs/remotes/"name"/etc"))))
    name)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a: ~s")
      (make-exception-with-irritants
       (list "Invalid remote name"
             name)))))))

(define-method (initialize (self <remote-name>)
                           initargs)
  (next-method)
  (let ((name (remote-name-ref self)))
    (slot-set! self 'name (sanitize-remote-name name))))

(define-method (display (self <remote-name>)
                        port)
  (format port
          "~a"
          (remote-name-ref self)))

(define-method (write (self <remote-name>)
                      port)
  (format port "#<~a ~s>"
          (class-name (class-of self))
          (remote-name-ref self)))

(define-class <remote> ()
  (name #:init-keyword #:name
        #:getter remote-name-ref)
  (url #:init-keyword #:url
       #:getter remote-url-ref))

(define-method (remote name url)
  (make <remote> #:name name #:url url))

(define-method (initialize (self <remote>)
                           initargs)
  (next-method)
  (let ((name (remote-name-ref self))
        (url (remote-url-ref self)))
    (slot-set! self 'name (remote-name name))))

(define-method (display (self <remote>)
                        port)
  (format port "~s" self))

(define-method (write (self <remote>)
                      port)
  (format port "#<~a ~s: ~s>"
          (class-name (class-of self))
          (remote-name-ref self)
          (remote-url-ref self)))

(define-class <remotes> ()
  (remotes #:init-keyword #:remotes
           #:getter remotes-ref))

(define-method (remotes . args)
  (make <remotes> #:remotes args))

(define-method (initialize (self <remotes>)
                           initargs)
  (next-method)
  (let ((remotes (remotes-ref self)))
    (unless (and (list? remotes)
                 (every (lambda (elem)
                          (is-a? elem <remote>))
                        remotes))
      (raise-exception
       (make-exception
        (make-assertion-failure)
        (make-exception-with-message
         "~a: ~s")
        (make-exception-with-irritants
         (list "Not a list of remotes"
               remotes)))))))

(define-method (display (self <remotes>)
                        port)
  (format port "~s" self))

(define-method (write (self <remotes>)
                      port)
  (format port
          "#<~a ~s>"
          (class-name (class-of self))
          (remotes-ref self)))

;;; END Classes

(define-method (remote-url-ref (self <repository>)
                               (name <remote-name>))
  "Return the remote url associated with the given name from repository. Value
 may be #f if no such remote exists with the name."
  (false-if-exception
   (capture-output*
    (lambda ()
      (with-error-to-void
       (lambda ()
         (invoke-git self
                     "remote"
                     "get-url"
                     "--"
                     (remote-name-ref name))))))))

(define-method (remote-url-ref (self <repository>)
                               (name <string>))
  (remote-url-ref self (remote-name name)))

(define-method (remote-ref (self <repository>)
                           (name <remote-name>))
  "Return the remote associated with the given name from repository. Value may
 be #f if no such remote exists with the name."
  (let ((url (remote-url-ref self name)))
    (if url
        (remote name url)
        #f)))

(define-method (remote-ref (self <repository>)
                           (name <string>))
  (remote-ref self (remote-name name)))

(define-method (remotes-ref (self <repository>))
  (apply remotes
         (filter-map
          (lambda (name)
            (if (string-null? name)
                #f
                (remote-ref self name)))
          (string-split
           (capture-output*
            (lambda ()
              (invoke-git self
                          "remote")))
           #\newline))))

(define-method (%remote-remove! (self <repository>)
                                (name <remote-name>))
  "Lower level procedure for removing a remote. This procedure may raise an
exception if remote is unable to be removed."
  (invoke-git self
              "remote"
              "remove"
              "--"
              (remote-name-ref name)))

(define-method (remote-remove! (self <repository>)
                               (name <remote-name>))
  "Remove a remote if it exists."
  (when (remote-url-ref self name)
    (%remote-remove! self name)))

(define-method (remote-remove! (self <repository>)
                               (name <string>))
  (remote-remove! self (remote-name name)))

(define-method (%remote-add! (self <repository>)
                             (remote <remote>))
  "Lower level procedure for adding a remote. If a remote of the same name
already exists, this procedure will raise an exception."
  (invoke-git self
              "remote"
              "add"
              "--"
              (remote-name-ref (remote-name-ref remote))
              (remote-url-ref remote)))

(define-method (%remote-add! (self <repository>)
                             (name <remote-name>)
                             (url <string>))
  (%remote-add! self (remote name url)))

(define-method (remote-set! (self <repository>)
                            (remote <remote>))
  (remote-remove! self (remote-name-ref remote))
  (%remote-add! self remote))

(define-method (remote-set! (self <repository>)
                            (name <remote-name>)
                            (url <string>))
  (remote-set! self (remote name url)))

(define-method (remotes-set! (self <repository>)
                             (remotes <remotes>))
  (for-each
   (lambda (remote)
     (%remote-remove! self (remote-name-ref remote)))
   (remotes-ref (remotes-ref self)))
  (for-each
   (lambda (remote)
     (%remote-add! self remote))
   (remotes-ref remotes)))
