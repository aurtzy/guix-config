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

(define-module (git-annex-configure spec)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure git annex remote)
  #:use-module (git-annex-configure records)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 exceptions)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<repository-configuration>
            repository-configuration repository-configuration?
            configuration-uuid
            configuration-disabled?
            configuration-description
            configuration-wanted
            configuration-required
            configuration-groups
            configuration-remotes
            configuration-config
            configuration-hooks
            
            <configuration>
            configuration configuration?
            configuration-annex-config
            configuration-groupwanted
            configuration-repositories))

;; Sanitization procedures provide assertions with formatted error messages.
;; Return values of procedures should be the sanitized value, which may or may
;; not be the same. Procedures are expected to have the #:accept-false? key for
;; cases where #f is not acceptable.

(define* ((sanitize-self field
                         #:key
                         (accept-false? #t))
          raw)
  "Accepts any value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   (raw
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a")
      (make-exception-with-irritants
       (list field
             "field value cannot be false")))))))

(define* ((sanitize-string field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts only string value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((string? raw)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a string"
             raw)))))))

(define* (((sanitize-list sanitize-value)
           field
           #:key (accept-false? #t))
          raw)
  "Accepts only a list where each element is sanitized with sanitize-value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((list? raw)
    (map
     (lambda (value)
       ((sanitize-value field #:accept-false? #f) value))
     raw))
   (else 
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a field value not a list: ~s")
      (make-exception-with-irritants
       (list field
             raw)))))))

(define* (((sanitize-alist sanitize-key sanitize-value)
           field
           #:key
           (accept-false? #t))
          raw)
  "Accepts only an alist where each pair has its key and value sanitized with
sanitize-key and sanitize-value, respectively."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((and (list? raw)
         (every pair?
                raw))
    (map
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (cons ((sanitize-key field #:accept-false? #f)
                key)
               ((sanitize-value field #:accept-false? #f)
                value))))
     raw))
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a field value not an alist: ~s")
      (make-exception-with-irritants
       (list field
             raw)))))))

(define* ((sanitize-group field
                          #:key
                          (accept-false? #t))
          raw)
  "Accepts <group> value; otherwise attempt to coerce value into a <group>."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <group>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a field value not a valid group: ~s")
         (make-exception-with-irritants
          (list field
                raw))
         exn)))
     (lambda ()
       (group raw))))))

(define* ((sanitize-groups field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts <groups> value; otherwise assumes value is a list of <group> objects
(or objects that can be coerced to <group>) and attempts to apply it to the
`groups' constructor."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <groups>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a ~a: ~s")
         (make-exception-with-irritants
          (list field
                "field value not a valid groups type"
                raw))
         exn)))
     (lambda ()
       (apply groups raw))))))

(define* ((sanitize-remote field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts <remote> value; otherwise raises an assertion exception."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <remote>)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a valid remote type"
             raw)))))))

(define* ((sanitize-remotes field
                            #:key
                            (accept-false? #t))
          raw)
  "Accepts <remotes> value; otherwise assumes raw value is a list of <remote>
objects and tries to apply it to the `remotes' constructor."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <remotes>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a ~a: ~s")
         (make-exception-with-irritants
          (list field
                "field value not a valid remotes type"
                raw))
         exn)))
     (lambda ()
       (apply remotes raw))))))

(define-record-type* <repository-configuration>
  repository-configuration make-repository-configuration
  repository-configuration?
  this-repository-configuration
  (uuid configuration-uuid
        (sanitize (sanitize-string "repository uuid")))
  (disabled? configuration-disabled?
             (default #f))

  ;; These configurations can be applied from any repository.
  (description configuration-description
               (default #f)
               (sanitize (sanitize-string "description")))
  (wanted configuration-wanted
          (default #f)
          (sanitize (sanitize-string "wanted matchexpression")))
  (required configuration-required
            (default #f)
            (sanitize (sanitize-string "required matchexpression")))
  (groups configuration-groups
          (default #f)
          (sanitize (sanitize-groups "groups")))

  ;; These configurations can only be applied locally - there appears to be no
  ;; method of syncing state for these settings.
  (remotes configuration-remotes
           (default #f)
           (sanitize (sanitize-remotes "remotes")))
  (config configuration-config
          (default #f)
          (sanitize ((sanitize-alist sanitize-string
                                     sanitize-string)
                     "git config")))
  (hooks configuration-hooks
         (default #f)
         (sanitize ((sanitize-alist sanitize-string
                                    sanitize-self)
                    "hooks"))))

(define* ((sanitize-repository-configuration field
                                             #:key
                                             (accept-false? #t))
          raw)
  "Accept only a <repository-configuration> value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((repository-configuration? raw)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a valid repository configuration"
             raw)))))))

(define-record-type* <configuration>
  configuration make-configuration
  configuration?
  this-configuration
  (annex-config configuration-annex-config
                (default #f)
                (sanitize ((sanitize-alist sanitize-string
                                           sanitize-string)
                           "git-annex config")))
  (groupwanted configuration-groupwanted
               (default #f)
               (sanitize ((sanitize-alist sanitize-group
                                          sanitize-string)
                          "groupwanted matchexpressions")))
  (repositories configuration-repositories
                (default '())
                (sanitize ((sanitize-list sanitize-repository-configuration)
                           "repository configurations"))))
