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

(define-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:export (<group>
            group-ref
            group

            <groups>
            groups-ref
            groups
            groups->string
            string->groups

            group-ref
            group-add!
            group-remove!
            groups-ref
            groups-add!
            groups-remove!
            groups-set!))

;;; BEGIN Classes

(define-class <group> ()
  (group #:init-keyword #:group
         #:getter group-ref))

(define-method (group name)
  (make <group> #:group name))

;; This current implementation might disallow certain
;; valid characters or even allow ceqrtain invalid
;; ones. We make do here, expecting new discoveries to
;; change the following logic.
(define (sanitize-group group)
  (cond
   ((is-a? group <group>)
    (group-ref group))
   ((and (string? group)
         (string-null? group))
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "Group name cannot be empty"))))
   ((string-any (lambda (char)
                  (and (char-whitespace? char)
                       (not (eqv? #\tab char))))
                group)
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a: ~s")
      (make-exception-with-irritants
       (list "Group name contains whitespace"
             group)))))
   (else
    group)))

(define-method (initialize (self <group>)
                           initargs)
  (next-method)
  (let ((group (group-ref self)))
    (slot-set! self 'group (sanitize-group group))))

(define-method (display (self <group>)
                        port)
  (write self port))

(define-method (write (self <group>)
                      port)
  (display (format #f
                   "#<~a ~s>"
                   (class-name (class-of self))
                   (group-ref self))
           port))

(define-class <groups> ()
  (groups #:init-keyword #:groups
          #:getter groups-ref))

(define (groups . groups)
  (make <groups> #:groups groups))

(define-method (initialize (self <groups>)
                           initargs)
  (next-method)
  (let ((groups
         (groups-ref self)))
    (unless (list? groups)
      (raise-exception
       (make-exception
        (make-assertion-failure)
        (make-exception-with-message
         "~a: ~s")
        (make-exception-with-irritants
         (list "Not a list of groups"
               groups)))))
    (slot-set! self 'groups
               (map
                (lambda (raw)
                  (if (is-a? raw <group>)
                      raw
                      (group raw)))
                groups))))

(define-method (groups->string (self <groups>))
  (string-join
   (map
    (lambda (group)
      (group-ref group))
    (groups-ref self))
   " "))

(define-method (string->groups (self <string>))
  (apply groups
         (delete ""
                 (string-split self #\space))))

(define-method (display (self <groups>)
                        port)
  (format port "~a" (groups-ref self)))

(define-method (write (self <groups>)
                      port)
  (format port "#<~a ~s>"
          (class-name (class-of self))
          (groups-ref self)))

;;; END Classes

(define-method (group-add! (self <annex-repository>)
                           (group <group>)
                           . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "group"
                      "--"
                      remote
                      (group-ref group))))

(define-method (group-remove! (self <annex-repository>)
                              (group <group>)
                              . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "ungroup"
                      "--"
                      remote
                      (group-ref group))))

(define-method (groups-ref (self <annex-repository>)
                           . options)
  (let-keywords
      options #f ((remote "."))
    (string->groups (capture-output*
                     (lambda ()
                       (invoke-git-annex self
                                         "group"
                                         remote))))))

(define-method (groups-add! (self <annex-repository>)
                            (groups <groups>)
                            . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "group"
                      "--"
                      remote
                      (groups->string groups))))

(define-method (groups-remove! (self <annex-repository>)
                               (groups <groups>)
                               . options)
  (let-keywords
      options #f ((remote "."))
    ;; git-annex ungroup does not interpret space-delimited string as separate
    ;; groups to remove, so we ungroup each one at a time.
    (for-each
     (lambda (group)
       (invoke-git-annex self
                         "ungroup"
                         "--"
                         remote
                         (group-ref group)))
     (groups-ref groups))))

(define-method (groups-set! (self <annex-repository>)
                            (groups <groups>)
                            . options)
  (let-keywords
      options #f ((remote "."))
    (apply groups-remove!
           self
           (apply groups-ref self options)
           options)
    (apply groups-add!
           self
           groups
           options)))
