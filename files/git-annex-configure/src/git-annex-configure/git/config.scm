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

(define-module (git-annex-configure git config)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (config-ref
            config-set!))

;;; BEGIN Classes

;;; BEGIN legacy code

;; TODO a config can have more than one type. also, sometimes can only
;; accept specific values of types. in the latter case, it might be of
;; interest to implement logic that checks if values are instances in
;; order to handle validities of specific values.
;; Git setting key. Can also be used for initializing config.

;; "git config" could initially support setting string values, and
;; then expand from there with methods that can take different types
;; and call the git-config method with converted string. This
;; approach, however, doesn't really work with encapsulation when one
;; can have access to the string method.

(define-class <config-key> ()
  ;; Alist of acceptable keys and their associated types. Config
  ;; values must have at least one type, but may accept more than one.
  (settings #:allocation #:each-subclass
            #:init-value `()
            #:getter config-settings-ref)
  ;; Symbol specifying key for git setting.
  (key #:init-keyword #:key
       #:getter config-key-ref))

(define-method (initialize (self <config-key>)
                           initargs)
  (next-method)
  (let ((key (config-key-ref self)))
    (unless (assq-ref (config-settings-ref self) key)
      (raise-exception
       (make-exception
        (make-external-error)
        (make-exception-with-message
         "~a: ~s")
        (make-exception-with-irritants
         (list "Config key does not match any valid setting"
               key)))))))

(define-method (make-config-key (key <symbol>))
  (make <config-key> #:key key))

(define-method (display (self <config-key>)
                        port)
  (format port "~a"
          (config-key-ref self)))

(define-method (write (self <config-key>)
                      port)
  (format port "<~a ~s>"
          (class-name (class-of self))
          (config-key-ref self)))

(define-class <config> (<config-key>)
  ;; A value of any type that conforms to a type specified in
  ;; config-settings-ref.
  (value #:init-keyword #:value
         #:getter config-value-ref))

(define-method (initialize (self <config>)
                           initargs)
  (next-method)
  (let* ((key (config-key-ref self))
         (value (config-value-ref self))
         (types (assq-ref (config-settings-ref self) key)))
    (unless (any
             (lambda (type)
               (is-a? value type))
             types)
      (raise-exception
       (make-exception
        (make-external-error)
        (make-exception-with-message
         "~a: {~s} not any of ~s")
        (make-exception-with-message
         (list "Config value does not match any setting type"
               value
               types)))))))

(define-method (make-config (key <symbol>)
                            value)
  (make <config>
    #:key key
    #:value value))

(define-method (make-config (key <config-key>)
                            value)
  (make <config>
    #:key (config-key-ref key)
    #:value value))

(define-method (display (self <config>)
                        port)
  (format port "(~a . ~a)"
          (config-key-ref self)
          (config-value-ref self)))

(define-method (write (self <config>)
                      port)
  (format port "<~a (~s . ~s)>"
          (class-name (class-of self))
          (config-key-ref self)
          (config-value-ref self)))

;;; END legacy code

;;; END Classes

(define-method (config-ref (self <repository>)
                           (key <string>))
  "Gets a repository config value. If the item does not exist, this method
returns #f."
  (false-if-exception
   (capture-output*
    (lambda ()
      (with-error-to-void
       (lambda ()
         (invoke-git self
                     "config"
                     "--"
                     key)))))))

(define-method (config-set! (self <repository>)
                            (key <string>)
                            (value <string>))
  (invoke-git self
              "config"
              "--"
              key
              value))
