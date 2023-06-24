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

(define-module (git-annex-configure git annex matchexpressions)
  #:use-module (oop goops))

;; THIS IS OLD CODE. Not sure what to do with this, yet.

;;; BEGIN match-expression classes

;; TODO improve naming of instance creation methods for ease of use
;; consider writing builder method; maybe pass a list where symbols
;; get substituted with the proper methods? this would allow for using
;; "and", "or" and the like as names without worrying about collision

(define (assert-file-glob glob)
  (when (string-any char-whitespace? glob)
    (format-log $error "Malformed match expression: ~s\n~a"
                glob
                (string-append
                 "File glob cannot contain whitespace; "
                 "use something like [[:blank:]] instead.")))
  glob)

(define-class <match-expression> ())

(define-method (resolve (self <match-expression>))
  (raise-exception
   (make-programming-error)
   (make-exception-with-message
    "~a: ~s")
   (make-exception-with-irritants
    (list "Resolve method not implemented for match-expression"
          (class-of self)))))

(define (assert-match-expression obj)
  (unless (is-a? obj <match-expression>)
    (format-log $error)
    (raise-continuable
     (make-programming-error)
     (make-exception-with-message
      "~a: ~s")
     (make-exception-with-irritants
      (list "Not a <match-expression>"
            obj))))
  obj)

(define-method (display (self <match-expression>)
                        port)
  (format port "~a" (resolve self)))

;;; BEGIN terminal match expressions

(define-method (write (self <match-expression>)
                      port)
  (format port "#<<~a> ~s>"
          (class-name (class-of self))
          (resolve self)))

(define-class <match-anything> (<match-expression>))

(define-method (resolve (self <match-anything>))
  "anything")

(define (m/anything)
  (make <match-anything>))

(define-class <match-nothing> (<match-expression>))

(define-method (resolve (self <match-nothing>))
  "nothing")

(define (m/nothing)
  (make <match-nothing>))

;;; END terminal match expressions

;;; BEGIN unary match expressions

(define-class <match-unary> (<match-expression>)
  (self #:init-keyword #:self
        #:getter expr-ref))

(define-method (initialize (self <match-unary>)
                           initargs)
  (next-method)
  (assert-match-expression (expr-ref self)))

(define-method (write (self <match-unary>)
                      port)
  (format port "#<<~a> ~s>"
          (class-name (class-of self))
          (expr-ref self)))

(define-class <match-include> (<match-unary>))

(define-method (initialize (self <match-include>)
                           initargs)
  (next-method)
  (assert-file-glob (expr-ref self)))

(define-method (resolve (self <match-include>))
  (format #f "include=~a"
          (expr-ref self)))

(define (m/include expr)
  (make <match-include> #:expr expr))

(define-class <match-exclude> (<match-unary>))

(define-method (initialize (self <match-exclude>)
                           initargs)
  (next-method)
  (assert-file-glob (expr-ref self)))

(define-method (resolve (self <match-exclude>))
  (format #f "exclude=~a"
          (expr-ref self)))

(define (m/exclude expr)
  (make <match-exclude> #:expr expr))

(define-class <match-not> (<match-unary>))

(define-method (resolve (self <match-not>))
  (format #f "(not ~a)"
          (expr-ref self)))

(define (m/not expr)
  (make <match-not> #:expr expr))

;;; END unary match expressions

;;; BEGIN binary match expressions

(define-class <match-binary> (<match-expression>)
  (expr1 #:init-keyword #:expr1
         #:getter expr1-ref)
  (expr2 #:init-keyword #:expr2
         #:getter expr2-ref))

(define-method (initialize (self <match-binary>)
                           initargs)
  (next-method)
  (assert-match-expression (expr1-ref self))
  (assert-match-expression (expr2-ref self)))

(define-method (write (self <match-binary>)
                      port)
  (format port "#<<~a> ~s ~s>"
          (class-name (class-of self))
          (expr1-ref self)
          (expr2-ref self)))

(define-class <match-and> (<match-binary>))

(define-method (resolve (self <match-and>))
  (format #f "(~a and ~a)"
          (resolve (expr1-ref self))
          (resolve (expr2-ref self))))

(define (m/and expr1 expr2)
  (make <match-and> #:expr1 expr1 #:expr2 expr2))

(define-class <match-or> (<match-binary>))

(define-method (resolve (self <match-or>))
  (format #f "(~a or ~a)"
          (resolve (expr1-ref self))
          (resolve (expr2-ref self))))

(define (m/or expr1 expr2)
  (make <match-or> #:expr1 expr1 #:expr2 expr2))

;;; END binary match expressions

;;; END match-expression classes
