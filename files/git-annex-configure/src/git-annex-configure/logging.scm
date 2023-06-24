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

(define-module (git-annex-configure logging)
  #:use-module (ice-9 format)
  #:export ($prog-name
            $debug
            $info
            $warning
            $error
            $suppress
            log-level-ref
            log-level-set!
            format-log))

(define $prog-name "git-annex-configure")

(define $debug '(0 . "debug"))
(define $info '(10 . #f))
(define $warning '(20 . "warning"))
(define $error '(30 . "error"))
(define $suppress '(100 . #f))

(define-once *log-level* $suppress)

(define (log-level-ref)
  *log-level*)

(define (log-level-set! level)
  (set! *log-level* level))

(define format-log
  ;; Prevent logging from being suppressed by setting static error
  ;; port at program start
  (let ((error-port (current-error-port)))
    (lambda* (log-level fmt #:rest args)
      (when (>= (car log-level)
                (car *log-level*))
        (let ((msg (apply format
                          #f
                          fmt
                          args))
              (prefix-log-level (cdr log-level)))
          (if prefix-log-level
              (format error-port
                      "~a: ~a: ~a\n"
                      $prog-name
                      prefix-log-level
                      msg)
              (format error-port
                      "~a: ~a\n"
                      $prog-name
                      msg)))))))
