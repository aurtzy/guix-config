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

(define-module (git-annex-configure utils)
  #:use-module (git-annex-configure logging)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:export (with-chdir
            invoke
            capture-output
            capture-output*
            with-error-to-void))

(define (with-chdir dir thunk)
  (let ((chdir-to-dir (lambda ()
                          (format-log $debug "chdir: ~s" dir)
                          (chdir dir)))
        (chdir-back (let ((orig-dir (canonicalize-path ".")))
                      (lambda ()
                        (format-log $debug "chdir back: ~s" orig-dir)
                        (chdir orig-dir)))))
    (let ((result
           (guard (exn
                   ((non-continuable-error? exn)
                    (chdir-back)
                    (raise-exception exn)))
             (with-exception-handler
              (lambda (exn)
                (chdir-back)
                (raise-continuable exn)
                (chdir-to-dir))
              (lambda ()
                (chdir-to-dir)
                (thunk))))))
      (chdir-back)
      result)))

(define* (invoke prog . args)
  "Wrapper for system* with some extra features."
  (format-log $debug "Running command: ~s" (cons prog args))
  (let ((exit-code (status:exit-val
                    (apply system* prog args))))
    (cond
     ((zero? exit-code)
      #t)
     (else
      (raise-exception
       (make-exception
        (make-external-error)
        (make-exception-with-message
         "~a ~a: ~s")
        (make-exception-with-irritants
         (list "Command failed with exit code"
               exit-code
               (cons prog args)))))))))

(define* (capture-output thunk)
  "Captures string sent to output from calling thunk."
  (let* ((capture-pipe (pipe))
         (capture-input-port (car capture-pipe))
         (capture-output-port (cdr capture-pipe)))
    (with-exception-handler
     (lambda (exn)
       (close-port capture-input-port)
       (close-port capture-output-port)
       (raise-exception exn))
     (lambda ()
       (setvbuf capture-output-port 'none)
       (with-output-to-port capture-output-port
         thunk)
       (close-port capture-output-port)
       (let ((result (get-string-all capture-input-port)))
         (close-port capture-input-port)
         result)))))

(define (capture-output* . args)
  "Like capture-output, except that the last character of the capture is
stripped before being returned. This is useful for commands that
produce trailing newlines as a side effect of printing mechanisms,
where the newline is not actually part of the value.

If the capture is an empty string, an empty string is returned."
  (let ((capture (apply capture-output args)))
    (if (or (not capture)
            (string-null? capture))
        capture
        (string-drop-right capture 1))))

(define (with-error-to-void thunk)
  (let* ((the-void (%make-void-port "w"))
         (result (with-error-to-port the-void thunk)))
    (close-port the-void)
    result))
