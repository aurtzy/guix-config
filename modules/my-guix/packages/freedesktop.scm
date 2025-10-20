;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (my-guix packages freedesktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libblockdev/fixed
  (package/inherit libblockdev
    (name "libblockdev")
    (arguments
     (substitute-keyword-arguments (package-arguments libblockdev)
       ((#:phases original-phases)
        #~(modify-phases #$original-phases
            (replace 'patch-plugin-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (define (search-program program)
                  (let ((result (or (false-if-exception
                                     (search-input-file inputs
                                                        (string-append "bin/" program)))
                                    (false-if-exception
                                     (search-input-file inputs
                                                        (string-append "sbin/" program)))
                                    (begin
                                      (format (current-warning-port)
                                              "warning: program ~s left unpatched~%"
                                              program)
                                      program))))
                    result))
                (substitute* (find-files "src/plugins" "\\.c$")
                  (("(\\{\")([^\"]+)(\",.*\\})" all start program end)
                   (string-append start (search-program program) end))
                  (("(\\..*_util = \")([^\"]+)" all start program)
                   (string-append start (search-program program))))))))))))

(define-public udisks/fixed
  (package/inherit udisks
    (name "udisks")
    (inputs (modify-inputs (package-inputs udisks)
              (replace "libblockdev" libblockdev/fixed)))))
