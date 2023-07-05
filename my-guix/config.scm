;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module defines tweakable settings.

(define-module (my-guix config)
  #:use-module (gnu system file-systems)
  #:use-module (guix utils)
  #:use-module (ice-9 exceptions)
  #:export ($modules-dir
            $default-file-system-options
            default-file-system-options-ref
            $xdg-data-home
            $flatpak-remotes))

(define $modules-dir
  ;; Assume base modules directory is previous directory following logic of
  ;; module name depth
  (or (and=> (current-source-directory)
             dirname)
      ;; Fallback in case return is #f - likely in a repl, so current
      ;; directory works
      (getcwd)))

(define $default-file-system-options
  '((btrfs . ((ssd . '(("compress-force" . "zstd:2")
                       "noatime"))
              (hdd . '(("compress-force" . "zstd:6")
                       "noatime"))))))

(define (default-file-system-options-ref file-system-type device-type)
  "This procedure retrieves the default file-system options for some type of
storage device. The format of keys expected to be symbols in the following
order: FILE-SYSTEM-TYPE => DEVICE-TYPE"
  (let ((options (assq-ref $default-file-system-options
                           file-system-type)))
    (or (assq-ref options device-type)
        (raise-exception
         (make-exception
          (make-programming-error)
          (make-exception-with-message "~a: ~s")
          (make-exception-with-irritants
           (list "default file-system options entry not available"
                 (list file-system-type device-type))))))))

(define $xdg-data-home (string-append
                        (getenv "HOME")
                        "/.local/share"))

(define $flatpak-remotes
  '((flathub . "https://dl.flathub.org/repo/flathub.flatpakrepo")))

