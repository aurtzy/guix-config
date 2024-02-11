;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This module defines tweakable settings.

(define-module (my-guix config)
  #:use-module (gnu system file-systems)
  #:use-module (guix utils)
  #:use-module (ice-9 exceptions)
  #:export (GUIX_CONFIG_DIR
            GUIX_CONFIG_MODULES_DIR
            BASE_FILE_SYSTEM_FLAGS
            base-file-system-flags-ref
            BASE_FILE_SYSTEM_OPTIONS
            base-file-system-options-ref
            $xdg-data-home))

;; Base directory for Guix configurations
(define GUIX_CONFIG_DIR
  ;; Value is based off of module directory location (my-guix config) =>
  ;; .../guix-config/modules/my-guix/../..
  (dirname (dirname (current-source-directory))))

(define GUIX_CONFIG_MODULES_DIR
  (string-append GUIX_CONFIG_DIR "/modules"))

(define (base-file-system-config-ref alist file-system-type device-type)
  (let ((device-alist (assq-ref alist file-system-type)))
    (or (assq-ref device-alist device-type)
        (raise-exception
         (make-exception
          (make-programming-error)
          (make-exception-with-message "~a: ~s")
          (make-exception-with-irritants
           (list "base file-system device-config entry not available"
                 (list file-system-type device-type))))))))

(define BASE_FILE_SYSTEM_FLAGS
  '((btrfs . ((ssd . (no-atime))
              (hdd . (no-atime))))))

(define (base-file-system-flags-ref file-system-type device-type)
  (base-file-system-config-ref BASE_FILE_SYSTEM_FLAGS
                               file-system-type
                               device-type))

(define BASE_FILE_SYSTEM_OPTIONS
  '((btrfs
     ;; Useful statistics for determining compression:
     ;; - https://docs.google.com/spreadsheets/d/1x9-3OQF4ev1fOCrYuYWt1QmxYRmPilw_nLik5H_2_qA/edit?usp=sharing
     ;; - https://linuxreviews.org/Comparison_of_Compression_Algorithms
     . ((ssd . (("compress-force" . "zstd:2")))
        (hdd . (("compress-force" . "zstd:6")))))))

(define (base-file-system-options-ref file-system-type device-type)
  "This procedure retrieves the base file-system options for some type of
storage device. The format of keys expected to be symbols in the following
order: FILE-SYSTEM-TYPE => DEVICE-TYPE"
  (base-file-system-config-ref BASE_FILE_SYSTEM_OPTIONS
                               file-system-type
                               device-type))

(define $xdg-data-home (string-append
                        (getenv "HOME")
                        "/.local/share"))
