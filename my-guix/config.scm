;; Copyright (c) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is NOT part of GNU Guix.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module defines tweakable settings.

(define-module (my-guix config)
  #:use-module (ice-9 exceptions)
  #:export ($config-dir
            files-ref
            $package-paths
            $xdg-data-home
            $flatpak-remotes))

(define $config-dir
  (canonicalize-path
   (or (getenv "GUIX_CONFIG_DIR")
       (begin
         (raise-exception
          (make-external-error)
          (make-exception-with-message "$GUIX_CONFIG_DIR not set."))))))

(define (files-ref . relpaths)
  (string-join
   (cons* $config-dir
          "files"
          relpaths)
   "/"))

;; Extra package path locations used by these configurations
(define $package-paths
  (let ((modules-dir (string-append $config-dir"/modules")))
    (list modules-dir
          ;; search-patches only does shallow search, so explicitly add patches
          ;; directory here
          (string-append modules-dir"/my-guix/packages/patches"))))

(define $xdg-data-home "$HOME/.local/share")

(define $flatpak-remotes
  '((flathub . "https://flathub.org/repo/flathub.flatpakrepo")))

