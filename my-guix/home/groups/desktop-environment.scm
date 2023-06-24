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
;; This module defines desktop-environment-specific groups.

(define-module (my-guix home groups desktop-environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages music)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (my-guix home groups)
  #:use-module (my-guix home services))

(define-group plasma
  (simple-service home-activation-service-type
                  #~(begin
                      ;; Use Overview as default action for Meta
                      ;; See: https://zren.github.io/kde/#windowsmeta-key
                      (invoke "kwriteconfig5"
                              "--file" (string-append (getenv "HOME") "/.config/kwinrc")
                              "--group" "ModifierOnlyShortcuts"
                              "--key" "Meta"
                              "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview")
                      (invoke "qdbus" "org.kde.KWin" "/KWin" "reconfigure"))))
