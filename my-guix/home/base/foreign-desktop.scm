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
;;; This module provides a base desktop home environment for foreign
;;; distributions.

(define-module (my-guix home base foreign-desktop)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (my-guix extensions)
  #:use-module (my-guix home base desktop)
  #:use-module (my-guix home extensions foreign)
  #:use-module (my-guix home services package-management)
  #:export (base-foreign-desktop-home-environment))

(define base-foreign-desktop-home-environment
  (apply-extensions
   (let ((env base-desktop-home-environment))
     (home-environment
      (inherit env)))
   (list foreign-extension)))
