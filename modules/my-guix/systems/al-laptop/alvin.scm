;;; Copyright © 2025 aurtzy <aurtzy@gmail.com>
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
;;; This module defines configurations for the "alvin" user on al-laptop.

(define-module (my-guix systems al-laptop alvin)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module ((my-guix systems)
                #:select ((base-desktop-home-environment . base-he)))
  #:export (alvin-home-environment))

(define alvin-home-environment
  (home-environment
    (inherit base-he)
    ))
