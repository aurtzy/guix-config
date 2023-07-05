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
;; This module provides miscellaneous extensions.

(define-module (my-guix home extensions misc)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (my-guix extensions))

(use-package-modules tex)

(define-public tex-extension
  (extension
    (name "tex")
    (configuration
     (extender home-environment
         env =>
       (packages
        (cons* texlive-bin  ;sets GUIX_TEXMF search path, which is important
               texlive-amsfonts
               texlive-base
               texlive-capt-of
               texlive-fonts-ec
               texlive-hyperref
               texlive-ulem
               texlive-wrapfig
               (home-environment-packages env)))))))
