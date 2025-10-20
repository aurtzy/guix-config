;;; Copyright © 2023-2025 aurtzy <aurtzy@gmail.com>
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
;;; This module defines configurations for the "alvin" user on al-pc.

(define-module (my-guix systems al-pc alvin)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages linux)
  #:use-module (gnu services linux)
  #:use-module (gnu services sddm)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system privilege)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (my-guix config)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix mods data)
  #:use-module (my-guix mods desktop)
  #:use-module (my-guix mods desktop-environment)
  #:use-module (my-guix mods desktop-extra)
  #:use-module (my-guix mods entertainment)
  #:use-module (my-guix mods hardware)
  #:use-module (my-guix mods server)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix packages mesa)
  #:use-module (my-guix packages keyboard-center)
  #:use-module (my-guix packages redlib)
  #:use-module (my-guix services hardware)
  #:use-module ((my-guix systems)
                #:select ((base-desktop-home-environment . base-he)))
  #:use-module (my-guix systems al-pc)
  #:use-module (my-guix utils)
  #:use-module (nongnu packages linux)
  #:use-module ((nongnu packages nvidia) #:prefix nvidia:)
  #:use-module ((nongnu services nvidia) #:prefix nvidia:)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nonguix utils)
  #:export (alvin-home-environment))

(define alvin-home-environment
  (home-environment
    (inherit base-he)
    (services
     (cons* (simple-service 'redlib
                            home-shepherd-service-type
                            (list
                             (shepherd-service
                               (documentation
                                "Run Redlib service.")
                               (provision
                                '(redlib))
                               (requirement
                                '())
                               (start
                                #~(make-forkexec-constructor
                                   (list #$(file-append redlib "/bin/redlib")
                                         "--port" "8081")))
                               (stop
                                #~(make-kill-destructor)))))
            (home-environment-user-services base-he)))))
