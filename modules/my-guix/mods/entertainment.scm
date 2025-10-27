;;; Copyright © 2023-2025 Alvin Hsu <aurtzy@gmail.com>
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
;;; This module provides entertainment-related mods.

(define-module (my-guix mods entertainment)
  #:use-module (gnu)
  #:use-module (gnu packages gl)
  #:use-module (gnu services)
  #:use-module (gnu system privilege)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix mods)
  #:use-module (my-guix mods base)
  #:use-module (my-guix packages game-client)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-26)
  #:export (meta-entertainment-mod))

(use-service-modules sysctl)

;; TODO: Factorize this.
(define meta-entertainment-mod
  (operating-system-mod
    (name 'meta-entertainment)
    (services
     (let-mod-arguments (this-operating-system-mod-arguments)
         ((replace-mesa replace-mesa-argument))
       (list (simple-service name
                             profile-service-type
                             (list (replace-mesa gamescope)))
             (simple-service name
                             privileged-program-service-type
                             (list
                              (privileged-program
                                (program
                                 (file-append
                                  (replace-mesa gamescope) "/bin/gamescope"))
                                (capabilities "cap_sys_nice=eip"))))
             ;; HACK: Using privileges causes gamescope to not inherit
             ;; environment, so it fails an attempt to search for needed
             ;; vulkan files.  Conveniently provide them at this location,
             ;; which gamescope searches by default.
             (simple-service name
                             etc-service-type
                             `(("vulkan" ,(file-append (replace-mesa mesa)
                                                       "/share/vulkan"))))
             (simple-service name
                             sysctl-service-type
                             ;; Copied value from Arch Linux (and context
                             ;; given in link):
                             ;; https://archlinux.org/news/increasing-the-default-vmmax_map_count-value/
                             '(("vm.max_map_count" . "1048576"))))))))
