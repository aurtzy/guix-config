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
;; This module defines extensions that add channels.

(define-module (my-guix extensions channels)
  #:use-module (gnu)
  #:use-module (my-guix utils)
  #:use-module (my-guix extensions))

(define-public nonguix-channel-extension
  (extension
    (name 'nonguix-channel-extension)
    (configuration
     (extender operating-system
         os =>
       (services
        (modify-services (operating-system-user-services os)
          (guix-service-type
           config => (guix-configuration
                      (inherit config)
                      (substitute-urls
                       (cons "https://substitutes.nonguix.org"
                             (guix-configuration-substitute-urls
                              config)))
                      (authorized-keys
                       ;; TODO this file is not currently present
                       (cons (local-file (search-files-path
                                          "guix/nonguix.pub"))
                             (guix-configuration-authorized-keys
                              config)))))))))))
