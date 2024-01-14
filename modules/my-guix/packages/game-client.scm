;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Kozo <kozodev@runbox.com>
;;; Copyright © 2021, 2022, 2023, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Elijah Malaby
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
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

(define-module (my-guix packages game-client)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (my-guix packages mesa)
  #:use-module (nonguix multiarch-container)
  #:use-module (nongnu packages game-client)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define (replace-mesa inputs)
  ;; Because this is a hacky hack, do a sanity check to make sure mesa is
  ;; actually matched, then remove it
  (let ((matched (and=> (member "mesa" inputs (lambda (x input)
                                                (equal? x (car input))))
                        car)))
    (unless matched
      (display "SANITY CHECK FAILED: MESA NOT FOUND\n")
      (raise-exception (make-exception)))
    (cons `("mesa" ,mesa-git) (delq matched inputs))))

(define-public steam-container-custom
  (let ((steam-client-libs (replace-mesa
                            (@@ (nongnu packages game-client)
                                steam-client-libs)))
        (steam-gameruntime-libs (@@ (nongnu packages game-client)
                                    steam-gameruntime-libs)))
    (nonguix-container
     (inherit steam-container)
     (name "steam-custom")
     (union64
      (fhs-union `(,@steam-client-libs
                   ,@steam-gameruntime-libs
                   ,@fhs-min-libs)
                 #:name "fhs-union-64"))
     (union32
      (fhs-union `(,@steam-client-libs
                   ,@steam-gameruntime-libs
                   ,@fhs-min-libs)
                 #:name "fhs-union-32"
                 #:system "i686-linux")))))

(define-public steam-custom
  (package
    (inherit (nonguix-container->package steam-container-custom))
    (supported-systems (list "x86_64-linux"))))
