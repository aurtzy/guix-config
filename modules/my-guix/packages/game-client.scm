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
  #:use-module (ice-9 match))

(define (replace-inputs inputs replacements)
  (let ((apply-replacements (apply compose replacements)))
    (map (lambda (input)
           (match input
             ((name pkg)
              (list name (apply-replacements pkg)))
             ((name pkg output)
              (list name (apply-replacements pkg) output))))
         inputs)))

;; TODO Custom Steam container with mesa-git replacing mesa.  Rust doesn't
;; work on i686 architecture currently, so this package will not compile.
;;
;; Potentially related issue: https://issues.guix.gnu.org/35519
(define-public steam-container-custom
  ;; Just using `package-input-rewriting' does not seem to work due to issues
  ;; with inputs not actually being replaced, so we replace them at the
  ;; source.
  ;;
  ;; See: https://gitlab.com/nonguix/nonguix/-/issues/197
  (let* ((replace-mesa (package-input-rewriting `((,mesa . ,mesa-git))))
         (steam-client-libs (replace-inputs
                             (@@ (nongnu packages game-client)
                                 steam-client-libs)
                             (list replace-mesa)))
         (steam-gameruntime-libs (replace-inputs
                                  (@@ (nongnu packages game-client)
                                      steam-gameruntime-libs)
                                  (list replace-mesa))))
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
