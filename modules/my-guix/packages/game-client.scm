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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gl)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
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

(define steam-custom
  (let ((steam-pkg (nonguix-container->package steam-container-custom)))
    (package
      (inherit steam-pkg)
      (version (string-append (package-version steam-pkg) "-custom")))))

;;; TODO This probably belongs in mods.
(define-public steam-custom-wrapped
  (package
    (name "steam-custom")
    (version (package-version steam-custom))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let* ((out (assoc-ref %outputs "out"))
                   (out-steam (string-append out "/bin/steam"))
                   (steam (assoc-ref %build-inputs "steam")))
              (mkdir-p (string-append out "/bin"))
              (symlink (string-append steam "/bin/steam")
                       out-steam)
              (mkdir-p (string-append out "/share"))
              (symlink (string-append steam "/share/applications")
                       (string-append out "/share/applications"))
              (wrap-program out-steam
                #:sh #$(file-append bash-minimal "/bin/bash")
                '("QT_X11_NO_MITSHM" = ("1"))
                '("GUIX_SANDBOX_EXTRA_SHARES"
                  = ("$HOME/storage/steam-alt-library"
                     "$HOME/areas/games"))))))))
    (inputs
     (list steam-custom))
    (home-page (package-home-page steam-custom))
    (synopsis (package-synopsis steam-custom))
    (description (package-description steam-custom))
    (license (package-license steam-custom))))
