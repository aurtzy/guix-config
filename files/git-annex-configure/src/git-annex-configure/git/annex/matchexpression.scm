;; Copyright (C) 2023 aurtzy <aurtzy@gmail.com>
;;
;; This file is part of git-annex-configure.
;;
;; git-annex-configure is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; git-annex-configure is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; git-annex-configure. If not, see <https://www.gnu.org/licenses/>.

(define-module (git-annex-configure git annex matchexpression)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:export (required-ref
            required-set!
            wanted-ref!
            wanted-set!
            groupwanted-ref
            groupwanted-set!))

(define-method (required-ref (self <annex-repository>)
                             . options)
  (let-keywords
      options #f ((remote "."))
    (capture-output*
     (lambda ()
       (invoke-git-annex self
                         "required"
                         remote)))))

(define-method (required-set! (self <annex-repository>)
                              (expr <string>)
                              . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "required"
                      "--"
                      remote
                      expr)))

(define-method (wanted-ref (self <annex-repository>)
                           . options)
  (let-keywords
      options #f ((remote "."))
    (capture-output*
     (lambda ()
       (invoke-git-annex self
                         "wanted"
                         remote)))))

(define-method (wanted-set! (self <annex-repository>)
                            (expr <string>)
                            . options)
  (let-keywords
      options #f ((remote "."))
    (invoke-git-annex self
                      "wanted"
                      "--"
                      remote
                      expr)))

(define-method (groupwanted-ref (self <annex-repository>)
                                (group <group>))
  (capture-output*
   (lambda ()
     (invoke-git-annex self
                       "groupwanted"
                       "--"
                       (group-ref group)))))

(define-method (groupwanted-set! (self <annex-repository>)
                                 (group <group>)
                                 (expr <string>))
  (invoke-git-annex self
                    "groupwanted"
                    "--"
                    (group-ref group)
                    expr))
