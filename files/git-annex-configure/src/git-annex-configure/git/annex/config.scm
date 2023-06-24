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

(define-module (git-annex-configure git annex config)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure utils)
  #:use-module (oop goops)
  #:export (annex-config-ref
            annex-config-set!
            annex-config-remove!))

(define-method (annex-config-ref (self <annex-repository>)
                                 (name <string>))
  (capture-output*
   (lambda ()
     (invoke-git-annex self
                       "config"
                       "--get"
                       name))))

(define-method (annex-config-set! (self <annex-repository>)
                                  (name <string>)
                                  (value <string>))
  (invoke-git-annex self
                    "config"
                    "--set"
                    name
                    value))

(define-method (annex-config-remove! (self <annex-repository>)
                                     (name <string>))
  (invoke-git-annex self
                    "config"
                    "--unset"
                    name))
