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

;;; Commentary:
;;;
;;; This module includes mods relating to my "data infrastructure".

(define-module (my-guix mods data)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (ice-9 match)
  #:use-module (my-guix home services)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (annexed-data
            data-mod))

(use-package-modules backup haskell-apps)

;; annexed-data: An alist of data repositories and items from respective
;; stores to be symlinked from $HOME.
;;
;; Each element should be the path to an annex repository (relative to $HOME),
;; followed by the list of store items to symlink from $HOME.  For example,
;; the following specifies two repositories at ~/data-repo and ~/data-repo-2,
;; with ~/data-repo/store/item and ~/data-repo-2/store/item{2,2.5} symlinked:
;;
;; '(("data-repo" "item") ("data-repo-2" "item2" "item2.5"))
(define annexed-data (make-parameter '() (lambda (val)
                                           (rnrs:assert (list? val))
                                           val)))

(define (build-assist-data-script annexed-repos)
  (with-imported-modules
      '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((annexed-repos '#$annexed-repos)
               (orig-dir (getcwd))
               (with-chdir
                (lambda (dir proc)
                  (chdir
                   (if (string-prefix? "/" dir)
                       dir
                       (format #f "~a/~a"
                               (getenv "HOME")
                               dir)))
                  (proc)
                  (chdir orig-dir))))
          ;; Always flush buffers regardless of fails to minimize chance that
          ;; changes leave device in invalid state (e.g. via power failure)
          (with-exception-handler
              (lambda (exn) (sync) (exit #f))
            (lambda ()
              (for-each
               (lambda (data-dir)
                 (format #t "SYNCING: ~s\n"
                         data-dir)
                 (with-chdir
                  data-dir
                  (lambda ()
                    (invoke #$(file-append git-annex "/bin/git-annex")
                            "assist"))))
               annexed-repos)
              (sync)))))))

(define data-mod
  (mod
    (name 'data)
    (description
     "Sets up my \"data infrastructure\" and provides additional utilities for
managing it.")
    (he-extension
     (let ((annexed-data (annexed-data)))
       (compose
        (mod-he-packages
         (list borg
               git-annex
               git-annex-configure))
        (mod-he-services
         (list (simple-service name
                               home-impure-symlinks-service-type
                               (map
                                (lambda (symlinks-spec)
                                  (let* ((data-dir (car symlinks-spec))
                                         (item-names (cdr symlinks-spec))
                                         (store-dir (string-append data-dir
                                                                   "/store")))
                                    (cons* "" store-dir item-names)))
                                annexed-data))
               (simple-service name
                               home-files-service-type
                               `((".local/bin/,annex-assist-all"
                                  ,(program-file
                                    "assist-data"
                                    (build-assist-data-script
                                     (map car annexed-data)))))))))))))
