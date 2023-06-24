(define-module (tests suites)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure logging)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-64))

(log-level-set! $debug)

(define tests-dir (string-append (getcwd)"/test-tmp"))
(mkdir tests-dir)
(chdir tests-dir)

(define (get-tmpdir subdir)
  (mkdtemp (string-append
            tests-dir"/"subdir"-XXXXXX")))

(define (cleanup path)
  "Cleans up files in path, removing permissions (i.e. chmod 777) if necessary."
  (ftw path
       (lambda (file _ __)
         (chmod file #o777)
         (if (equal? 'regular
                     (stat:type (stat file)))
             (delete-file file))
         #t)))

(test-begin "utility methods")

(test-equal "sanity check: correct current directory"
            (getcwd)
            tests-dir)

(let ((dir (get-tmpdir "chdir")))
  (test-equal "with-chdir"
              (string-append tests-dir"/"(basename dir))
              (with-chdir
               dir
               (lambda ()
                 (getcwd)))))

(test-group
 "output captures"
 (test-equal "test  string\n"
             (capture-output
              (lambda ()
                (invoke "echo" "test  string"))))
 (test-equal "test  string"
             (capture-output*
              (lambda ()
                (invoke "echo" "test  string"))))
 
 (test-equal ""
             (capture-output
              (lambda ()
                (invoke "echo" "-n"))))
 (test-equal ""
             (capture-output*
              (lambda ()
                (invoke "echo" "-n")))))

(test-end "utility methods")


(test-begin "interface class initializations")

(test-group
 "<remote-name>"
 (test-group
  "invalid names"
  (for-each
   (lambda (name)
     (test-error name
                 &programming-error
                 (remote-name name)))
   '(".test"
     ".test"
     "..test"
     ""
     "test test"
     "test^asdf"
     "test\n"
     "[test]"
     "test@{}asdf"
     "test.."
     "test..test"
     "test/"
     "test/test/")))

 (test-group
  "valid names"
  (for-each
   (lambda (name)
     (test-equal name
                 name
                 (remote-name-ref
                  (remote-name name))))
   '("test"
     "a"
     "test."
     "test/test"
     "test.test/testing"))))

;; TODO These tests fail when attempting to build with Guix, presumably because
;; of permission issues with git. Try to fix.

;; (let ((nonexistant-path "path/to/nowhere")
;;       (git-dir (canonicalize-path
;;                 (get-tmpdir "git-repository"))))
;;   (test-group-with-cleanup
;;    "<git-repository>"
;;    (invoke "git" "init" "--quiet" git-dir)

;;    (test-group
;;     "invalid repository"
;;     (test-error &programming-error
;;                 (make-git-repository nonexistant-path)))

;;    (test-group
;;     "valid repository"
;;     (test-equal git-dir
;;       (git-path-ref (make-git-repository git-dir))))

;;    (cleanup (string-append git-dir"/.git"))))

(test-group
 "<group> and <groups>"
 (test-group
  "invalid names"
  (for-each
   (lambda (group-name)
     (test-error group-name
                 &programming-error
                 (group group-name)))
   '(""
     " test"
     "test "
     "te st")))

 (test-group
  "valid names"
  (let* ((samples '("test"
                    "te\tst"))
         (sample-groups (apply groups samples)))
    (for-each
     (lambda (group-name)
       (test-equal group-name
                   group-name
                   (group-ref (group group-name))))
     samples)
    (for-each
     (lambda (group-name group)
       (test-equal (string-append "groups: "group-name)
                   (group-ref group)
                   group-name))
     samples
     (groups-ref sample-groups)))))

;; (let ((nonexistant-path "path/to/nowhere")
;;       (git-dir (canonicalize-path
;;                 (get-tmpdir "git-repository")))
;;       (git-annex-dir (canonicalize-path
;;                       (get-tmpdir "git-annex-repository"))))
;;   (test-group-with-cleanup
;;    "<git-annex-repository>"
;;    (invoke "git" "init" "--quiet" git-dir)
;;    (invoke "git" "init" "--quiet" git-annex-dir)
;;    (with-chdir
;;     git-annex-dir
;;     (lambda ()
;;       (invoke "git-annex" "--quiet" "init")))

;;    (test-group
;;     "invalid repository"
;;     (test-error &programming-error
;;                 (make-git-annex-repository nonexistant-path))
;;     (test-error &programming-error
;;                 (make-git-annex-repository git-dir)))

;;    (test-group
;;     "valid repository"
;;     (test-equal git-annex-dir
;;       (git-path-ref (make-git-annex-repository git-annex-dir))))

;;    (begin
;;      (cleanup (string-append git-dir"/.git"))
;;      (cleanup (string-append git-annex-dir"/.git")))))

;; TODO configuration classes

(test-end "interface class initializations")


(test-begin "simulate calling git-annex-configure")

;; (let ((repo (get-tmpdir "configure-repository")))
;;   (chdir repo)
;;   (invoke "git" "init" "--quiet" ".")
;;   (invoke "git-annex" "init" "--quiet" ".")
;;   )

(test-end "simulate calling git-annex-configure")
