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
;;; This module includes base mods.

(define-module (my-guix home mods base)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services syncthing)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (my-guix build-system emacs)
  #:use-module (my-guix home mods)
  #:use-module (my-guix home services package-management)
  #:use-module (my-guix home services redlib)
  #:use-module (my-guix mods)
  #:use-module (my-guix packages emacs-xyz)
  #:use-module (my-guix packages git-annex-configure)
  #:use-module (my-guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((rnrs base) #:prefix rnrs:)
  #:export (home-audio-mod
            home-bottles-mod
            home-breeze-theme-mod
            home-browsers-mod
            home-common-fonts-mod
            home-data-entry
            home-data-entry?
            home-data-entry-source
            home-data-entry-borg-repositories
            home-data-entries-argument
            home-data-mod
            home-desktop-environment-mod
            home-emacs-mod
            home-flatpak-mod
            home-media-mod
            home-password-management-mod
            home-redlib-mod
            home-meta-desktop-mod))

(use-package-modules aspell audio backup compression emacs emacs-build
                     emacs-xyz fonts freedesktop guile haskell-apps
                     kde-frameworks kde-plasma music protobuf pulseaudio tex
                     tree-sitter video)


;;; Desktop mods.

(define home-audio-mod
  (home-environment-mod
    (name 'home-audio)
    (description
     "Configures audio packages and services for this environment.  This mod
enables the use of Pipewire.")
    (packages (list easyeffects pavucontrol))
    (services
     (list (service home-pipewire-service-type)
           (simple-service name
                           home-xdg-configuration-files-service-type
                           `( ;; Increase buffer size to avoid crackling.
                             ("pipewire/pipewire-pulse.conf.d/quantum.conf"
                              ,(mixed-text-file "quantum.conf" "\
pulse.properties = {
	pulse.min.quantum = " #;"TODO: make this a parameter?" "512/48000"
        "
}"))))
           (simple-service name
                           home-xdg-configuration-files-service-type
                           `(("easyeffects"
                              ,(symlink-to (path-append-my-assets-directory
                                            "easyeffects" ".static/config")))))
           ;; FIXME: easyeffects service crashes on startup with the error
           ;; "Failed to open display", leading it to be disabled.  Needs
           ;; investigation.  Use other means of starting it for now.
           ;; (simple-service name
           ;;                 home-shepherd-service-type
           ;;                 (list
           ;;                  (shepherd-service
           ;;                   (documentation
           ;;                    "Run Easy Effects (easyeffects).")
           ;;                   (provision
           ;;                    '(easyeffects))
           ;;                   (requirement
           ;;                    '(pipewire))
           ;;                   (start
           ;;                    #~(make-forkexec-constructor
           ;;                       (list #$(file-append easyeffects
           ;;                                            "/bin/easyeffects")
           ;;                             "--gapplication-service")))
           ;;                   (stop
           ;;                    #~(make-kill-destructor)))))
           ))))

(define home-bottles-mod
  (home-environment-mod
    (name 'home-bottles)
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           (list
                            (flatpak-app
                              (id "com.usebottles.bottles")
                              (overrides
                               (flatpak-overrides-configuration
                                 (filesystems
                                  '("~/.guix-home"
                                    "~/Games"
                                    "~/.var/app/com.valvesoftware.Steam/data/Steam")))))))))))

;; TODO: do I actually need this?
(define home-breeze-theme-mod
  (home-environment-mod
    (name 'home-breeze-theme)
    (packages (list breeze breeze-icons))))

(define home-browsers-mod
  (home-environment-mod
    (name 'home-browsers)
    (description
     "This mod adds web browsers to the environment for browsing the
Internet.")
    (services
     (let ((firefox-profile-append (cut path-append
                                        ".var/app/org.mozilla.firefox"
                                        ".mozilla/firefox/profile.default"
                                        <...>)))
       (list
        (simple-service name
                        home-files-service-type
                        `((,(firefox-profile-append "bookmarkbackups")
                           ,(symlink-to
                             (path-append-my-assets-directory
                              "firefox"
                              ".static/default-profile/bookmarkbackups")))))
        (simple-service name
                        home-activation-service-type
                        ;; This file isn't /that/ important, and Firefox keeps
                        ;; overwriting it, so we just copy it without a backup
                        ;; instead (if it exists)
                        #~(let ((src #$(path-append-my-assets-directory
                                        "firefox"
                                        ".static/default-profile/search.json.mozlz4"))
                                (dest #$(path-append-my-home
                                         (firefox-profile-append
                                          "search.json.mozlz4"))))
                            (when (file-exists? src)
                              (copy-file src dest))))
        (simple-service name
                        home-flatpak-profile-service-type
                        (list
                         "org.torproject.torbrowser-launcher"
                         (flatpak-app
                           (id "org.mozilla.firefox")
                           (overrides
                            (flatpak-overrides-configuration
                              (filesystems
                               `(,(path-append-my-assets-directory
                                   "firefox" ".static/default-profile")
                                 "xdg-pictures/screenshots"
                                 "~/.local/share/fonts:ro"
                                 "/run/current-system/profile/share/fonts:ro"
                                 "~/.guix-home:ro"))
                              (environment
                               ;; Zink seems to cause some crashes.
                               '(("NOUVEAU_USE_ZINK" . "0"))))))
                         (flatpak-app
                           (id "com.brave.Browser")
                           (overrides
                            (flatpak-overrides-configuration
                              (filesystems
                               '("~/.local/share/fonts:ro"
                                 "/run/current-system/profile/share/fonts:ro"
                                 "~/.guix-home:ro"))))))))))))

(define home-common-fonts-mod
  (home-environment-mod
    (name 'home-common-fonts)
    (description
     "Adds common fonts that provide support for other languages.")
    (packages (list font-google-noto font-wqy-zenhei))
    (services
     (list (simple-service name
                           home-files-service-type
                           `((".local/share/fonts"
                              ,(symlink-to (path-append-my-home
                                            ".guix-home/profile"
                                            "share/fonts")))))))))

(define-record-type* <home-data-entry>
  home-data-entry make-home-data-entry
  home-data-entry?
  this-home-data-entry
  ;; Path to main data source for this data entry that is written to and read
  ;; from.  It should be relative to $HOME if not absolute.
  (source home-data-entry-source
          (sanitize (lambda (value)
                      (rnrs:assert (string? value))
                      value)))
  ;; Paths to borg repositories that data source is backed up to, if any.
  ;; These should be relative to $HOME if paths are not absolute.
  (borg-repositories home-data-entry-borg-repositories
                     (default '())
                     (sanitize (lambda (value)
                                 (rnrs:assert (and (list? value)
                                                   (every string? value)))
                                 value))))

(define (data-backup-create-script data-entries)
  "Return a script that creates backups of data from DATA-ENTRIES."
  (define borg-repository-sources
    ;; Construct alist with borg repository path as keys and list of entry
    ;; sources as values so we can group together data entries that have the
    ;; same repository when backing up.
    (fold
     (lambda (home-data-entry borg-repository-sources)
       (match-record home-data-entry <home-data-entry>
                     (source borg-repositories)
         (fold
          (lambda (borg-repo borg-repository-sources)
            (assoc-set! borg-repository-sources
                        borg-repo
                        (cons source
                              (or (assoc-ref borg-repository-sources borg-repo)
                                  '()))))
          borg-repository-sources
          borg-repositories)))
     '()
     data-entries))
  (program-file
   "data-backup-create"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (srfi srfi-1)
                      (srfi srfi-26))
         ;; Operations should start in $HOME by default
         (chdir (getenv "HOME"))
         (define (prettify-path path)
           (if (string-prefix? "/" path)
               path
               (string-append "~/" path)))
         (define (format-repo-and-sources borg-repo sources)
           (format (current-error-port) "~s\n" (prettify-path borg-repo))
           (for-each (cut format (current-error-port) "<- ~s\n" <>)
                     ;; Prefix ~/ to relative paths to make them more readable
                     (map prettify-path sources)))
         (define (patterns-file borg-repo)
           (string-append borg-repo "/patterns"))
         (when #$(null? borg-repository-sources)
           (display "No data sources to back up.")
           (exit #f))
         (for-each
          (match-lambda
            ((borg-repo sources ..1)
             (format (current-error-port) "Creating backups for: ")
             (format-repo-and-sources borg-repo sources)
             (unless ((compose zero?
                               status:exit-val)
                      (apply
                       system*
                       `(#$(file-append borg "/bin/borg")
                         "create"
                         "--stats"
                         "--patterns-from" ,(patterns-file borg-repo)
                         ;; Optimize for storage on an HDD
                         "--compression" "zstd,6"
                         ,(string-append borg-repo "::{utcnow}-auto")
                         "--"
                         ,@sources)))
               (display "Error encountered while backing up.\n")
               (exit #f))
             (sync)))
          (filter
           (match-lambda
             ((borg-repo sources ..1)
              ;; Skip backing up a repository if..
              (cond
               ;; ..repository is missing
               ((not (file-exists? borg-repo))
                (format (current-error-port) "\
[WARNING] Skipping a backup; borg repository not found: ")
                (format-repo-and-sources borg-repo sources)
                #f)
               ;; ..some source files are missing
               ((not (every file-exists? sources))
                (format (current-error-port) "\
[WARNING] Skipping a backup; source files for borg repository not found: ")
                (format-repo-and-sources borg-repo sources)
                #f)
               ;; ..patterns file is missing
               ((not (file-exists? (patterns-file borg-repo)))
                ;; XXX: This is technically sufficient for preventing unexpected
                ;; backups of unnecessary files, but consider implementation
                ;; with file-likes or similar to be more "sanitary".
                (format (current-error-port) "\
[WARNING] Skipping a backup; patterns file is missing: ~s\n"
                        (patterns-file borg-repo))
                #f)
               (else
                #t))))
           '#$borg-repository-sources))))))

(define home-data-entries-argument
  (mod-argument
    (keyword #:home-data-entries)
    (description
     "Specifies a list of data entries in the home environment.  Elements of
the list should be a @code{home-data-entry}, but strings may also be used as
a shorthand if only the source needs to be specified.")
    (sanitizer (lambda (value)
                 (map (match-lambda
                        ((? home-data-entry? data-entry)
                         data-entry)
                        ((? string? string)
                         (home-data-entry (source string))))
                      value)))))

(define home-data-mod
  (home-environment-mod
    (name 'home-data)
    (description
     "Sets up my \"data infrastructure\" and provides additional utilities for
managing it.")
    (packages
     (list borg
           git-annex
           ;; TEMP: doesn't build due to install phase being removed
           ;; git-annex-configure
           ))
    (services
     (let-mod-arguments (this-home-environment-mod-arguments)
         ((home-data-entries home-data-entries-argument))
       (list (service home-syncthing-service-type
                      (for-home
                       (syncthing-configuration
                         (user (getenv "USER")))))
             (simple-service name
                             home-files-service-type
                             `((".local/bin/data-backup-create"
                                ,(data-backup-create-script home-data-entries))))
             (simple-service name
                             home-files-service-type
                             (concatenate
                              (map (match-record-lambda <home-data-entry>
                                       (source)
                                     (define source-path
                                       (canonicalize-path
                                        (if (absolute-file-name? source)
                                            source
                                            ;; guix may not necessarily be
                                            ;; invoked from $HOME, so
                                            ;; explicitly append to it here.
                                            (path-append-my-home source))))
                                     ;; ~/data is the destination directory, so
                                     ;; don't create a symlink if the source is
                                     ;; in that directory.
                                     (if (equal? (path-append-my-home "data")
                                                 (dirname source-path))
                                         '()
                                         (list
                                          (list (path-append
                                                 "data" (basename source-path))
                                                (symlink-to source-path)))))
                                   home-data-entries))))))))

(define home-desktop-environment-mod
  (home-environment-mod
    (name 'home-desktop-environment)
    (description
     "Apply configurations depending on the current desktop environment.

When configuring on a Plasma desktop environment, the org.gtk.Gtk3theme.Breeze
flatpak is provided for applying Breeze themes to GTK applications.  This must
still be manually switched to in system settings.")
    (services
     (append
      (match (getenv "XDG_SESSION_TYPE")
        ("wayland"
         (list
          (simple-service name
                          home-bash-service-type
                          (home-bash-extension
                            (environment-variables
                             '(("MOZ_ENABLE_WAYLAND" . "1")))))))
        (else '()))
      (match (getenv "XDG_SESSION_DESKTOP")
        ("KDE"
         (list
          (simple-service name
                          home-flatpak-profile-service-type
                          (list "org.gtk.Gtk3theme.Breeze"))))
        (else '()))))))

(define open-emacs-with-text-script
  ;; Return a program file that - when invoked and passed text in the first
  ;; argument - puts that text in a file and opens it with emacs.  If the
  ;; argument is missing, a default message indicating this case is used.  The
  ;; file is deleted when emacsclient returns (i.e. user is finished with the
  ;; buffer).
  (with-imported-modules '((guix build utils))
    (program-file
     "open-emacs-with-text"
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (ice-9 textual-ports))
         (let* ((port (mkstemp "/tmp/textfile-XXXXXX"))
                (file (port-filename port))
                (text (match (command-line)
                        ((_ text _ ...) text)
                        (else "No text provided."))))
           (put-string port text)
           (close-port port)
           (invoke "emacsclient" "-a" "emacs" file)
           (delete-file file))))))

(define home-emacs-mod
  (home-environment-mod
    (name 'home-emacs)
    (description
     "Configures Emacs.")
    (packages (map
               package-with-emacs-pgtk
               (list emacs-pgtk
                     emacs-ef-themes
                     font-hack
                     font-iosevka-etoile
                     ;; tree-sitter
                     tree-sitter-bash
                     tree-sitter-c
                     tree-sitter-c-sharp
                     tree-sitter-cpp
                     tree-sitter-go
                     tree-sitter-gomod
                     tree-sitter-javascript
                     tree-sitter-python
                     tree-sitter-rust
                     ;; latex with org-mode
                     texlive-beamer
                     texlive-capt-of
                     texlive-libertine
                     texlive-scheme-basic
                     texlive-setspace
                     texlive-ulem
                     texlive-wrapfig
                     ;; project-specific files
                     emacs-envrc
                     emacs-editorconfig
                     ;; completion bundle
                     emacs-vertico
                     emacs-consult
                     emacs-marginalia
                     emacs-orderless
                     ;; Fix Embark using features that have changed in more
                     ;; recent versions of Org; see:
                     ;; <https://github.com/oantolin/embark/issues/723#issuecomment-2212767073>
                     (package/inherit emacs-embark
                       (propagated-inputs
                        (modify-inputs (package-propagated-inputs emacs-embark)
                          (prepend emacs-org))))
                     emacs-corfu
                     ;; editing tweaks
                     emacs-adaptive-wrap
                     emacs-avy
                     emacs-unfill
                     emacs-vundo
                     ;; git
                     emacs-agitjo/maybe-newer
                     emacs-magit
                     emacs-magit-todos ; XXX: Strangely only works with ripgrep?
                     emacs-magit-annex
                     emacs-git-annex
                     emacs-forge
                     ;; guix/guile/scheme hacking
                     guile-3.0
                     emacs-guix
                     emacs-geiser
                     emacs-geiser-guile
                     emacs-paredit
                     ;; dashboard on init
                     emacs-dashboard
                     ;; data science stuff
                     emacs-jupyter
                     emacs-code-cells
                     emacs-csv-mode
                     ;; ispell/aspell for flyspell
                     aspell
                     aspell-dict-en
                     ;; etc
                     emacs-casual
                     emacs-denote
                     emacs-delight
                     emacs-dimmer
                     emacs-disproject/newer
                     emacs-eat
                     emacs-hl-todo
                     emacs-markdown-mode
                     emacs-nftables-mode
                     emacs-package-lint
                     emacs-page-break-lines
                     ;; Last release was in 2020.
                     (let* ((commit "25ba9463a443f0e904147138f226284e437248d3")
                            (transform
                             (options->transformation
                              `((with-commit . ,(string-append
                                                 "emacs-polymode=" commit))))))
                       (transform
                        (package/inherit emacs-polymode
                          (arguments
                           ;; Tests also fail upstream at the moment.
                           (cons* #:tests? #f
                                  (package-arguments emacs-polymode))))))
                     emacs-protobuf-mode
                     emacs-ripgrep
                     emacs-sharper
                     emacs-wgrep
                     emacs-web-mode
                     ;; packages being tried out go below here
                     emacs-dape
                     emacs-org-noter
                     emacs-pdf-tools
                     ;; Enable viewing .zip files
                     unzip)))
    (services
     (list (simple-service name
                           home-xdg-configuration-files-service-type
                           `(("emacs/init.el"
                              ,(symlink-to (path-append-my-files
                                            "emacs/impure/init.el")))
                             ("emacs/lisp"
                              ,(symlink-to (path-append-my-files
                                            "emacs/impure/lisp")))))
           (simple-service name
                           home-environment-variables-service-type
                           '( ;; Set editor for e.g. sudoedit
                             ("VISUAL"
                              .
                              "/usr/bin/env emacsclient -a emacs")
                             ("EDITOR"
                              .
                              "/usr/bin/env emacs -nw")
                             ("SUDO_EDITOR"
                              .
                              "/usr/bin/env emacs")))))))

(define home-flatpak-mod
  (home-environment-mod
    (name 'home-flatpak)
    (description
     "Configures flatpak for the home environment.  This mod adds Flathub as a
remote.")
    (packages (list flatpak-xdg-utils xdg-utils))
    (services
     (list (simple-service name
                           home-files-service-type
                           (append
                            ;; Flatpak doesn't like dangling symlinks, so only
                            ;; make a symlink to this icons directory when it
                            ;; exists (i.e. when on Guix System).
                            (if (file-exists?
                                 "/run/current-system/profile/share/icons")
                                `((".local/share/icons"
                                   ,(symlink-to
                                     "/run/current-system/profile/share/icons")))
                                '())))
           (simple-service name
                           home-flatpak-profile-service-type
                           (list "com.github.tchx84.Flatseal"))))))

;; TODO: Emacs 30 will allow for passing emacsclient arguments to arbitrary
;; functions, so I won't have to take the roundabout path of writing to a file
;; and calling emacsclient with it; instead, I can just pass emacsclient the
;; text to be written to a buffer.
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?h=emacs-30#n76
(define mpv-input-file
  (mixed-text-file
   "mpv-input-file"
   "D show-text \"Displaying video description in Emacs...\""
   "; run \"" open-emacs-with-text-script "\" \"${metadata/ytdl_description}\"\n"
   "c show-text \"Uploader: ${metadata/uploader}\"\n"
   "C show-text \"Opening channel associated with video...\""
   "; run \"xdg-open\" \"${metadata/channel_url}\"\n"))

(define home-media-mod
  (home-environment-mod
    (name 'home-media)
    (description
     "Configures applications for the consumption of media.")
    (packages (list yt-dlp mpv quodlibet))
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           (list
                            (flatpak-app
                              (id "io.freetubeapp.FreeTube")
                              (overrides
                               (flatpak-overrides-configuration
                                 (sockets '("wayland" "!x11"))
                                 (session-bus-policy
                                  '(("org.freedesktop.Flatpak" . "talk"))))))))
           (simple-service name
                           home-bash-service-type
                           (home-bash-extension
                             (aliases
                              '(("mpv-without-cache"
                                 . "mpv --cache-secs=5")))))
           (simple-service name
                           home-xdg-configuration-files-service-type
                           `(("mpv/mpv.conf"
                              ,(local-file
                                (path-append-my-files "mpv/mpv.conf")))
                             ("mpv/input.conf" ,mpv-input-file)
                             ("mpv/scripts/trigger-restart-playback-on-eof.lua"
                              ,(local-file
                                (path-append-my-files
                                 "mpv/scripts"
                                 "trigger-restart-playback-on-eof.lua")))
                             ("mpv/scripts/mpris.so"
                              ,(file-append mpv-mpris "/lib/mpris.so"))))))))

(define home-password-management-mod
  (home-environment-mod
    (name 'home-password-management)
    (description
     "Configures password management applications.")
    (services
     (list (simple-service name
                           home-flatpak-profile-service-type
                           '("org.keepassxc.KeePassXC"))
           (service home-ssh-agent-service-type)))))

(define home-redlib-mod
  (home-environment-mod
    (name 'home-redlib-server)
    (description
     "Configure redlib service.")
    (services
     (list (service home-redlib-service-type (redlib-configuration
                                               (port 8081)))))))

(define home-meta-desktop-mod
  (home-environment-mod
    (name 'home-meta-desktop)
    (addons (list home-audio-mod
                  home-bottles-mod
                  home-breeze-theme-mod
                  home-browsers-mod
                  home-common-fonts-mod
                  home-data-mod
                  home-desktop-environment-mod
                  home-emacs-mod
                  home-flatpak-mod
                  home-media-mod
                  home-password-management-mod
                  home-redlib-mod))))
