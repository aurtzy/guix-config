(use-modules (gnu)
             (gnu home)
             (gnu home services desktop)
             (gnu home services shepherd)
             (gnu home services sound)
             (gnu packages linux)
             (gnu services linux)
             (gnu services sddm)
             (gnu services web)
             (gnu services xorg)
             (gnu system)
             (gnu system file-systems)
             (gnu system privilege)
             (guix download)
             (guix git-download)
             (guix packages)
             (my-guix config)
             (my-guix home services package-management)
             (my-guix mods)
             (my-guix mods base)
             (my-guix mods data)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix mods desktop-extra)
             (my-guix mods entertainment)
             (my-guix mods hardware)
             (my-guix mods server)
             (my-guix packages game-client)
             (my-guix packages mesa)
             (my-guix packages keyboard-center)
             (my-guix packages redlib)
             (my-guix services hardware)
             ((my-guix systems)
              #:select ((base-desktop-operating-system . base-os)
                        (base-desktop-home-environment . base-he)))
             (my-guix systems al-pc)
             (my-guix utils)
             (nongnu packages linux)
             ((nongnu packages nvidia) #:prefix nvidia:)
             ((nongnu services nvidia) #:prefix nvidia:)
             (nongnu system linux-initrd)
             (nonguix utils))

(define initial-home-environment
  (home-environment
    (inherit base-he)
    (services
     (cons* (simple-service 'redlib
                            home-shepherd-service-type
                            (list
                             (shepherd-service
                               (documentation
                                "Run Redlib service.")
                               (provision
                                '(redlib))
                               (requirement
                                '())
                               (start
                                #~(make-forkexec-constructor
                                   (list #$(file-append redlib "/bin/redlib")
                                         "--port" "8081")))
                               (stop
                                #~(make-kill-destructor)))))
            (home-environment-user-services base-he)))))

(define system
  (modded-system
    (parameters `((,swapfile ,(swapfile-configuration
                               (file "/swapfile")
                               (device "/dev/mapper/cryptroot")
                               (offset "6036736")))
                  (,data-entries ,(list (data-entry
                                         (source "data/workshop")
                                         (borg-repositories
                                          '("/media/backup/workshop.borg"
                                            "/media/usb-backup/workshop.borg")))
                                        (data-entry
                                         (source "data/areas")
                                         (borg-repositories
                                          '("/media/backup/areas.borg"
                                            "/media/usb-backup/areas.borg")))
                                        (data-entry
                                         (source "storage/library")
                                         (borg-repositories
                                          '("/media/backup/library.borg"
                                            "/media/usb-backup/library.borg")))
                                        (data-entry
                                         (source "storage/archives"))))))
    (mods (append desktop-mods
                  extra-mods
                  entertainment-mods
                  (list data-mod
                        gnome-mod
                        nvidia-mod
                        ssh-server-mod
                        web-server-mod)))
    (final-he-extension (lambda (he)
                          (let ((replace-mesa (replace-mesa)))
                            (with-transformation replace-mesa he))))
    (initial-os al-pc-operating-system)
    (initial-he initial-home-environment)))

(modded-system-guess-environment system)
