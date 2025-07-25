(use-modules (gnu)
             (gnu home)
             (gnu home services desktop)
             (gnu home services shepherd)
             (gnu home services sound)
             (gnu packages linux)
             (gnu services sddm)
             (gnu services web)
             (gnu services xorg)
             (gnu system)
             (gnu system file-systems)
             (gnu system privilege)
             (guix download)
             (guix git-download)
             (guix packages)
             (my-guix base desktop)
             (my-guix config)
             (my-guix home services)
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
             (my-guix utils)
             (nongnu packages linux)
             ((nongnu packages nvidia) #:prefix nvidia:)
             ((nongnu services nvidia) #:prefix nvidia:)
             (nongnu system linux-initrd)
             (nonguix utils))

(define initial-operating-system
  (let ((base-os base-desktop-operating-system))
    (operating-system
      (inherit base-os)
      (host-name "al-pc")
      (kernel linux)
      (label (format #f "GNU with ~a ~a (Nouveau)"
                     (string-titlecase (package-name kernel))
                     (package-version kernel)))
      (initrd microcode-initrd)
      (firmware (list linux-firmware))
      (kernel-arguments
       (cons*
        ;; vfio
        ;; "intel_iommu=on"
        ;; "iommu=pt"
        (operating-system-user-kernel-arguments base-os)))
      (users
       (cons* (user-account
               (name "alvin")
               (comment "Alvin")
               (group "users")
               (home-directory "/home/alvin")
               (supplementary-groups '("wheel"
                                       "netdev"
                                       "audio"
                                       "video"
                                       "kvm")))
              (operating-system-users base-os)))
      (mapped-devices
       (list (mapped-device
              (source
               (uuid "7ccff0a3-b181-4788-9892-e68306566325"))
              (target "cryptroot")
              ;; TODO: Get key file working with root drive
              (type luks-device-mapping))
             (mapped-device
              (source
               (uuid "d50f62c1-e312-4c83-8b55-b0af00a4de2a"))
              (target "cryptstorage")
              (type (luks-device-mapping-with-options
                     #:key-file (string-append "/root/keys/"
                                               (uuid->string source)))))))
      (file-systems
       (cons* (file-system
                (mount-point "/")
                (device "/dev/mapper/cryptroot")
                (flags
                 (base-file-system-flags-ref 'btrfs 'ssd))
                (options
                 (alist->file-system-options
                  (base-file-system-options-ref 'btrfs 'ssd)))
                (type "btrfs")
                (dependencies (filter
                               (lambda (dev)
                                 (member "cryptroot"
                                         (mapped-device-targets dev)))
                               mapped-devices)))
              ;; XXX: This file-system requires cryptstorage map to exist,
              ;; otherwise reconfiguration fails.  Can be a problem in cases
              ;; where storage may not be available (e.g. from configuration
              ;; messups/testing that exclude cryptstorage mapping).  Current
              ;; process is to comment out this file-system, reconfigure, then
              ;; uncomment and reconfigure again once cryptstorage mapping
              ;; exists.
              (file-system
                (mount-point "/home/alvin/storage")
                (device "/dev/mapper/cryptstorage")
                (flags
                 (base-file-system-flags-ref 'btrfs 'hdd))
                (options
                 (alist->file-system-options
                  (base-file-system-options-ref 'btrfs 'hdd)))
                (type "btrfs")
                (create-mount-point? #t)
                (mount-may-fail? #t)
                (dependencies mapped-devices))
              (file-system
                (mount-point "/boot/efi")
                (device (uuid "DC21-DB63" 'fat32))
                (type "vfat"))
              (file-system
                (mount-point "/home/alvin/solid-drive")
                (device (uuid "043dc8b2-b5e6-4bfe-aa58-5ff1e995a22a" 'btrfs))
                (flags
                 (base-file-system-flags-ref 'btrfs 'ssd))
                (options
                 (alist->file-system-options
                  (base-file-system-options-ref 'btrfs 'ssd)))
                (type "btrfs")
                (create-mount-point? #t)
                (mount-may-fail? #t))
              (file-system
                (mount-point "/media/backup")
                (device "/dev/mapper/luks-f42810d8-c723-4521-9646-da12f6103b59")
                (flags
                 (base-file-system-flags-ref 'btrfs 'hdd))
                (options
                 (alist->file-system-options
                  '(("compress-force" . "zstd:10"))))
                (type "btrfs")
                (mount? #f))
              (operating-system-file-systems base-os)))
      (services
       (cons* (service keyboard-center-service-type)
              (service nginx-service-type)
              (simple-service 'redlib-reverse-proxy
                              nginx-service-type
                              (list
                               (nginx-server-configuration
                                (listen '("8080"))
                                (server-name '("redlib.localhost"))
                                (locations
                                 (list
                                  (nginx-location-configuration
                                   (uri "/")
                                   (body '("proxy_pass http://localhost:8081;"))))))))
              (operating-system-user-services base-os))))))

(define initial-home-environment
  (let ((base-he base-desktop-home-environment))
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
             (home-environment-user-services base-he))))))

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
    (initial-os initial-operating-system)
    (initial-he initial-home-environment)))

(modded-system-guess-environment system)
