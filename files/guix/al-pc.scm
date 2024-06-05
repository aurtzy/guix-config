(use-modules (gnu)
             (gnu home)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu packages linux)
             (gnu services sddm)
             (gnu services xorg)
             (gnu system)
             (gnu system file-systems)
             (guix download)
             (guix git-download)
             (guix packages)
             (my-guix base desktop)
             (my-guix config)
             (my-guix home services)
             (my-guix home services package-management)
             (my-guix mods)
             (my-guix mods base)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix mods desktop-extra)
             (my-guix mods entertainment)
             (my-guix mods hardware)
             (my-guix mods server)
             (my-guix packages mesa)
             (my-guix packages keyboard-center)
             (my-guix services hardware)
             (my-guix utils)
             (nongnu packages linux)
             ((nongnu packages nvidia) #:prefix nvidia:)
             ((nongnu services nvidia) #:prefix nvidia:)
             (nongnu system linux-initrd))

(define linux-gfxstrand-nvk
  (let ((revision "0")
        (commit "d6820f36f375ece63226b26568be3a893985dc60"))
    (package
      (inherit
       (customize-linux
        #:name "linux"
        #:linux linux-libre-6.8
        #:source (origin
                   (method git-fetch)
                   (uri
                    (git-reference
                     (url "https://gitlab.freedesktop.org/gfxstrand/linux")
                     (commit commit)))
                   (sha256
                    (base32
                     "1yirhgjaipja17jd0mzf0zcajghzpln6a9qmiwq6glz69qf5hm1l")))))
      (version (git-version "gfxstrand-nvk" revision commit)))))

(define initial-operating-system
  (let ((base-os base-desktop-operating-system))
    (operating-system
      (inherit base-os)
      (host-name "al-pc")
      (kernel linux-gfxstrand-nvk)
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
                                       "kvm"
                                       "libvirt")))
              (operating-system-users base-os)))
      (mapped-devices
       (list (mapped-device
              (source
               (uuid "7ccff0a3-b181-4788-9892-e68306566325"))
              (target "cryptroot")
              ;; TODO Get key file working with root drive
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
              (operating-system-user-services base-os))))))

(define initial-home-environment
  (let ((base-env base-desktop-home-environment))
    (home-environment
     (inherit base-env)
     (services
      (cons* (simple-service 'test-zink
                             (@ (gnu home services) home-environment-variables-service-type)
                             '(("NOUVEAU_USE_ZINK" . "1")))
             (home-environment-user-services base-env))))))


(define system
  (modded-system
    (parameters `((,swapfile ,(swapfile-configuration
                               (file "/swapfile")
                               (device "/dev/mapper/cryptroot")
                               (offset "5250304")))
                  (,replace-mesa ,replace-mesa->mesa-nvk-git)
                  (,annexed-data (("data" "workshop" "areas")
                                  ("storage/data" "library" "attic")))))
    (mods (append desktop-mods
                  extra-mods
                  entertainment-mods
                  (list gnome-mod
                        nvidia-mod
                        web-server-mod)))
    (initial-os initial-operating-system)
    (initial-he initial-home-environment)))

(modded-system-guess-environment system)
