(use-modules (gnu)
             (gnu packages linux)
             (gnu services sddm)
             (gnu services xorg)
             (gnu system)
             (gnu system file-systems)
             (my-guix base desktop)
             (my-guix config)
             (my-guix mods)
             (my-guix mods desktop)
             (my-guix packages linux)
             (my-guix services hardware)
             (nongnu packages linux)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             (nongnu system linux-initrd))

(define swapfile-mod
  (build-swapfile-mod
   (swapfile-configuration
    (file "/swapfile")
    (device "/dev/mapper/cryptroot")
    (offset "5250304"))))

(apply-mods
 (let ((base-os base-desktop-operating-system))
   (operating-system
     (inherit base-os)
     (host-name "al-pc")
     (kernel linux-6.7-rc)
     (initrd microcode-initrd)
     (firmware (list linux-firmware))
     (kernel-arguments
      (cons* "nouveau.config=NvGspRm=1"
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
             %base-user-accounts))
     (mapped-devices
      (list (mapped-device
             (source
              (uuid "7ccff0a3-b181-4788-9892-e68306566325"))
             (target "cryptroot")
             (type luks-device-mapping))
            (mapped-device
             (source
              (uuid "d50f62c1-e312-4c83-8b55-b0af00a4de2a"))
             (target "cryptstorage")
             (type luks-device-mapping))))
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
               (dependencies mapped-devices))
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
               (mount-point "/mnt/backup")
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
             (operating-system-user-services base-os)))))
 (list swapfile-mod
       gnome-mod
       virtualization-mod))
