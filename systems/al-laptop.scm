(use-modules (gnu)
             (gnu home)
             (gnu packages gnome)
             (gnu packages radio)
             (gnu services networking)
             (gnu system file-systems)
             (guix packages)
             (my-guix base desktop)
             (my-guix config)
             (my-guix home services)
             (my-guix home services package-management)
             (my-guix mods)
             (my-guix mods data)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix mods desktop-extra)
             (my-guix mods hardware)
             (my-guix mods server)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(define initial-operating-system
  (let ((base-os base-desktop-operating-system))
    (operating-system
      (inherit base-os)
      (kernel linux)
      (initrd microcode-initrd)
      (kernel-arguments
       ;; Fix keyboard not working when resuming from suspend
       (cons* "i8042.dumbkbd"
              "modprobe.blacklist=dvb_usb_rtl28xxu"
              (operating-system-user-kernel-arguments base-os)))
      (host-name "al-laptop")
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
                                        "libvirt"
                                        ;; rtl-sdr
                                        "dialout")))
              (operating-system-users base-os)))
      (mapped-devices
       (list (mapped-device
               (source
                (uuid "bf8c2749-f357-4c6e-be6b-f1009a58aa5f"))
               (target "cryptroot")
               (type luks-device-mapping))))
      (file-systems
       (cons* (file-system
                (mount-point "/")
                (device "/dev/mapper/cryptroot")
                (type "btrfs")
                (flags
                 (base-file-system-flags-ref 'btrfs 'ssd))
                (options
                 (alist->file-system-options
                  (base-file-system-options-ref 'btrfs 'ssd)))
                (dependencies mapped-devices))
              (file-system
                (mount-point "/boot/efi")
                (device (uuid "DC21-DB63"
                              'fat32))
                (type "vfat"))
              (operating-system-file-systems base-os)))
      (packages
       (cons* rtl-sdr
              (operating-system-packages base-os)))
      (services
       (cons*
        (udev-rules-service 'rtl-sdr rtl-sdr)
        (operating-system-user-services base-os))))))

(define initial-home-environment
  (let ((base-env base-desktop-home-environment))
    (home-environment
      (inherit base-env)
      )))

(define system
  (modded-system
    (parameters `((,swapfile ,(swapfile-configuration
                               (file "/swapfile")
                               (device "/dev/mapper/cryptroot")
                               (offset "269568")))
                  (,data-entries ("workshop" "areas" "library" "archives"))
                  (,excluded-mods ,(list creative-mod
                                         personal-comms-mod))))
    (mods (append desktop-mods
                  extra-mods
                  (list battery-mod
                        data-mod
                        gnome-mod
                        ssh-server-mod)))
    (initial-os initial-operating-system)
    (final-os-extension
     (lambda (os)
       (operating-system
         (inherit os)
         (services
          (modify-services (operating-system-user-services os)
            (network-manager-service-type
             config => (network-manager-configuration
                         (inherit config)
                         (vpn-plugins (list network-manager-openconnect)))))))))
    (initial-he initial-home-environment)))

(modded-system-guess-environment system)
