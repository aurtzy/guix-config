(use-modules (gnu)
             (gnu packages gnome)
             (gnu packages radio)
             (gnu services networking)
             (gnu system file-systems)
             (guix packages)
             (my-guix base desktop)
             (my-guix config)
             (my-guix mods)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(parameterize ((swapfile (swapfile-configuration
                          (file "/swapfile")
                          (device "/dev/mapper/cryptroot")
                          (offset "269568"))))
  (apply-mods
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
         (modify-services (operating-system-user-services base-os)
           (network-manager-service-type
            config => (network-manager-configuration
                       (inherit config)
                       (vpn-plugins (list network-manager-openconnect)))))))))
   (list swapfile-mod
         gnome-mod
         battery-mod
         virtualization-mod)))
