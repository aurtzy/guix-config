(use-modules (gnu)
             (gnu system file-systems)
             (my-guix config)
             (my-guix extensions)
             (my-guix base desktop)
             (my-guix extensions desktop))

(define swapfile-extension
  (build-swapfile-extension 
   (swapfile-configuration
    (file "/swapfile")
    (device "/dev/mapper/cryptroot")
    (offset "269568"))))

(apply-extensions
 (let ((os base-desktop-operating-system))
   (operating-system
     (inherit os)
     (host-name "al-laptop")
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
               (device (uuid "955C-7266"
                             'fat32))
               (type "vfat"))
             (operating-system-file-systems os)))))
 (list swapfile-extension
       gnome-extension))
