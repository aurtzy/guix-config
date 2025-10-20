(use-modules (gnu)
             (gnu home)
             (gnu packages gnome)
             (gnu packages radio)
             (gnu services networking)
             (gnu system file-systems)
             (guix packages)
             (my-guix config)
             (my-guix home services package-management)
             (my-guix mods)
             (my-guix mods data)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix mods desktop-extra)
             (my-guix mods hardware)
             (my-guix mods server)
             ((my-guix systems)
              #:select ((base-desktop-operating-system . base-os)
                        (base-desktop-home-environment . base-he)))
             (my-guix systems al-laptop)
             (my-guix systems al-laptop alvin)
             (nongnu packages linux)
             (nongnu system linux-initrd))

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
    (initial-os al-laptop-operating-system)
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
    (initial-he alvin-home-environment)))

(modded-system-guess-environment system)
