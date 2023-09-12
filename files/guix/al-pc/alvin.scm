(use-modules (gnu home)
             (gnu services)
             (my-guix mods)
             (my-guix home base foreign-desktop)
             (my-guix home mods channels)
             (my-guix home mods common)
             (my-guix home mods desktop-environment)
             (my-guix home mods entertainment)
             (my-guix home mods extra)
             (my-guix home mods hardware)
             (my-guix home mods server)
             (my-guix home services)
             (my-guix home services package-management)
             (my-guix packages keyboard-center)
             (guix utils))


(apply-mods
 (let ((env base-foreign-desktop-home-environment))
   (home-environment
    (inherit env)
    (packages
     (cons* keyboard-center
            (home-environment-packages env)))
    (services
     (cons* (simple-service 'home-impure-symlinks-data
                            home-impure-symlinks-service-type
                            '((""
                               "data/store"
                               "workshop"
                               "areas")
                              (""
                               "/mnt/storage/data/store"
                               "library"
                               "attic")))
            (home-environment-user-services env)))))
 (append common-mods
         extra-mods
         entertainment-mods
         (list plasma-mod
               pipewire-mod
               web-server-mod
               nonguix-channel-mod)))
