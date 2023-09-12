(use-modules (gnu home)
             (gnu services)
             (my-guix mods)
             (my-guix home base desktop)
             (my-guix home mods channels)
             (my-guix home mods common)
             (my-guix home mods desktop-environment)
             (my-guix home mods extra)
             (my-guix home services)
             (my-guix home services package-management))

(apply-mods
 (let ((env base-desktop-home-environment))
   (home-environment
    (inherit env)
    (services
     (cons* (simple-service 'home-impure-symlinks-data
                            home-impure-symlinks-service-type
                            `((""
                               "data/store"
                               "workshop"
                               "areas"
                               "library"
                               "attic")))
            (home-environment-user-services env)))))
 (append common-mods
         extra-mods
         (list gnome-mod
               nonguix-channel-mod))
 #:exclude
 (list creative-mod
       personal-comms-mod))
