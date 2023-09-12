(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base desktop)
             (my-guix home extensions channels)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions extra)
             (my-guix home services)
             (my-guix home services package-management))

(apply-extensions
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
 (append common-extensions
         extra-extensions
         (list gnome-extension
               nonguix-channel-extension))
 #:exclude
 (list creative-extension
       personal-comms-extension))
