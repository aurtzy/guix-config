(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base foreign-desktop)
             (my-guix home extensions channels)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions entertainment)
             (my-guix home extensions extra)
             (my-guix home extensions hardware)
             (my-guix home extensions server)
             (my-guix home services)
             (my-guix home services package-management)
             (my-guix packages keyboard-center)
             (guix utils))


(apply-extensions
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
 (append common-extensions
         extra-extensions
         entertainment-extensions
         (list plasma-extension
               pipewire-extension
               web-server-extension
               nonguix-channel-extension)))
