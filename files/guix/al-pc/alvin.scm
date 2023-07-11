(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base foreign-desktop)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions entertainment)
             (my-guix home extensions extras)
             (my-guix home extensions foreign)
             (my-guix home extensions hardware)
             (my-guix home extensions server)
             (my-guix home services package-management)
             (my-guix packages keyboard-center)
             (guix utils))


(extend
 (let ((env base-foreign-desktop-home-environment))
   (home-environment
    (inherit env)
    (packages
     (cons* keyboard-center
            (home-environment-packages env)))
    (services
     (cons* (simple-service 'stow-data
                            home-stow-service-type
                            (list "alvin@al-pc"))
            (home-environment-user-services env)))))
 (append (list plasma-extension
               pipewire-extension
               web-server-extension)
         common-extensions
         extras-extensions
         entertainment-extensions))
