(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base desktop)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions extras)
             (my-guix home services)
             (my-guix home services package-management))

(apply-extensions
 (let ((env base-desktop-home-environment))
   (home-environment
    (inherit env)
    (services
     (cons* (simple-service 'home-impure-symlinks-data
                            home-impure-symlinks-service-type
                            `(("workshop" "data/store/workshop")
                              ("areas" "data/store/areas")
                              ("library" "data/store/library")
                              ("attic" "data/store/attic")))
            (home-environment-user-services env)))))
 (append (list gnome-extension)
         common-extensions
         extras-extensions)
 #:exclude
 (list creative-extension
       personal-comms-extension))
