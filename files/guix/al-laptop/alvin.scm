(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base desktop)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions extras)
             (my-guix home services package-management))

(apply-extensions
 (let ((env base-desktop-home-environment))
   (home-environment
    (inherit env)
    (services
     (cons* (simple-service 'data-stow
                            home-stow-service-type
                            (list "alvin@al-laptop"))
            (home-environment-user-services env)))))
 (append (list gnome-extension)
         common-extensions
         extras-extensions)
 #:exclude
 (list creative-extension
       personal-comms-extension))
