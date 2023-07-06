(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base desktop)
             (my-guix home extensions common)
             (my-guix home extensions extras)
             (my-guix home services))

(extend
 (let ((env base-desktop-home-environment))
   (home-environment
    (inherit env)
    (services
     (cons* (stow-service 'data-stow "alvin@al-laptop")
            (home-environment-user-services env)))))
 (list common-extension
       extras-extension)
 #:excluded
 (list creative-apps-extension
       personal-comms-extension))
