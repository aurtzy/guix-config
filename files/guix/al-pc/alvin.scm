(use-modules (gnu home)
             (gnu services)
             (my-guix extensions)
             (my-guix home base desktop)
             (my-guix home extensions common)
             (my-guix home extensions desktop-environment)
             (my-guix home extensions entertainment)
             (my-guix home extensions extras)
             (my-guix home extensions foreign)
             (my-guix home extensions hardware)
             (my-guix home extensions server)
             (my-guix home services)
             (my-guix packages keyboard-center)
             (guix utils))

(extend
 (let ((env base-desktop-home-environment))
   (home-environment
    (inherit env)
    (packages
     (cons* keyboard-center
            (home-environment-packages env)))
    (services
     (cons* (stow-service 'stow-data "alvin@al-pc")
            (home-environment-user-services env)))))
 (list foreign-extension
       common-extension
       plasma-extension
       game-managers-extension
       minecraft-extension
       minetest-extension
       syncplay-extension
       extras-extension
       pipewire-extension
       web-server-extension))
