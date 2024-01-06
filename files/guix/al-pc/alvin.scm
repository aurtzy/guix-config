(use-modules (gnu home)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu packages minetest)
             (gnu services)
             (my-guix mods)
             (my-guix home base desktop)
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
             (my-guix utils)
             (guix utils))

(define data-mod
  (build-data-mod '(("data" "workshop" "areas")
                    ("storage/data" "library" "attic"))))

(apply-mods
 (home-environment
  (inherit base-desktop-home-environment)
  (services
   (cons* (service home-dbus-service-type)
          (service home-pipewire-service-type)
          (home-environment-user-services base-env))))
 (append common-mods
         extra-mods
         entertainment-mods
         (list data-mod
               gnome-mod
               pipewire-mod
               web-server-mod)))
