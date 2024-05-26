(use-modules (gnu home)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu packages minetest)
             (gnu services)
             (guix utils)
             (my-guix mods)
             (my-guix home base desktop)
             (my-guix home mods common)
             (my-guix home mods desktop-environment)
             (my-guix home mods entertainment)
             (my-guix home mods extra)
             (my-guix home mods server)
             (my-guix home services)
             (my-guix home services package-management)
             (my-guix packages keyboard-center)
             (my-guix utils))

(define initial-home-environment
  (let ((base-env base-desktop-home-environment))
    (home-environment
     (inherit base-env)
     )))

(define system-home
  (modded-system
    (mods (append common-mods
                  extra-mods
                  entertainment-mods
                  (list gnome-mod
                        web-server-mod)))
    (initial-he initial-home-environment)))

(parameterize ((annexed-data '(("data" "workshop" "areas")
                               ("storage/data" "library" "attic"))))
  (modded-system-home-environment system-home))
