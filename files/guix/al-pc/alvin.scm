(use-modules (gnu home)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu packages minetest)
             (gnu services)
             (guix utils)
             (my-guix mods)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix mods desktop-extra)
             (my-guix mods entertainment)
             (my-guix mods server)
             (my-guix home base desktop)
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
    (parameters `((,annexed-data (("data" "workshop" "areas")
                                  ("storage/data" "library" "attic")))))
    (mods (append desktop-mods
                  extra-mods
                  entertainment-mods
                  (list gnome-mod
                        web-server-mod)))
    (initial-he initial-home-environment)))

(modded-system-home-environment system-home)
