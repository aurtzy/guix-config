(use-modules (gnu home)
             (gnu services)
             (my-guix mods)
             (my-guix mods desktop)
             (my-guix mods desktop-environment)
             (my-guix home base desktop)
             (my-guix home mods extra)
             (my-guix home services)
             (my-guix home services package-management))

(define initial-home-environment
  (let ((base-env base-desktop-home-environment))
    (home-environment
     (inherit base-env)
     )))

(define system-home
  (modded-system
    (parameters `((,annexed-data (("data" "workshop" "areas" "library" "attic")))
                  (,excluded-mods ,(list creative-mod
                                         personal-comms-mod))))
    (mods (append desktop-mods
                  extra-mods
                  (list gnome-mod)))
    (initial-he initial-home-environment)))

(modded-system-home-environment system-home)

