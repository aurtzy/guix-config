(use-modules (gnu home)
             (gnu services)
             (my-guix mods)
             (my-guix home base desktop)
             (my-guix home mods common)
             (my-guix home mods desktop-environment)
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
    (mods (append common-mods
                  extra-mods
                  (list gnome-mod)))
    (initial-he initial-home-environment)))

(parameterize ((annexed-data '(("data" "workshop" "areas" "library" "attic")))
               (excluded-mods (list creative-mod
                                    personal-comms-mod)))
  (modded-system-home-environment system-home))

