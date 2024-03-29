(use-modules (gnu home)
             (gnu services)
             (my-guix mods)
             (my-guix home base desktop)
             (my-guix home mods common)
             (my-guix home mods desktop-environment)
             (my-guix home mods extra)
             (my-guix home services)
             (my-guix home services package-management))

(define data-mod
  (build-data-mod '(("data" "workshop" "areas" "library" "attic"))))

(apply-mods
 base-desktop-home-environment
 (append common-mods
         extra-mods
         (list data-mod
               gnome-mod))
 #:exclude (list creative-mod
                 personal-comms-mod))

