(use-modules (gnu home)
             (gnu packages minetest)
             (gnu services)
             (my-guix mods)
             (my-guix home base foreign-desktop)
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

(define environment
  (apply-mods
   (let ((env base-foreign-desktop-home-environment))
     (home-environment
      (inherit env)
      (packages
       (cons* keyboard-center
              (home-environment-packages env)))
      (services
       (cons* (simple-service 'home-impure-symlinks-data
                              home-impure-symlinks-service-type
                              '((""
                                 "data/store"
                                 "workshop"
                                 "areas")
                                (""
                                 "/mnt/storage/data/store"
                                 "library"
                                 "attic")))
              (home-environment-user-services env)))))
   (append common-mods
           extra-mods
           entertainment-mods
           (list plasma-mod
                 pipewire-mod
                 web-server-mod
                 nonguix-channel-mod))))

;; Use flatpak instead of minetest-mod since foreign system is
;; not set up to use NVIDIA proprietary driver
(home-environment
 (inherit environment)
 (packages
  (delq minetest (home-environment-packages environment)))
 (services
  (cons* (simple-service 'home-flatpak-minetest
                         home-flatpak-profile-service-type
                         '((flathub "net.minetest.Minetest")))
         (simple-service 'home-impure-symlinks-minetest
                         home-impure-symlinks-service-type
                         `((".local/share/flatpak/overrides"
                            ,(path-append-my-files "impure/minetest")
                            "net.minetest.Minetest")))
         (home-environment-user-services environment))))
