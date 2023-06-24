(use-modules (gnu home)
             (gnu services)
             (my-guix home groups)
             (my-guix home groups base)
             (my-guix home groups common)
             (my-guix home groups desktop-environment)
             (my-guix home groups development)
             (my-guix home groups foreign-distro)
             (my-guix home groups hardware)
             (my-guix home services))

(define-group home
  (stow-service "alvin@al-laptop"))

(home-environment
 (services
  (append home-group
          base-groups
          common-groups
          pipewire-group
          plasma-group
          c-group
          python-group
          foreign-distro-group)))
