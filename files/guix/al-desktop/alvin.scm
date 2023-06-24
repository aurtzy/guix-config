(use-modules (gnu home)
             (gnu services)
             (my-guix home groups)
             (my-guix home groups base)
             (my-guix home groups common)
             (my-guix home groups desktop-environment)
             (my-guix home groups development)
             (my-guix home groups entertainment)
             (my-guix home groups foreign-distro)
             (my-guix home groups hardware)
             (my-guix home services))

(define-group home
  (stow-service "alvin@al-desktop"))

(home-environment
 (services
  (append home-group
          base-groups
          common-groups
          pipewire-group
          keyboard-center-group
          game-manager-groups
          syncplay-group
          plasma-group
          c-group
          python-group
          minecraft-group
          foreign-distro-group)))
