(use-modules (gnu)
             (gnu system file-systems)
             (my-guix config)
             (my-guix mods)
             (my-guix base desktop))

(apply-mods
 (let ((os base-desktop-operating-system))
   (operating-system
     (inherit os)
     (host-name "al-pc")
     ))
 (list))
