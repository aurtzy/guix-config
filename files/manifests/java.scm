(use-modules (gnu)
             (my-guix packages jdt-language-server))

(packages->manifest (list jdt-language-server))
