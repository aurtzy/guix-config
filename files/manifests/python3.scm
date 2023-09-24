(use-modules (gnu))

(use-package-modules python python-xyz emacs-xyz)

(packages->manifest (list python-lsp-server
                          python-isort
                          emacs-pyvenv))
