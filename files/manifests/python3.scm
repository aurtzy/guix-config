(use-modules (gnu))

(use-package-modules python python-xyz)

(packages->manifest (list python-lsp-server
                          python-isort))
