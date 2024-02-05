(use-modules (gnu))

(use-package-modules python-xyz)

(packages->manifest (list python-lsp-server
                          ;; Below are packages that have to be installed in
                          ;; Python environment if using `layout' with direnv
                          python-isort))
