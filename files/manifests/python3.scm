(use-modules (gnu))

(use-package-modules python-xyz tree-sitter)

(packages->manifest (list python-lsp-server
                          tree-sitter-python
                          ;; Below are packages that have to be installed in
                          ;; Python environment if using `layout' with direnv
                          python-isort))
