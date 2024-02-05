(use-modules (gnu))

(use-package-modules python-xyz tree-sitter)

(packages->manifest (list python-lsp-server
                          python-isort
                          tree-sitter-python))
