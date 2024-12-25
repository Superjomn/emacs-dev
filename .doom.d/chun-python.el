(use-package! eglot
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; (use-package! lsp-booster
;;   :hook (eglot-managed-mode . lsp-booster-mode))

(use-package! pyvenv
  :config
  (setenv "WORKON_HOME" "~/_pyenv")
  (pyvenv-tracking-mode 1))

(use-package! eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))
