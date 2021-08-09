(use-package! flycheck)

(flycheck-def-args-var flycheck-python-mypy-args python-mypy)

(flycheck-def-config-file-var flycheck-mypy.ini flycheck-mypy "mypy.ini"
  :safe #'stringp)

(defun flycheck-mypy--find-project-root (_checker)
  "Compute an appropriate working-directory for flycheck-mypy.
This is either a parent directory containing a flycheck-mypy.ini, or nil."
  (and
   buffer-file-name
   (locate-dominating-file buffer-file-name flycheck-mypy.ini)))

(flycheck-define-checker python-mypy
  "Mypy syntax checker. Requires mypy>=0.3.1.
Customize `flycheck-python-mypy-args` to add specific args to default
executable.
E.g. when processing Python2 files, add \"--py2\".
See URL `http://mypy-lang.org/'."

  :command ("mypy"
            (config-file "--config-file" flycheck-mypy.ini)
            (eval flycheck-python-mypy-args)
            source-original)
  :working-directory flycheck-mypy--find-project-root
  :error-patterns
  ((error line-start (file-name) ":" line ": error:" (message) line-end)
   (warning line-start (file-name) ":" line ": note:" (message) line-end)
   (info line-start (file-name) ":" line ": note:" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)

(provide 'flycheck-mypy)
;;; flycheck-mypy.el ends here
