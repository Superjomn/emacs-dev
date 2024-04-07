(require 'python)

(define-derived-mode jupyter-mode python-mode "Faked for jupyter mode"
  "A customized python-mode to enhance syntax highlighting."
  ;; You can add more keywords or patterns here
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face t))))
