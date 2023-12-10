(require 'projectile)

(defcustom chun-mode/projectile-dirs '("~/project/pscore"
                                         "~/centra/info_center"
                                         "~/project/emacs-dev"
                                         "~/project/algo-trading")
  "The yas-snippets directory."
  :type '(restricted-sexp :tag "Vector"
                          :match-alternatives
                          (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs))))
  :group 'chun)


(defvar chun/projectile-ignored-directories
  '("^\\.git$" "env"
    "^\\.idea$" "^cmake-build-debug$"
    "^build*$" "^cmake-build-debug-*"
    "^__pycache__$"
    ".pytest_cache"
    )
  "Directories to ignore")


(-map (lambda (path)
        (projectile-add-known-project path)) chun-mode/projectile-dirs)

(setq projectile-indexing-method 'native)


(setq projectile-generic-command
        (mapconcat #'shell-quote-argument
                   (append (list "rg" "-0" "--files" "--follow" "--color=never" "--hidden")
                           (cl-loop for dir in chun/projectile-ignored-directories collect
                                    "--glob" collect (concat "!" dir))) " ") projectile-git-command
                                    projectile-generic-command)
