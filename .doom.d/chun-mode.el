(defcustom chun-mode/keymap-prefix "C-c C-."
  "The prefix for chun-mode key bindings."
  :type 'string
  :group 'chun)

(defcustom chun-mode/org-roam-dir "~/OneDrive/org-roam"
  "The org-roam cloud directory."
  :type 'string
  :group 'chun)

(defcustom chun-mode/yas-snippets-dirs '("~/project/yas-snippets")
  "The yas-snippets directory."
  :type '(restricted-sexp :tag "Vector"
                          :match-alternatives
                          (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs))))
  :group 'chun)

(defcustom chun-mode/projectile-dirs '("~/project/pscore"
                                         "~/centra/info_center"
                                         "~/project/emacs-dev"
                                         "~/project/algo-trading")
  "The yas-snippets directory."
  :type '(restricted-sexp :tag "Vector"
                          :match-alternatives
                          (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs))))
  :group 'chun)

(define-minor-mode chun-mode
  "Toggles chun mode"
  nil
  :global t
  :group 'chun
  :lighter " chun")
