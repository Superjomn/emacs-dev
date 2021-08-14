;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Superjomn"
      user-mail-address "Superjomn@")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/yanchunwei/centra/info_center/agenda")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ==============================================================================
(display-time-mode 1)                   ; Enable time in the mode-line

;; (if (eq initial-window-system 'x)       ; Startup by full frame
;;     (toggle-frame-maximized)
;;   (toggle-frame-fullscreen))

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string ".*/[0-9]*-?" "<" buffer-file-name)
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; ==============================================================================
;; Load some utility functions.
(load! "./base.el")
;; chun-mode contains all of the personal settings.
(load! "./chun-mode.el")
(use-package! chun-mode
  :ensure t)

;; (require 'rtags) ;; optional, must have rtags installed
;; (rtags-start-process-unless-running)
;; (cmake-ide-setup)

(require 'helm)
(map! :leader
      :desc "Open like spacemacs" "SPC" #'helm-M-x)

(define-key evil-normal-state-map "vs" '(lambda ()
                                                (interactive)
                                                ;; In some version, the split-window-right-and-focus method is undefined.
                                                (if (boundp 'split-window-right-and-focus)
                                                    (split-window-right-and-focus)
                                                  (progn
                                                    (split-window-right) (other-window 1)))
                                                (balance-windows)))


(define-key evil-normal-state-map "vh" 'evil-window-left)
(define-key evil-normal-state-map "vl" 'evil-window-right)


;; Bug on Mac
(map! :leader
      :desc "Open config" "fc" #'doom/find-file-in-private-config)

(map! :leader
      :desc "Open magit status" "gs" #'magit-status)

(map! :leader
      :desc "Open vterm popup" "'" #'+vterm/toggle)

(map! :leader
      :desc "Go to scratch buffer" "bs" '(lambda ()
                                           (interactive)
                                           (switch-to-buffer "*scratch*")))


(require 'dash)

;; projectile
(after! projectile
  (setq chun/--projectile-known-projects chun-mode/projectile-dirs)
  (-map (lambda (path)
          (projectile-add-known-project path)) chun/--projectile-known-projects)
  (setq projectile-globally-ignored-directories '("*.git" "env"))
  (setq projectile-indexing-method 'native)
  (setq projectile-generic-command
        (mapconcat #'shell-quote-argument
                   (append (list "rg" "-0" "--files" "--follow" "--color=never" "--hidden")
                           (cl-loop for dir in projectile-globally-ignored-directories collect
                                    "--glob" collect (concat "!" dir))) " ") projectile-git-command
                                    projectile-generic-command))


(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


;; YAS related.
(setq yas-snippet-dirs chun-mode/yas-snippets-dirs)
(yas-global-mode 1)

(global-set-key (kbd "M-/") 'yas-expand)


;; Helm related.
(require 'helm)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-semantic-fuzzy-match t)

;;; ===================== details =====================
(setq chun/--projectile-globally-ignored-directories
      '("*build"
        "*cmake-build-debug"
        ".idea"))
(require 'projectile)
;; This is broken, not works
(-map (lambda (s)
        (setq projectile-globally-ignored-directories
              (add-to-list 'projectile-globally-ignored-directories s)))
      chun/--projectile-globally-ignored-directories)
(setq irony-cdb-search-directory-list
      "/home/chunwei/project/pscore/")

;;; make the code style as google-c-style
(add-hook 'c-mode-common-hook 'google-set-c-style)


(defun chun/update-compile-commands ()
  "Run update-compile-commands.sh in a project
NOTE it use the variable defined in .dir-locals.el in the specific project.
"
  (interactive)
  (message "working on dirctor: %s" cmake-ide-project-dir)
  (let*
      ((bash-file (concat cmake-ide-project-dir "/" "update_compile_commands.sh"))
       (default-directory cmake-ide-project-dir)
       (-output (shell-command-to-string bash-file)))
    (message "Output: %s" -output)))

;; Info colors
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)


;; Ctrl-K remove the whole line
(setq kill-whole-line t)

;; set spacemacs theme
(setq doom-theme 'spacemacs-light)


(load! "./chun-agenda.el")

(use-package! org
  :init (setq-default org-export-with-todo-keywords t)
  (setq-default org-enforce-todo-dependencies t)
  (defun org-insert-quote ()
    (interactive)
    (insert "#+begin_quote\n\n#+end_quote")
    (forward-line -1))
  :bind (:map org-mode-map
         ("C-c RET" . org-insert-heading)
         ("C-c q t" . org-insert-quote)
         ))

(setq org-journal-dir chun-mode/org-roam-dir)

(setq org-todo-keyword-faces '(("TODO" :foreground "red"
                             :weight bold)
                            ("NEXT" :foreground "blue"
                             :weight bold)
                            ("DONE" :foreground "forest green"
                             :weight bold)
                            ("WAITING" :foreground "orange"
                             :weight bold)
                            ("HOLD" :foreground "magenta"
                             :weight bold)
                            ("CANCELLED" :foreground "forest green"
                             :weight bold)))
(setq org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "NEXT(n)" "|" "DONE(d)")
                               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(load! "./chun-org.el")


;; set encoding
;; This seems not working
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Load my config
(load! "./chun.el")


;; Python config
(use-package! elpy
  :commands (elpy-enable))
(after! elpy
  (setq elpy-rpc-virtualenv-path "~/project/algo-trading/venv")
  ;; format python code before save file
  (add-hook 'elpy-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'elpy-format-code nil t))))

;; avy jump config
(after! avy
  (map! :leader :desc "Jump to a word" "jj" #'avy-goto-word-or-subword-1)
  (map! :leader :desc "Jump to a word" "jw" #'avy-goto-word-0)
  (map! :leader :desc "Jump to a line" "jl" #'avy-goto-line))

(setq org-roam-v2-ack t)
(use-package! org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename chun-mode/org-roam-dir))
      (org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
      (org-roam-complete-everywhere t)
      (org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol)
      (org-id-update-id-locations))


(use-package! deft
  :after org
  :bind ("C-c n d" . deft)
  :custom (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))


(use-package! org-journal
  :after org
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir org-roam-directory)
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A > %d %B %Y"))
(setq org-journal-enable-agenda-integration t)

(use-package! org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;; org babel ;;
;; auto insert code
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++"
                                       "css" "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond"
                                       "mscgen" "octave" "oz" "plantuml" "R" "sass" "screen" "sql"
                                       "awk" "ditaa" "haskell" "latex" "lisp" "matlab" "ocaml" "org"
                                       "perl" "ruby" "scheme" "sqlite")))
                 (list (ido-completing-read "Source code type: " src-code-types))))
  (progn (newline-and-indent)
         (insert (format "#+BEGIN_SRC %s\n" src-code-type))
         (newline-and-indent)
         (insert "#+END_SRC\n")
         (previous-line 2)
         (org-edit-src-code)))
(add-hook 'org-mode-hook '(lambda ()
                            ;; keybiding for insert source code
                            (local-set-key (kbd "C-c s") 'org-insert-src-block)))
;; add support for exectuate c++ in org-mode
(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)
                                                         (latex . t)))

(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14))

;; -------------------------------------- python ---------------------------------
;; mypy flycheck mode
(load! "./mypy-flycheck.el")
(use-package! elpy
  :ensure t
  :init
  (elpy-enable))

(after! elpy
  (add-hook! 'elpy-mode-hook (lambda ()
                               (add-hook! 'before-save-hook
                                          'elpy-format-code nil t))))

;; quickly switch fro different layouts
(use-package! eyebrowse
  :ensure t
  :config
  (progn
    (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (eyebrowse-mode t)
    (setq eyebrowse-new-workspace t)))


(use-package! yasnippet
  :ensure t
  :bind
  (:map yas-minor-mode-map
   (("<tab>" . yas/expand))))
