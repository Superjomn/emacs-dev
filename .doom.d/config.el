(setq user-full-name "Superjomn"
      user-mail-address "yanchunwei _@_ outlook _._ com")


(setq display-line-numbers-type t)


;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chun--os-is-mac ()
  (eq system-type "darwin"))
(defun chun--os-is-windows ()
  (eq system-type "windows-nt"))
(defun chun--os-is-linux ()
  (eq system-type "gnu/linux"))


;; ==============================================================================
(display-time-mode 1)                   ; Enable time in the mode-line


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

;; Load some utility functions.
(load! "./base.el")

(load! "./chun-misc.el")


(defcustom chun-mode/pc-name ""
  "The PC name used for config customization which are different on different PCs."
  :type 'string
  :group 'chun)

;; TODO[Superjomn]: Move to some seperate file such as "cpp-dev.el"
;; (require 'rtags) ;; optional, must have rtags installed
;; (rtags-start-process-unless-running)
;; (cmake-ide-setup)

(require 'helm)
(map! :leader
      :desc "Open like spacemacs" "SPC" #'helm-M-x)

;; Mirror some VIM behavior here
(define-key evil-normal-state-map "vs" '(lambda ()
                                                (interactive)
                                                ;; In some version, the split-window-right-and-focus method is undefined.
                                                (if (boundp 'split-window-right-and-focus)
                                                    (split-window-right-and-focus)
                                                  (progn
                                                    (split-window-right) (other-window 1)))
                                                (balance-windows)))

(defun chun-window-left-balanced ()
  (interactive)
  (evil-window-left 1)
  (balance-windows))

(defun chun-window-right-balanced ()
  (interactive)
  (evil-window-right 1)
  (balance-windows))

(define-key evil-normal-state-map "vh" 'chun-window-left-balanced)
(define-key evil-normal-state-map "vl" 'chun-window-right-balanced)


;; Keymap for chun-mode
(map! :leader
      :desc "chun/insert today date" "cd" #'chun/insert-current-date)

;; Bug on Mac
(map! :leader
      :desc "Open config" "fc" #'doom/find-file-in-private-config)

(map! :leader
      :desc "Open magit status" "gs" #'magit-status)

;; On Mac and Windows, vterm not work well, so eshell is used instead.
(if (not (chun--os-is-linux))
        (map! :leader :desc "Open vterm popup" "'" #'+eshell/toggle)
      (map! :leader :desc "Open vterm popup" "'" #'+vterm/toggle))

(map! :leader :desc "Open vterm popup" "'" #'+vterm/toggle)

(map! :leader
      :desc "Go to scratch buffer" "bs" '(lambda ()
                                           (interactive)
                                           (switch-to-buffer "*scratch*")))

;; Keymap for avy jump
(map! :leader
      :desc "avy jump" "jl" #'avy-goto-line)
(map! :leader
      :desc "avy jump" "jj" #'avy-goto-word-0)

;; Keymap for chun-mode
(map! :leader :desc "chun/insert today date"
      "cd" #'chun/insert-current-date)
(map! :leader :desc "chun/insert anki card"
      :mode 'org-mode
      :map org-mode-map
      "ak" #'chun/anki-sentence-template)
(map! :leader :desc "chun/insert simple anki card"
      :mode 'org-mode
      :map org-mode-map
      "as" #'chun-anki-simple-card)
(map! :leader :desc "chun convert anki card"
      :mode 'org-mode
      :map org-mode-map
      "ac" #'chun/anki-convert)
(map! :leader :desc "chun reset anki org level"
      :mode 'org-mode
      :map org-mode-map
      "ar" #'chun-anki-reset-org-level)
(map! :leader :desc "Convert an heading to simple card"
      :mode 'org-mode
      :map org-mode-map
      "at" #'chun-anki-transform-headline)

(require 'dash)

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
;; (setq irony-cdb-search-directory-list
;;       "~/project/cinn2/")


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
;; (after! elpy
;;   (setq elpy-rpc-virtualenv-path "/Users/yanchunwei/project/matshow/venv")

  ;; format python code before save file
  ;; (add-hook 'elpy-mode-hook (lambda ()
  ;;                             (add-hook 'before-save-hook 'elpy-format-code nil t))))

;; avy jump config
(after! avy
  (map! :leader :desc "Jump to a word" "jj" #'avy-goto-word-or-subword-1)
  (map! :leader :desc "Jump to a word" "jw" #'avy-goto-word-0)
  (map! :leader :desc "Jump to a line" "jl" #'avy-goto-line))

(load! "./chun-org-roam.el")

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

(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "JetBrains Mono" :size 15 :weight 'normal)
          doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15)))



;; -------------------------------------- python ---------------------------------
;; mypy flycheck mode
;;(load! "./mypy-flycheck.el")
(use-package! elpy
  :init
  (elpy-enable))

(after! elpy
  (add-hook! 'elpy-mode-hook (lambda ()
                               (add-hook! 'before-save-hook
                                          'elpy-format-code nil t))))

;; quickly switch fro different layouts
(use-package! eyebrowse
  :config
  (progn
    (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (eyebrowse-mode t)
    (setq eyebrowse-new-workspace t)))

;; (use-package! atomic-chrome
;;   :init
;;   (atomic-chrome-start-server))

(use-package! google-translate
  :bind ("C-c t" . google-translate-at-point))

(use-package! anki-connect)


 ;; end of use-package org


;; (after! org
;;   (add-to-list 'org-latex-classes
;;                '("cn-article"
;;                  "\\documentclass{ctexart}
;; \\usepackage[UTF8]{ctex}
;; \\usepackage{graphicx}
;; \\usepackage{grffile}
;; \\usepackage{longtable}
;; \\usepackage{wrapfig}
;; \\usepackage{rotating}
;; \\usepackage[normalem]{ulem}
;; \\usepackage{amsmath}
;; \\usepackage{textcomp}
;; \\usepackage{amssymb}
;; \\usepackage{capt-of}
;; \\usepackage{hyperref}
;; \\usepackage{minted}
;; \\setCJKmainfont[Scale=0.9]{Adobe Heiti Std}
;; \\setCJKmonofont[Scale=0.9]{Adobe Heiti Std}
;; \\renewcommand{\\MintedPygmentize}{/home/chunwei/.local/bin/pygmentize}"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;   )

;; Set default font, it will override the doom-fonts setting


(setq chun--emacs-font-size 16)
(if (window-system)
    (setq chun--emacs-font-size 12)

    (set-frame-height (selected-frame) 60)
    (set-frame-width (selected-frame) 140))


(after! org
  (add-hook! 'org-mode-hook 'org-download-enable))


(use-package! ox-gfm)

;; (when (chun/os/on-wsl-p)
;;   (load-theme 'doom-acario-dark t))

(use-package! calfw)
(use-package! anki-editor
  :ensure t)
(use-package! calfw-org
  :config
  (require 'calfw-org))

(use-package! org-tree-slide
  :bind (:map org-tree-slide-mode-map
         ("C-M-n" . org-tree-slide-move-next-tree)
         ("C-M-p" . org-tree-slide-move-previous-tree)))

(use-package! ox-hugo
  :after ox)

(load! "./chun-agenda.el")

(use-package! crux)
(use-package! epc)

;; Use org-reveal to write slides only in Mac.
;; (if (chun--os-is-mac)
;;     (progn
;;       (load! "~/emacs-dev/org-reveal/ox-reveal.el")
;;       (require 'ox-reveal)))


;; (org-babel-load-file (concat "~/emacs-dev/chun-mode.org"))

(elpy-enable)

;; load my config from org
(org-babel-load-file (concat chun-mode/org-roam-dir "/20211001225141-emacs_config.org"))


;; (if (chun--os-is-mac)
;;   .load! "../bili.el"))

;;(require 'ox-reveal)

(load! "./chun-project.el")
(load! "./chun-projectile.el")


(after! chun-project
  (chun-project-update-cache)
  (global-set-key (kbd "C-c n x") 'chun-project-search-title))

;; restore the behaviour mapped to "C-l" in helm
(after! helm-files
  (define-key helm-find-files-map (kbd "C-l") 'helm-find-files-up-one-level))

(load! "./chun-anki.el")
(load! "./chun-anki2.el")
(load! "./chun-bookmark.el")

(if (window-system)
      (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)"))


(setq elfeed-feeds
      '(
        ("https://lilianweng.github.io/index.xml" tech ai) ; Lil'Log blog
        ("https://jaykmody.com/feed.xml" tech ai) ; Jay Mody's blog
        ("https://lowlevelbits.org/atom.xml" tech compiler llvm) ; LowLevelBits
      ))


(if (eq system-type 'gnu/linux)
    (progn
      (elpy-enable)
      (setq elpy-rpc-virtualenv-path "~/pyenv2")
      (setq python-shell-virtualenv-path "~/pyenv2")))


;; make '_' a part of word during programming
(defun chun/--treat-_-as-word ()
  "Treat '_' as part of word."
  (modify-syntax-entry ?_ "w"))

;; (add-hook! markdown-mode-hook 'chun/--treat-_-as-word)
;; ;;(add-hook! graphviz-mode-hook 'chun/--treat-_-as-word)
;; (add-hook! emacs-lisp-mode-hook 'chun/--treat-_-as-word)
;; (add-hook! c++-mode-hook 'chun/--treat-_-as-word)
;; (add-hook! python-mode-hook 'chun/--treat-_-as-word)

(load! "./chun-mindmap.el")

;; google-translate
;;(if google-translate-translation-directions-alist
      ;;'(("en" . "ch") ("ch" . "en")))

(if (chun/os/is-macos)
    ;; set PYTHONPATH for elpy mode
    (setq python-shell-extra-pythonpaths '("/Users/yanchunwei/project/tekit0")))

;; TODO does this work?
(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(load! "./chun-babel.el")
(load! "./chun-hugo.el")
(load! "~/emacs-dev/thirdparty/ptx-mode.el")


;; (load! "./chun-yabai.el")

;; (setq easy-hugo-basedir "~/project/myblog2022/content-org")
;; (setq easy-hugo-root "~/project/superjomn.github.io/")
;; (setq easy-hugo-basedir "~/project/superjomn.github.io/")


;; Disable company-mode when using org-mode
;; It is a nightmare on Mac of intel
(add-hook 'org-mode-hook (lambda () (company-mode -1)))

(condition-case err
    (call-interactively #'chun-bookmark/update-web-bookmarks)
  (error (message "Failed to update bookmarks: %S" err)))

