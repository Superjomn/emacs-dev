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
(load! "./chun-core.el")

(load! "./chun-misc.el")

(load! "~/emacs-dev/thirdparty/my-python-mode.el")
;;(load! "~/emacs-dev/dsl-mode.el")


(defcustom chun-mode/pc-name ""
  "The PC name used for config customization which are different on different PCs."
  :type 'string
  :group 'chun)

;; TODO[Superjomn]: Move to some seperate file such as "cpp-dev.el"
;; (require 'rtags) ;; optional, must have rtags installed
;; (rtags-start-process-unless-running)
;; (cmake-ide-setup)

(require 'helm)
(map! :leader :desc "Open like spacemacs" "SPC" #'helm-M-x)
(map! :leader :desc "Open treemacs" "ft" #'treemacs)

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

(require 'dash)

;; Helm related.
(require 'helm)
(setq helm-mode-fuzzy-match t)
;;(setq helm-M-x-fuzzy-match t)
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
(load! "./chun-memo.el")

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
;; (use-package! elpy
;;   :commands (elpy-enable))
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
;; (use-package! elpy
;;   :init
;;   (elpy-enable))

;; (after! elpy
;;   (add-hook! 'elpy-mode-hook (lambda ()
;;                                (add-hook! 'before-save-hook
;;                                           'elpy-format-code nil t))))

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

(setq user-emacs-directory
      (or (and (file-directory-p (expand-file-name "~/.emacs.d/"))
               (expand-file-name "~/.emacs.d/"))
          (and (file-directory-p (expand-file-name "~/.config/emacs/"))
               (expand-file-name "~/.config/emacs/"))))


;; The package! can install the package, but it is not loaded.
(load! (concat user-emacs-directory "./.local/straight/repos/anki-editor/anki-editor.el"))
;;(load! (concat user-emacs-directory "./.local/straight/repos/anki-connect/anki-connect.el"))
;(load! anki-editor-path)
;(require 'anki-connect)

;;(use-package! anki-connect)

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
;;(use-package! anki-editor :ensure t)
(use-package! calfw-org
  :config
  (require 'calfw-org))

(use-package! org-tree-slide
  :bind (:map org-tree-slide-mode-map
         ("C-M-n" . org-tree-slide-move-next-tree)
         ("C-M-p" . org-tree-slide-move-previous-tree)))

(use-package! ox-hugo
  :after ox)


(use-package! crux)
(use-package! epc)

;; Use org-reveal to write slides only in Mac.
;; (if (chun--os-is-mac)
;;     (progn
;;       (load! "~/emacs-dev/org-reveal/ox-reveal.el")
;;       (require 'ox-reveal)))


;; (org-babel-load-file (concat "~/emacs-dev/chun-mode.org"))

;; (elpy-enable)

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


;; (if (eq system-type 'gnu/linux)
;;     (progn
;;       (elpy-enable)
;;       (setq elpy-rpc-virtualenv-path "~/pyenv2")
;;       (setq python-shell-virtualenv-path "~/pyenv2")))


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

;; TODO does this work?
(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(load! "./chun-babel.el")
(load! "./chun-hugo.el")
(load! "~/emacs-dev/thirdparty/ptx-mode.el")

;; Keymap for chun-mode -------------------------------------
;; anki related:
(map! :leader :desc "chun/insert anki card"
      :mode 'org-mode
      :map org-mode-map
      "aak" #'chun/anki-sentence-template)
(map! :leader :desc "chun/insert simple anki card"
      :mode 'org-mode
      :map org-mode-map
      "aas" #'chun-anki-simple-card)
(map! :leader :desc "chun convert anki card"
      :mode 'org-mode
      :map org-mode-map
      "aac" #'chun/anki-convert)
(map! :leader :desc "chun reset anki org level"
      :mode 'org-mode
      :map org-mode-map
      "aar" #'chun-anki-reset-org-level)
(map! :leader :desc "Convert an heading to simple card"
      :mode 'org-mode
      :map org-mode-map
      "aat" #'chun-anki-transform-headline)
;; link related
(map! :leader :desc "Insert org link with title extracted from html"
      :mode 'org-mode
      :map org-mode-map
      "ail" #'chun-org-insert-link-smart)

;; chun-memo
(map! :leader :desc "chun memo build db"
      :mode 'org-mode
      :map org-mode-map
      "amb" #'chun-memo-build-db)
(map! :leader :desc "chun memo query"
      :mode 'org-mode
      :map org-mode-map
      "amq" #'chun-memo-query)
(map! :leader :desc "open web calendar"
      :mode 'org-mode
      :map org-mode-map
      "amw" #'chun-agenda-lanuch-web-view)
(map! :leader :desc "open agenda"
      :mode 'org-mode
      :map org-mode-map
      "aaa" #'chun-agenda/org-agenda-in-workspace)

;; misc
(map! :leader :desc "insert date"
      "amd" #'chun/insert-current-date)
(map! :leader :desc "Load bookmarks and project"
      "amb" #'chun-load-worktree)



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

(map! :leader
      :desc "Open config"
      "fc"
      (lambda ()
        (interactive)
        (call-interactively 'projectile-find-file-in-directory "~/emacs-dev/.doom.d/")
        )
      )

(defun chun-load-worktree ()
  "Load bookmarks and project cache"
  (interactive)
  (call-interactively 'chun-bookmark/update-web-bookmarks)
  (call-interactively 'chun-project-update-cache)
  )

;; switch window size
(defvar my-window-sizes
  '((80 . 30)   ; Width: 80 characters, Height: 30 lines
    (137 . 50)
    (113 . 35)
    )) ; Width: 120 characters, Height: 50 lines

(defvar my-current-window-size 0)

(defun chun-switch-window-size ()
  "Switch between predefined window sizes."
  (interactive)
  (setq my-current-window-size (mod (1+ my-current-window-size) (length my-window-sizes)))
  (let ((size (nth my-current-window-size my-window-sizes)))
    (setq width (car size)
          height (cdr size))
    (set-frame-width (selected-frame) width t)
    (set-frame-height (selected-frame) height t)))

(defun chun-get-window-size ()
  (interactive)
  (message "window: (%d %d)" (frame-width) (frame-height))
  )

(map! :leader :desc "Switch window size"
      "amw" #'chun-switch-window-size)


;; Set the http proxy for the whole Emacs
(setq http-proxy "http://127.0.0.1:1095")

(defun chun/move-done-items-to-backup ()
  (interactive)
  (let ((backup-file "~/backup.org")
        (buffer (current-buffer)))
    ;; Ensure the backup file exists.
    (with-temp-buffer
      (insert-file-contents backup-file)
      (write-region nil nil backup-file))
    (save-excursion
      ;; Start from the beginning of the file.
      (goto-char (point-min))
      ;; Loop over all headlines.
      (while (re-search-forward "^\\*+ \\(DONE\\) " nil t)
        (let ((element (org-element-at-point)))
          (when (member "DONE" (org-element-property :todo-keyword element))
            ;; If the current headline is DONE, move it to the backup file.
            (let ((beg (progn (org-back-to-heading t) (point)))
                  (end (progn (org-end-of-subtree t t) (point))))
              ;; Cut the DONE section.
              (append-to-file beg end backup-file)
              (delete-region beg end)
              (write-region nil nil backup-file))))))
    ;; Save changes to the original buffer.
    (with-current-buffer buffer
      (save-buffer))))

;; (after! python
;;   (add-to-list 'auto-mode-alist '("\\.pis\\'" . python-mode))
;;   (add-to-list 'auto-mode-alist '("\\.pim\\'" . python-mode))
;;   )


;; accept completion from copilot and fallback to company
(use-package! copilot
  :load-path  "~/project/copilot.el/copilot.el"
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq! gc-cons-threshold 100000000)

(defun chun-org-get-id ()
  "Create an ID for the current heading and copy it to the clipboard."
  (interactive)
  (let ((id (org-id-get-create)))
    (kill-new id)
    (message "ID %s copied to clipboard" id)))

(map! :leader :desc "LLM"
      "llm" #'gptel)


(defun org-clean-and-start-h2 ()
  "Clean up the current org-mode buffer and start an empty H2 heading."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert "* ")
  ;;(evil-normal-state)
  (evil-insert-state)
  )

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c c") 'org-clean-and-start-h2)))

;; For LLM
;;

(defcustom chun-gptel-model "mistralai/mistral-large-2-instruct"
  "The model to use for completion."
  :type 'string
  :group 'gptel)

(defcustom chun-gptel-host "integrate.api.nvidia.com"
  "The host to use for completion."
  :type 'string
  :group 'gptel)

(defcustom chun-gptel-key ""
  "The key to use for completion."
  :type 'string
  :group 'gptel)

(defcustom chun-gptel-endpoint "/v1/chat/completions"
  "The endpoint to use for completion."
  :type 'string
  :group 'gptel)

(setq gptel-model chun-gptel-model
      gptel-backend
      (gptel-make-openai "Nvidia NIM" ;Any name you want
        :host chun-gptel-host
        :endpoint chun-gptel-endpoint
        :stream t
        :key chun-gptel-key
        :models '("mistralai/mistral-large-2-instruct")))

(setq gptel-default-mode 'org-mode)

(setq jupyter-use-zmq nil)
(defun my-jupyter-api-http-request--ignore-login-error-a
    (func url endpoint method &rest data)
  (cond
   ((member endpoint '("login"))
    (ignore-error (jupyter-api-http-error)
      (apply func url endpoint method data)))
   (:else
    (apply func url endpoint method data))))
(advice-add
 #'jupyter-api-http-request
 :around #'my-jupyter-api-http-request--ignore-login-error-a)

(load! "./chun-agenda.el")
(load! "./chun-agenda-web-view.el")
(require 'chun-agenda-web-view)

;; Make org-passwords work
(require 'org-passwords)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-crypt-key nil)
;; GPG key to use for encryption.
;; nil means  use symmetric encryption unconditionally.
;; "" means use symmetric encryption unless heading sets CRYPTKEY property.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.


;;; some configs borrowed from https://hieuphay.com/doom-emacs-config/

;; Enable automatic visiable of the hidden elements
(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

;; Make background transparent
(add-to-list 'default-frame-alist '(alpha-background . 96))

(setq doom-modeline-height 35)

;; max-width of the buffer contents
;; (use-package! olivetti
;;   :config
;;   (setq-default olivetti-body-width 130)
;;   (add-hook 'mixed-pitch-mode-hook  (lambda () (setq-local olivetti-body-width 80))))

;; (use-package! auto-olivetti
;;   :custom
;;   (auto-olivetti-enabled-modes '(text-mode prog-mode helpful-mode ibuffer-mode image-mode))
;;   :config
;;   (auto-olivetti-mode))

;; make scroll more smoothly
(pixel-scroll-precision-mode t)

(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-enable-variable-pitch nil)

(use-package! doom-modeline
  :config
  (setq doom-modeline-persp-name t))

(defun chun-split-command-lines (start end)
  "Split a single-line shell command into multiple lines with '\\' continuation."
  (interactive "r")
  (let* ((command (buffer-substring-no-properties start end))
         (lines (chun-str-split command))
         (content (mapconcat 'identity lines " \\\n"))
         )
    (message "lines: %S\ncontent: %S" lines content)
    (delete-region start end)
    (insert content)
    ))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)

  :hook ((elpy-mode . flycheck-mode)
           (elpy-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               '((elpy-company-backend :with company-yasnippet))))))

  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "/Users/chunwei/_pyenv/bin/python")
  (setq elpy-rpc-timeout 2)
  )

(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(setq flycheck-python-flake8-executable "~/_pyenv/bin/flake8")
(setq fycheck-python-pylint-executable "/Users/chunwei/_pyenv/bin/pylint")

(global-eldoc-mode 1)

(add-hook 'elpy-mode-hook (lambda () (eldoc-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(add-hook 'fish-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; navigate the file in remote git repo
(after! browse-at-remote
  (setq browse-at-remote-remote-type-regexps
        '((:host "^codeberg\\.org$" :type "codeberg")
        (:host "^github\\.com$" :type "github")
        (:host "^bitbucket\\.org$" :type "bitbucket")
        (:host "^gitlab\\.com$" :type "gitlab")
        (:host "^git\\.savannah\\.gnu\\.org$" :type "gnu")
        (:host "^gist\\.github\\.com$" :type "gist")
        (:host "^git\\.sr\\.ht$" :type "sourcehut")
        (:host "^.*\\.visualstudio\\.com$" :type "ado")
        (:host "^pagure\\.io$" :type "pagure")
        (:host "^.*\\.fedoraproject\\.org$" :type "pagure")
        (:host "^.*\\.googlesource\\.com$" :type "gitiles")
        (:host "^gitlab\\.gnome\\.org$" :type "gitlab")
        (:host "^gitlab\\." :type "gitlab")
        (:host "^gitlab\\-master\\.nvidia\\.com$" :type "gitlab"))
        ))

(use-package! fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :config
  (setq fish-enable-auto-indent t))
