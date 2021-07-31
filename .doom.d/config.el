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

(if (eq initial-window-system 'x)       ; Startup by full frame
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

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

(require 'rtags) ;; optional, must have rtags installed
(rtags-start-process-unless-running)
(cmake-ide-setup)

(require 'helm)
(map! :leader
      :desc "Open like spacemacs" "SPC" #'helm-M-x)


;; window control
(if (chun/os/is-linux)
        (define-key evil-normal-state-map "vs" '(lambda ()
                                                (interactive)
                                                (split-window-right-and-focus)
                                                (balance-windows))))
;; on mac, no (split-window-right-and-focus)
(if (chun/os/is-macos)
    (define-key evil-normal-state-map "vs" '(lambda ()
                                                (interactive)
                                                (split-window-right)
                                                (balance-windows)
                                                (other-window 1))))

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

(setq chun/--projectile-known-projects
      '("~/project/pscore"
        "~/centra/info_center"
        "~/project/emacs-dev"
        "~/project/algo-trading"
        ))

(-map (lambda (path)
        (projectile-add-known-project path))
      chun/--projectile-known-projects)


(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


;; YAS related.
(setq yas-snippet-dirs '(
                         "/home/chunwei/project/yas-snippets"
                         ))
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
        ".idea"
        ))
(require 'projectile)
;; This is broken, not works
(-map (lambda (s)
        (setq projectile-globally-ignored-directories
              (add-to-list 'projectile-globally-ignored-directories s))
        )
      chun/--projectile-globally-ignored-directories)
(setq irony-cdb-search-directory-list
      "/home/chunwei/project/pscore/"
      )

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
    (message "Output: %s" -output)
    ))

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

;; set encoding
;; This seems not working
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Load my config
(load! "./chun.el")


;; Python config
(use-package! elpy
  :commands (elpy-enable))

(setq elpy-rpc-virtualenv-path "~/project/algo-trading/venv")
;; format python code before save file
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-format-code nil t)))

;; avy jump config
(map! :leader
      :desc "Jump to a word" "jj" #'avy-goto-word-or-subword-1)
(map! :leader
      :desc "Jump to a word" "jw" #'avy-goto-word-0)
(map! :leader
      :desc "Jump to a line" "jl" #'avy-goto-line)
