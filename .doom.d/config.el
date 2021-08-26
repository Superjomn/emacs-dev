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
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-1337)
(setq doom-theme 'doom-acario-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "/Users/yanchunwei/centra/info_center/agenda")

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

(global-set-key (kbd "C-M-/") 'yas-expand)


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
         ("C-c l l" . my-org-insert-link)))

(after! chun-mode
  (setq org-journal-dir chun-mode/org-roam-dir))


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
      (org-id-update-id-locations)

      (setq org-roam-capture-templates '(
                                         ("d" "default" plain "%?"
                                          :if-new
                                          (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                          :unnarrowed t)
                                         ("b" "beamer" plain "%?"
                                          :if-new (file+head "%<%Y%m%d%H%M%S>-beamer-${slug}.org"
                                                             "#+STARTUP: beamer
#+TITLE:     ${title}
#+AUTHOR:    Chunwei Yan
#+EMAIL:     yanchunwei@baidu.com
#+DATE:      %Y-%m-%d
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+OPTIONS:   H:3 num:t toc:t \n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:https://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+HTML_LINK_UP:
#+HTML_LINK_HOME:
#+BEAMER_FRAME_LEVEL: 2")))

            ;; end of org-roam-capture-templates
            )
      )


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
  (org-journal-dir (concat org-roam-directory "/journal"))
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

(use-package! atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(use-package! google-translate
  :bind ("C-c t" . google-translate-at-point))

(use-package! anki-connect
  :ensure t)


(use-package! org
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-classes '("cn-article" "\\documentclass[10pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{minted}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont[BoldFont=Microsoft YaHei]{Microsoft YaHei}
\\setsansfont[BoldFont=Adobe Heiti Std]{Adobe Heiti Std}
\\setmonofont{Bitstream Vera Sans Mono}
\\newcommand\\fontnamemono{Adobe Heiti Std}%等宽字体
%\\newfontinstance\\MONO{\\fontnamemono}
%\\newcommand{\\mono}[1]{{\\MONO #1}}
\\setCJKmainfont[Scale=0.9]{Adobe Heiti Std}%中文字体
\\setCJKmonofont[Scale=0.9]{Adobe Heiti Std}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
               ;; beamer class, for presentations
               '("beamer" "\\documentclass[11pt,professionalfonts]{beamer}
%\\mode
\\usetheme{Warsaw}
%\\usecolortheme{{{{beamercolortheme}}}}

\\beamertemplateballitem
\\setbeameroption{show notes}
\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\usepackage{lmodern}
\\usepackage{fontspec,xunicode,xltxtra}
\\usepackage{polyglossia}
\\setmainfont{Microsoft YaHei}
\\setCJKmainfont{Microsoft YaHei}
\\setCJKmonofont{Microsoft YaHei}
\\usepackage{verbatim}
\\usepackage{listings}
%\\institute{{{{beamerinstitute}}}}
%\\subject{{{{beamersubject}}}}
" ("\\section{%s}" . "\\section*{%s}")
("\\begin{frame}[fragile]\\frametitle{%s}" "\\end{frame}" "\\begin{frame}[fragile]\\frametitle{%s}"
 "\\end{frame}")))




(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-hook 'org-mode-hook 'org-indent-mode)

) ;; end of use-package org


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



(after! chun-mode
  ;; Currently, on Mac has the required fonts installed.
  (unless (not (chun/os/on-wsl-p))
    ;; Set the default
    (let ((emacs-font-size 14)
          (emacs-font-name "WenQuanYi Micro Hei Mono"))
      (set-frame-font (format "%s-%s" (eval emacs-font-name)
                              (eval emacs-font-size)))
      (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name)))))


(after! '(org chun-mode)
  ;; Set org-download directory.
  (setq-default org-download-image-dir (concat chun-mode/org-roam-dir "/images"))

  ;; Make the table in org-mode better
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil
                        :font "Sarasa Fixed SC 14")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
  ;; Currentlly, other PC doesn't have the fonts required installed.
  (unless (chun/os/on-mac)
    (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)))


(use-package! ox-gfm
  :ensure t)

(load-theme 'doom-acario-dark)

(use-package! calfw
  :ensure t)
(use-package! calfw-org
  :ensure t)

(use-package! anki-editor
  :ensure t)
