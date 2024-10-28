;;; ../project/emacs-dev/.doom.d/chun-org.el -*- lexical-binding: t; -*-

(use-package! org
  :init (setq-default org-export-with-todo-keywords t)
  (setq-default org-enforce-todo-dependencies t)
  (defun org-insert-quote ()
    (interactive)
    (insert "#+begin_quote\n\n#+end_quote")
    (forward-line -1))


  :config
  (setq org-todo-keyword-faces '(("TODO" :foreground "orange"
                             :weight bold)
                            ("NEXT" :foreground "green"
                             :weight bold)
                            ("DONE" :foreground "grey"
                             :weight bold)
                            ("DOING" :foreground "green"
                             :weight bold)
                            ("WAITING" :foreground "orange"
                             :weight bold)
                            ("HOLD" :foreground "magenta"
                             :weight bold)
                            ("CANCEL" :foreground "forest green"
                             :weight bold)))


  (setq org-todo-keywords '((sequence "TODO(t)" "IDEA(x)" "DOING(i)" "NEXT(n)" "|" "DONE(d)")
                               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  :bind (:map org-mode-map
         ("C-c RET" . org-insert-heading)
         ("C-c q t" . org-insert-quote)
         ("C-c l l" . my-org-insert-link)))




(with-eval-after-load 'ox-latex
 ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
 ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
 ;; automatically to resolve the cross-references.
 (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
 (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  )


(require 'mm-url) ; to include mm-url-decode-entities-string

(use-package! mm-url
  :ensure t)

(after! mm-url
  (defun chun-org-insert-link-smart ()
    "Insert org link where default description is set to html title."
    (interactive)
    (let* ((url (read-string "URL: "))
           (title (get-html-title-from-url url)))
      (org-insert-link nil url title)))

  (defun chun-org-insert-link-smart-str ()
    "Insert org link where default description is set to html title."
    (interactive)
    (let* ((url (read-string "URL: "))
           (title (get-html-title-from-url url)))
      (format "[[%s][%s]]" url title)))

  (defun get-html-title-from-url (url)
    "Return content in <title> tag."
    (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
      (save-excursion (set-buffer download-buffer)
                      (beginning-of-buffer)
                      (setq x1 (search-forward "<title>"))
                      (search-forward "</title>")
                      (setq x2 (search-backward "<"))
                      (mm-url-decode-entities-string
                       (buffer-substring-no-properties
                        x1
                        x2))))))

(setq org-download-heading-lvl 0)

(defun chun-org-element-on-this ()
  (interactive)
  (message "element: %S" (org-element-at-point))
  )

;; (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
;;   "Set the org file publish to another directory"
;;   (unless pub-dir
;;     (setq pub-dir "~/exported-org-files")
;;     (unless (file-directory-p pub-dir)
;;       (make-directory pub-dir)))
;;   (apply orig-fun extension subtreep pub-dir nil))
;; (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

;; refine link as headings
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local imenu-generic-expression
                        '(("Headings" "^\\*+ \\(.*\\)$" 1)
                          ("Links" "\\[\\[\\(.*\\)\\]\\]" 1)))))

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
\\usepackage[cache=false]{minted}
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
               '("beamer" "\\documentclass[8pt,professionalfonts]{beamer}
%\\mode
%\\usetheme{Warsaw}
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
\\usepackage[cache=false]{minted}
%\\institute{{{{beamerinstitute}}}}
%\\subject{{{{beamersubject}}}}
" ("\\section{%s}" . "\\section*{%s}")
("\\begin{frame}[fragile]\\frametitle{%s}" "\\end{frame}" "\\begin{frame}[fragile]\\frametitle{%s}"
 "\\end{frame}")))




(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-hook 'org-mode-hook 'org-indent-mode)
)

;; set some global tags
;; reference https://orgmode.org/guide/Tags.html
(setq org-tag-alist '(("@work" . ?w) ("@life" . ?l) ("@read" . ?r) ("paper" . ?p)))

;; Ajudst the org-mode cache to work with huge files
(setq org-element-cache-persistent nil)
