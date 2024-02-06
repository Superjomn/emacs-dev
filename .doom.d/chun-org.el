;;; ../project/emacs-dev/.doom.d/chun-org.el -*- lexical-binding: t; -*-

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

(setq org-todo-keyword-faces '(("TODO" :foreground "red"
                             :weight bold)
                            ("NEXT" :foreground "orange"
                             :weight bold)
                            ("DONE" :foreground "grey"
                             :weight bold)
                            ("DOING" :foreground "orange"
                             :weight bold)
                            ("WAITING" :foreground "orange"
                             :weight bold)
                            ("HOLD" :foreground "magenta"
                             :weight bold)
                            ("CANCEL" :foreground "forest green"
                             :weight bold)))

(setq org-todo-keywords '((sequence "TODO(t)" "IDEA(i)" "STRT(s)" "NEXT(n)" "|" "DONE(d)")
                               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))


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
  (defun my-org-insert-link ()
    "Insert org link where default description is set to html title."
    (interactive)
    (let* ((url (read-string "URL: "))
           (title (get-html-title-from-url url)))
      (org-insert-link nil url title)))

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
