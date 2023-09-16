(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive (let* ((src-code-types '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++"
                                       "css" "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond"
                                       "mscgen" "octave" "oz" "plantuml" "R" "sass" "screen" "sql"
                                       "awk" "ditaa" "haskell" "latex" "lisp" "matlab" "ocaml" "org"
                                       "perl" "ruby" "scheme" "sqlite"
                                       "cpp" "cmake" "swift" "cuda" "llvm" "td" "ptx" "yaml"
                                       "groovy"
                                       )))
                 (list (ido-completing-read "Source code type: " src-code-types))))

  (let* (type)
    (setq type src-code-type)
    (when (string= type "cpp")
      (setq type "C++"))
    (when (string= type "td")
      (setq type "tablegen")
      )
    (progn (newline-and-indent)
         (insert (format "#+BEGIN_SRC %s\n" type))
         (newline-and-indent)
         (insert "#+END_SRC\n")
         (previous-line 2)
         (org-edit-src-code))))

(add-hook 'org-mode-hook '(lambda ()
                            ;; keybiding for insert source code
                            (local-set-key (kbd "C-c s") 'org-insert-src-block)))

(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)
                                                         (latex . t)
                                                         (dot . t)
                                                         ;;(jupyter . t)
                                                         ))
