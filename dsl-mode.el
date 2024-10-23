(defvar pimacs-mode-syntax-table nil "Syntax table for `pimacs-mode'.")

(setq pimacs-mode-syntax-table
      (let ((syntax-table (make-syntax-table)))
        ;; Python-style comment: “# ...”
        (modify-syntax-entry ?# "<" syntax-table)
        (modify-syntax-entry ?\n ">" syntax-table)
        syntax-table))

(defvar pimacs-font-lock-keywords
  (let* (
         ;; define several category of keywords
         (x-keywords '("class" "def" "return" "var" "let"
                       "if" "elif" "else"
                       "while"
                       "for" "in"
                       "with"
                       ))
         (x-types '("Int" "Str" "Float" "Bool" "Lisp"))
         (x-constants '("nil" "true" "false" "self"))
         (x-events '("__init__" "__add__"))
         (x-functions '("print"))
         (x-decorators '("@[a-zA-Z_][a-zA-Z0-9_]*"))

         ;; create the regex string for each category of keywords
         (x-keywords-regexp (regexp-opt x-keywords 'words))
         (x-types-regexp (regexp-opt x-types 'words))
         (x-constants-regexp (regexp-opt x-constants 'words))
         (x-events-regexp (regexp-opt x-events 'words))
         (x-functions-regexp (regexp-opt x-functions 'words))
         (x-decorators-regexp (mapconcat 'identity x-decorators "\\|")))

    `(
      (,x-keywords-regexp . font-lock-keyword-face)
      (,x-types-regexp . font-lock-type-face)
      (,x-constants-regexp . font-lock-constant-face)
      (,x-events-regexp . font-lock-builtin-face)
      (,x-functions-regexp . font-lock-function-name-face)
      (,x-decorators-regexp . font-lock-preprocessor-face)
      ("\\<var\\s-+\\([a-zA-Z_][a-zA-Z0-9_\\-]*\\)\\>" . font-lock-variable-name-face)
      ("def\\s-+\\([a-zA-Z_][a-zA-Z0-9_\\-]*\\)\\s-*(.*):"
       (1 font-lock-function-name-face))
      ("class\\s-+\\([a-zA-Z_][a-zA-Z0-9_\\-]*\\)\\s-*:"
       (1 font-lock-type-face))
      )))

(defun dsl-indent-line ()
  "Indent current line of DSL code."
  (interactive)
  (let ((not-indented t) (current-indentation 0))
    (if (bobp)
        (setq current-indentation 0)
      (save-excursion
        (while not-indented
          (forward-line -1)
          (if (looking-at "^[ \t]*$")  ; Check if the line is blank
              (setq not-indented nil)
            (if (looking-at ".*:$")  ; Check if the line ends with :
                (setq current-indentation (+ (current-indentation (if (boundp 'standard-indent) standard-indent 4)))
                  (setq not-indented nil))
              (setq not-indented nil)))))
    (indent-line-to current-indentation)))

(define-derived-mode pimacs-mode prog-mode "pimacs"
  "Major mode for editing pimacs (similar to Python)."
  (setq-local indent-line-function 'dsl-indent-line)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq font-lock-defaults '((pimacs-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set-syntax-table pimacs-mode-syntax-table))

;; Bind the TAB key to the indentation function
(define-key pimacs-mode-map (kbd "TAB") 'dsl-indent-line)

(provide 'pimacs-mode)

(add-to-list 'auto-mode-alist '("\\.pis\\'" . pimacs-mode))
