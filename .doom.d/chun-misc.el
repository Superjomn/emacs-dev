(defun chun/insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))

(defun chun/--set-inline-code-wrapper ()
  (let* (start
         end
         (space "[?\s?\n?,]"))
    (when (re-search-forward space nil t)
      (setq end (point))
      (backward-char 1)
      (when (re-search-backward space nil t)
        (setq start (point))
        (goto-char start)
        (forward-char 1)
        (insert "~")
        (goto-char end)
        (insert "~")))))

(defun chun/wrap-inline-code ()
  (interactive)
  (chun/--set-inline-code-wrapper))

(after! org-mode
  (add-hook 'org-mode
            (lambda () (local-set-key (kbd "C-x C-w") #'chun/wrap-inline-code))))


(defalias 'chun/remove-file 'crux-delete-buffer-and-file)

(defalias 'chun/find-recent-file 'crux-recentf-find-file)

(defun chun/get-file-name ()
  "Get the filename without directory to clipboard."
  (interactive)
  (kill-new
   (file-name-nondirectory (buffer-file-name))))

(defun --chun-cur-dir ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))
