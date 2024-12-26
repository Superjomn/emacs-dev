(require 'org)
(require 'helm)
(require 'seq)
(require 'chun-core)

(defvar chun-memo-files (chun-org-roam--get-files-with-tag "memo")
  "List of org-mode files to search for memo entries.")

(defcustom chun-memo-key-enable-headline-hierarchy t
  "If non-nil, include all parent headlines in the key for memo entries."
  :type 'boolean
  :group 'chun-memo)

(defcustom chun-memo-key-concat-delimiter " / "
  "Delimiter for concatenating headline hierarchy in memo keys."
  :type 'string
  :group 'chun-memo)

(defvar chun-memo-db nil
  "Global list to store memo entries as key-value pairs.")

(defun chun-memo-parse-file (file)
  "Parse FILE for memo-tagged headlines and store in `chun-memo-db`."
  (with-current-buffer (find-file-noselect file)
    (org-mode)
    (let*
        ((file-title (chun-memo--org-get-title file)))
        (message "file-title: %S" file-title)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (when (member "memo" (org-element-property :tags hl))
              ;;(message "hl: %S, title: %S" hl (org-element-property :raw-value hl))
              (let* ((title (org-element-property :raw-value hl))
                     (content (buffer-substring-no-properties
                               (org-element-property :contents-begin hl)
                               (org-element-property :contents-end hl)))
                     ;;(titles (list file-title title))
                     (key title))
                (setq content (chun-str--trim content))
                (when chun-memo-key-enable-headline-hierarchy
                  (let ((parent (org-element-property :parent hl))
                        (titles (list title)))
                    (while parent
                      (push (org-element-property :raw-value parent) titles)
                      (setq parent (org-element-property :parent parent)))
                    (push file-title titles)
                    ;; remove all the nil items
                    (setq titles (seq-filter 'identity titles))
                    (setq key
                          (mapconcat 'identity titles chun-memo-key-concat-delimiter))))
                (push (cons key content) chun-memo-db))))))))

(defun chun-memo--org-get-title (file)
  "Get the title of the current org-mode file."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t)
        (match-string 1)
      nil)))

(defun chun-memo-build-db ()
  "Build the memo database by parsing files in `chun-memo-files`."
  (interactive)
  (setq chun-memo-db nil)
  (dolist (file chun-memo-files)
    (chun-memo-parse-file file))
  ;;(message "build db with %d items" (length chun-memo-db))
  )

(defun chun-memo-query ()
  "Query the memo database with Helm and copy the selected entry to the clipboard."
  (interactive)
  (if (= (length chun-memo-db) 0)
      (chun-memo-build-db))

  (helm :sources (helm-build-sync-source "Chun Memo Query"
                   :candidates (mapcar #'car chun-memo-db)
                   :action (lambda (key)
                             (kill-new (cdr (assoc key chun-memo-db)))))
        :buffer "*chun-memo-query*"))

(provide 'chun-memo)
