;; This is a file for core functions that are used in multiple places.
;;

;; string-related functions
;;
;;
(defun chun-str-split (string &optional sep)
  "Split STRING into a list of substrings using SEP as a delimiter.
If SEP is nil or empty, split on whitespace like Python's str.split()."
  (if (or (not sep) (string-empty-p sep))
      (split-string string)  ;; Default split on whitespace.
    (split-string string (regexp-quote sep)))) ;; Split by exact SEP.

(defun chun-str--join (separator strings)
  "Join a list of strings with a separator.
Inputs:
  - separator (string): the separator to join the strings.
  - strings (list of strings): the strings to join.
Returns:
  - (string): the joined string.
"
  (mapconcat 'identity strings separator))

(defun chun-str--trim (string)
  "Remove leading and trailing whitespace from STRING."
  (string-trim string))

;; org-element related utilites
;;
(defun chun-org--get-hierarchical-title (hl dilimiter &optional file-title)
  "Get the hierarchical title of the current headline.
Inputs:
  - hl (org-element): the org-element of the current headline.
  - dilimiter (string): the dilimiter to separate the titles.
  - file-title (string): the title of the file.

Returns:
  - (string): the hierarchical title of the current headline.
"
  (let* ((title (org-element-property :raw-value hl))
         (parent (org-element-property :parent hl))
         (titles (list title))
         )

    (while parent
      (push (org-element-property :raw-value parent) titles)
      (setq parent (org-element-property :parent parent)))

    (if file-title (push file-title titles))

    (chun-str--join dilimiter titles)))


(defun chun-org--get-file-title (file)
  "Get the title of the org file.

Inputs:
  - file (string): the path to the org file.

Returns:
  - (string): the title of the org file.
"

  (with-temp-buffer
    (insert-file-contents file)
    (chun-org--get-file-title-from-buffer))
  )

(defun chun-org--get-file-title-from-buffer ()
  "Get the title of the org file from the current buffer.

Returns:
  - (string): the title of the org file.
"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
        (match-string 1)
      (buffer-name))))

;; org-roam related

(require 'org-roam)
(defun chun-org-roam--get-files-with-tag (tag)
  "Return a list of org-roam files that have the specified TAG."
  (let* ((file-lists
          (org-roam-db-query
           [:select [file]
            :from tags
            :inner :join nodes
            :on (= tags:node-id nodes:id)
            :where (= tags:tag $s1)]
           tag))
         files '()
         )
    (dolist (file-list file-lists)
      (push (car file-list) files))
    files
    ))

(provide 'chun-core)
