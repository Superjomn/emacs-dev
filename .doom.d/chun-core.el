;; This is a file for core functions that are used in multiple places.
;;

;; string-related functions
;;
(defun chun-str--join (separator strings)
  "Join a list of strings with a separator.
Inputs:
  - separator (string): the separator to join the strings.
  - strings (list of strings): the strings to join.
Returns:
  - (string): the joined string.
"
  (mapconcat 'identity strings separator))

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




(provide 'chun-core)
