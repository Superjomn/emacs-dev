;; This is a plugin to hold all the org files and navigate to a specific file using helm on titles.
;; The general workflow is
;; 1. Set project directories
;; 2. Scan the all the titles of the org files, and hold it as a helm database
;; 3. navigate with helm

(defcustom chun-project--dirs "directories to scan" '()
  :type 'list
  :group 'chun-project)

(defvar chun-project--org-files '() "paths of the org files.")
(defvar chun-project--titles '() "titles of the org files.")
(defvar chun-project--title-to-path-dic nil "mapping from title to path.")


(defun chun-project--get-title-from-org-file (path)
  "Get the title from an org file.
path: string
"
  (let* ()
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$" nil t)
          (match-string 1)
        (warn "%s file does not have a title, using %s as the title" path path)
        path))))

(defun chun-project--get-titles-from-directory (dir)
  "Get the titles from the files in a directory."
  (message "dir: %S" dir)
  (let* ((org-files (directory-files (expand-file-name dir) nil "\\.org$"))
         (org-paths (mapcar (lambda (file) (concat dir "/" file)) org-files))
         (org-titles (mapcar (lambda (path) (chun-project--get-title-from-org-file path)) org-paths)))
    `(,org-paths . ,org-titles)))

(defun chun-project--cache-add-dir (dir)
  "Update the org-files and titles cache from a directory.
NOTE: It simply append new records to the list, so an external reset is nessary.
"
  (let* ((res (chun-project--get-titles-from-directory dir))
         (files (car res))
         (titles (cdr res)))
    (message "files: %S" files)
    (message "titles: %S" titles)
    (message "to show appended")
    (setq chun-project--org-files (append chun-project--org-files files))
    (setq chun-project--titles (append chun-project--titles titles))))

(defun chun-project--reset-dic (files titles)
  "Update the `chun-project--title-to-path-dic'."
  (cl-assert (eq (length files) (length titles)))
  (setq chun-project--title-to-path-dic (ht-create))
  (let* ((i 0)
         (n (length files))
         title file)
    (while (< i n)
      (setq file (nth i files))
      (setq title (nth i titles))
      (ht-set! chun-project--title-to-path-dic title file)
      (incf i))))

(defun chun-project-update-cache ()
  (interactive)
  (progn
    (setq chun-project--org-files '())
    (setq chun-project--titles '())
    (mapcar (lambda (dir)
              (chun-project--cache-add-dir dir)) chun-project--dirs)
    (message "chun-project updated %d items" (length chun-project--titles))
    (chun-project--reset-dic chun-project--org-files chun-project--titles)))

(defun chun-project-search-title ()
  (interactive)
  "Goto an org file with a specific title."

  (ivy-read "Goto file: " chun-project--titles
            :action (lambda (title)
                      (let* ((path (ht-get chun-project--title-to-path-dic title)))
                        (switch-to-buffer (find-file-noselect path))))))
