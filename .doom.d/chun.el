(defun chun/--current-directory ()
  "Get the cuurent directory"
  ;; (if (current-buffer)
  ;;     (file-name-directory (buffer-file-name (current-buffer)))
  ;;   default-directory)
  default-directory)

(require 'f)
(defun chun/--join-paths (path0 path1)
  "Join two components to a path"
  (f-join path0 path1))

(defun chun/--dir-name (path)
  "Get the directory name."
  (f-dirname path))

(defun chun/--abs-path (file &rest direcotry)
  "Get the absolute file path"
  (if direcotry (expand-file-name file direcotry)
    (expand-file-name file)))

(defun chun/--create-empty-file (path)
  "Create an empty file."
  (write-region "" nil path))

(defun chun/--create-empty-file--safely (path)
  "Create an empty file with prompt to ensure."
  (let* ((to-continue (string= "y" (read-string "want to create file %s ? [y/n]" path))))
    (if to-continue (chun/--create-empty-file path))))

;; c++ create new class
(require 'helm)
(defun chun/cpp/--generate-upper-directories (path)
  "Get a list of paths

/a/b/c
/a/b
/a
/
"
  (let* ((dir path)
         (dirs '()))
    (while (not (string= dir "/"))
      (setq dir (chun/--join-paths dir ".."))
      (message "dir: %s" dir)
      (push dir dirs))
    ;; make the longest path the first
    (reverse dirs)))

(defun chun/cpp/new-class-files ()
  "Create a class_name.h and class_name.cc files."
  (interactive)
  (let* ((mydirs (chun/cpp/--generate-upper-directories (chun/--current-directory)))
         (dir "") class-name header-path source-path)
    (setq dir (helm :sources (helm-build-sync-source "test"
                               :candidates mydirs
                               :fuzzy-match t)
                    :buffer "*helm test*"))
    (message "selected: %s" dir)
    (setq class-name (read-string "class name:"))
    (message "class name: %s" class-name)
    (setq header-path (format "%s/%s.h" dir class-name))
    (setq source-path (format "%s/%s.cc" dir class-name))

    ;; create empty files
    (message "to create header: %s" header-path)
    (message "to create source: %s" source-path)
    ;; (chun/--create-empty-file header-path)
    ;; (chun/--create-empty-file source-path)
    (chun/--create-empty-file--safely header-path)
    (chun/--create-empty-file--safely source-path)))

(require 'elisp-format)
(defun chun/format-elisp-buffer ()
  "Reformat the emacs lisp code."
  (interactive)
  (elisp-format-buffer)
  (delete-trailing-whitespace))


;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile (assert (string= (chun/--join-paths "a" "b") "a/b")))

(eval-when-compile (assert (string= (chun/--join-paths "/a/b/" "../b") "/a/b")))

(eval-when-compile (message "current directory: %S" (chun/--current-directory)))

;; (eval-when-compile
;;   (message "abs path: %s" (chun/--abs-path "../a")))

(eval-when-compile (assert (string=  (chun/--dir-name "a/b/c.txt") "a/b/")))

(eval-when-compile (message "dirs: %S" (chun/cpp/--generate-upper-directories "/a/b/c/d.txt")))
;; (eval-when-compile
;;   (chun/cpp/new-class-files))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
