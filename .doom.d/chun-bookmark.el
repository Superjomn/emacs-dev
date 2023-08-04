;; A minor mode for managing web bookmarks in org-mode
;;
;;
;;

(define-minor-mode chun-bookmark "Toggles chun mode"
  nil
  :global t
  :group 'chun-bookmark
  :lighter " chun-bookmark")

(defcustom chun-bookmark-file-path ""
  "The path to the org-mode file holding all the web bookmarks."
  :type 'string
  :group 'chun-bookmark)

(defvar chun-bookmark/--site-url-dic '()
  "The web bookmarks.
An alist of (title . url)
")

(defun chun-bookmark/update-web-bookmarks ()
  "Update the url list from the bookmarks.org"
  (interactive)
  ;; clear the dic
  (setq chun-bookmark/--site-url-dic '())
  (let* ()
    (save-current-buffer (set-buffer (find-file-noselect chun-bookmark-file-path))
                         (let* ((parsetree (org-element-parse-buffer))
                                (counter 0)
                                (all-link-desc (retrieve-org-links chun-bookmark-file-path)))
                           (dolist (link all-link-desc)
                             (let* ((url (car link))
                                    (desc (cdr link)))
                               (add-to-list 'chun-bookmark/--site-url-dic `(,desc . ,url)))))))
  (message (format "Load %d bookmarks!" (length chun-bookmark/--site-url-dic))))


(defun chun-bookmark/open-site ()
  (interactive)
  (with-current-buffer (get-buffer-create "*modal-ivy*")
    (let ((frame (make-frame '((auto-raise . t)
                               (background-color . "DeepSkyBlue3")
                               (cursor-color . "MediumPurple1")
                               (font . "Menlo 20")
                               (foreground-color . "#999999")
                               (height . 20)
                               (internal-border-width . 20)
                               (left . 0.33)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (minibuffer . only)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (top . 48)
                               (undecorated . t)
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)
                               (width . 110)))))
      (set-face-attribute 'ivy-minibuffer-match-face-1 frame
                          :background nil
                          :foreground nil)
      (set-face-attribute 'ivy-minibuffer-match-face-2 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-minibuffer-match-face-3 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-minibuffer-match-face-4 frame
                          :background nil
                          :foreground "orange1")
      (set-face-attribute 'ivy-current-match frame
                          :background "#ffc911"
                          :foreground "red")
      (set-face-attribute 'minibuffer-prompt frame
                          :foreground "grey")
      (let* ((ivy-height 20)
             (ivy-count-format "")
             (site-candidates (mapcar 'car chun-bookmark/--site-url-dic)))
        (ivy-read "Open url: " site-candidates
                  :action (lambda (app)
                            (chun-bookmark/--process-open-site app))
                  :unwind (lambda ()
                            (delete-frame)
                            (other-window 1)))))))

(with-eval-after-load 'ivy (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
                      (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
                      (define-key ivy-minibuffer-map (kbd "C-g") 'keyboard-escape-quit)
                      (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
                      )

(defun chun-bookmark/--chrome-browse-url (url)
  "Open Chrome and go to the url"
  (message "chrome-browse-url %s" url)
  (let* ((chrome-applescript "
tell application \"Google Chrome\"
    if it is running then
        activate
        open location \"%s\"
        delay 1
        activate
    end if
end tell
"))
    (do-applescript (format chrome-applescript url))))

(defun chun-bookmark/--process-open-site (app)
  "open a site"
  (let* ((url (assoc app chun-bookmark/--site-url-dic)))
    (if url (progn (chun-bookmark/--chrome-browse-url (cdr url)) t))))

(map! 
 :leader
 :desc "Open an application"
 "c c" #'chun-bookmark/open-site)


(provide 'chun-bookmark)
