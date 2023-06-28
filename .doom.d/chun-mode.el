(defcustom chun-mode/keymap-prefix "C-c C-."
  "The prefix for chun-mode key bindings."
  :type 'string
  :group 'chun)

(defcustom chun-mode/org-roam-dir "~/OneDrive/org-roam"
  "The org-roam cloud directory."
  :type 'string
  :group 'chun)

(defcustom chun-mode/doom-config-dir "~/.doom.d"
  "The doom-emacs .doom.d directory."
  :type 'string
  :group 'chun)

(defcustom chun-mode/yas-snippets-dirs '("~/project/yas-snippets")
  "The yas-snippets directory."
  :type '(restricted-sexp :tag "Vector"
                          :match-alternatives
                          (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs))))
  :group 'chun)

(defcustom chun-mode/projectile-dirs '("~/project/pscore"
                                         "~/centra/info_center"
                                         "~/project/emacs-dev"
                                         "~/project/algo-trading")
  "The yas-snippets directory."
  :type '(restricted-sexp :tag "Vector"
                          :match-alternatives
                          (lambda (xs) (and (vectorp xs) (seq-every-p #'stringp xs))))
  :group 'chun)

(define-minor-mode chun-mode
  "Toggles chun mode"
  nil
  :global t
  :group 'chun
  :lighter " chun")

(defvar chun-mode/--open-chrome "
tell application \"Google Chrome\" to tell active tab of window 1
        activate
end tell
"
  "The applescript to open Chrome.")

(defvar chun-mode/--open-application-tpl "
tell application \"%s\"
        activate
end tell
"
  "The applescript template to open an application.")

(defvar chun-mode/--site-url-dic '()
  "The web bookmarks.
An alist of (title . url)
"
  )


(defvar chun-mode/--application-candidates
  '("Chrome" "Infoflow" "iTerm" "TIDAL" "Google")
  "The application cadidates")

(defun chun-mode/update-web-bookmarks()
  (interactive)
  (chun-mode/--update-web-bookmarks))

(defun chun-mode/--update-web-bookmarks ()
  "Update the url list from the bookmarks.org"
  ;; clear the dic
  (setq chun-mode/--site-url-dic '())

  (let* ((bookmarks-file-path (concat chun-mode/org-roam-dir "/20210921113038-bookmarks.org")))
    (save-current-buffer
      (set-buffer (find-file-noselect bookmarks-file-path))
      (let* ((parsetree (org-element-parse-buffer))
             (counter 0))
        (org-element-map parsetree 'link
          (lambda (link)
            (let* ((plist (nth 1 link))
                   (content (buffer-substring-no-properties (plist-get plist :contents-begin)
                                                            (plist-get plist :contents-end)))
                   (type (plist-get plist :type))
                   (path (plist-get plist :path))
                   (is-url nil)
                   )
              (setq is-url (or (string= type "https") (string= type "http")))
              (when is-url
                  (setq path (format "%s:%s" type path))
                  (add-to-list 'chun-mode/--site-url-dic `(,content . ,path))
                  (message "content: %S" content))
                  (message "site-url-dic: %S" chun-mode/--site-url-dic))
              link
              )))))
  ;; (with-output-to-temp-buffer "*chun-mode*"
  ;;   (print (format "Load %d bookmarks!" (length chun-mode/--site-url-dic))))
  (message (format "Load %d bookmarks!" (length chun-mode/--site-url-dic))))

;; (chun-mode/--update-web-bookmarks)

(defun chun-mode/--process-open-chrome (app)
  "Check if the app is Chrome and open chrome.
Returns t or nil
"
  (if (string-equal app "Chrome")
      (progn (do-applescript chun-mode/--open-chrome)
             t
             )))

(defun chun-mode/--process-open-google (app)
  "Open chrome and launch google"
  (if (string-equal app "Google")
      (progn (chun-mode/--google-this)
             t
             )))

(defun chun-mode/--process-other-application (app)
  "Open application"
  (if (member app chun-mode/--application-candidates)
        (progn (do-applescript (format chun-mode/--open-application-tpl app)) t)))

(defun chun-mode/--process-open-site (app)
  "open a site"
  (let* ((url (assoc app chun-mode/--site-url-dic)))
    (if url
        (progn (chun-mode/--chrome-browse-url (cdr url))
               t
               ))))

(defun chun-mode/open-application_ (app)
  "Open an application method."
  (let* ((app-launcher-list
          '(chun-mode/--process-open-chrome
            chun-mode/--process-open-google
            chun-mode/--process-other-application
            chun-mode/--process-open-site
            ))
         (i 0))
    (while (and (< i (length app-launcher-list))
                (not (funcall (nth i app-launcher-list) app)))
      (progn
        (message "try %d-th application" i)
        (incf i)))))


(defun chun-mode/open-application ()
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
             (app-candidates chun-mode/--application-candidates)
             (site-candidates (mapcar 'car chun-mode/--site-url-dic))
             )
        (ivy-read "Open application: " (append app-candidates site-candidates)
                  :action (lambda (app)
                            (chun-mode/open-application_ app))

                  :unwind (lambda ()
                            (delete-frame)
                            (other-window 1)))))))

(require 'subr-x)
(defun chun-mode/--google-this ()
  (let* (
         (query (read-string "Google it: "))
         (tokens (split-string query))
         (query-word (string-join tokens "+"))
         (base-url "https://google.com")
         (url (format "%s/search?q=%s" base-url query-word))
         (chrome-applescript "
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

(defun chun-mode/--chrome-browse-url (url)
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
    (do-applescript (format chrome-applescript url))
    ))

(map! :leader
      :desc "Open an application"
      "c c"
      #'chun-mode/open-application)


(provide 'chun-mode)
