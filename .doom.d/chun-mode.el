(defcustom chun-mode/keymap-prefix "C-c C-."
  "The prefix for chun-mode key bindings."
  :type 'string
  :group 'chun)

(defcustom chun-mode/org-roam-dir "~/OneDrive/org-roam"
  "The org-roam cloud directory."
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

(defun chun-mode/open-application_ ()
  "Open an application method."
  (if (string-equal app "Chrome")
      (do-applescript chun-mode/--open-chrome)
    (do-applescript (format chun-mode/--open-application-tpl app))
    ))


(defun chun-mode/open-application ()
  (interactive)
  (with-current-buffer (get-buffer-create "*modal-ivy*")
    (let ((frame (make-frame '((auto-raise . t)
                               (background-color . "DeepSkyBlue3")
                               (cursor-color . "MediumPurple1")
                               (font . "Menlo 20")
                               (foreground-color . "#eeeeec")
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
      (let ((ivy-height 20)
            (ivy-count-format ""))
        (ivy-read "Open application: " '("Chrome" "Infoflow" "iTerm" "TIDAL")
                  :action (lambda (app)
                            (chun-mode/open-application_))

                  :unwind (lambda ()
                            (delete-frame)
                            (other-window 1))
)))))

(require 'subr-x)
(defun chun-mode/google-this ()
  (let* (
         (query "hello world")
         (tokens (split-string query))
         (base-url "https://google.com")
         (url (format base-url "/search?q="))
         )))

(map! :leader
      :desc "Open an application"
      "c c"
      #'chun-mode/open-application)


(provide 'chun-mode)
