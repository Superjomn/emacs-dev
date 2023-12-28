(defcustom yabai-chrome-of-working-profile-suffix ""
  "The suffix of the Google Chrome title of working profile."
  :type 'string)

(defcustom yabai-chrome-of-incognito-profile-suffix "Google Chrome (Incognito)"
  "The suffix of the Google Chrome title of chatgpt."
  :type 'string
  )

(defcustom yabai-chrome-of-personal-profile-suffix ""
  "The suffix of the Google Chrome title of the personal profile."
  :type 'string
  )

(defcustom yabai-temporary-space 6
  "The space to hold all temporary windows")

;; (defcustom yabai-number-of-spaces 7
;;   "The maximum number of spaces.")

(defvar yabai-window-list-cache '()
  "The cache for window list")

(defun yabai-get-window-list-on-space (space)
  "Return a list of (app-name, window-id, title, space) for each window on the given space using yabai commands."
  (let ((output (shell-command-to-string
                 (format "yabai -m query --windows --space %d | jq -r '.[] | [.app, .id, .title .space] | @tsv'" space))))
    (mapcar (lambda (line)
              (split-string line "\t" t))
            (split-string output "\n" t))))

(defun yabai-get-window-list ()
  "Return all the windows of information of (app-name, window-id, title, space)."
  (let ((output (shell-command-to-string
                 (format "yabai -m query --windows | jq -r '.[] | [.app, .id, .title, .space] | @tsv'"))))
    (mapcar (lambda (line)
              (split-string line "\t" t))
            (split-string output "\n" t))))

(defun yabai-build-window-list-cache ()
  "Build the window list cache for helm service."
  (interactive)
  (let* ((window-list (yabai-get-window-list)))
    (setq yabai-window-list-cache (mapcar (lambda (x) (list* (yabai--get-key-for-window x)
                                                             x)) window-list))
    )
  (message "Update the window list cache with %d windows" (length yabai-window-list-cache)))

(defun yabai--get-key-for-window (window)
  "Create helm query key with window
Inputs:
 - window: the item type of `yabai-get-window-list'
Returns:
 - string
"
  (let* ((window-name (nth 0 window))
         (window-title (nth 2 window))
         )
    (if (string-equal window-name "Google Chrome")
        (cond ((yabai--chrome-is-personal-profile window-title)
               "Chrome - Personal")
              ((yabai--chrome-is-working-profile window-title)
               "Chrome - Working")
              ((yabai--chrome-is-incognito-profile window-title)
               "Chrome - Incognito")
              )
      (concat window-name " /// " window-title)
      )))

(defun yabai-switch-windows ()
  "Helm source for yabai windows."
  (interactive)
  (let ()
    (helm :sources (helm-build-sync-source "Yabai windows"
                     :candidates yabai-window-list-cache
                     :fuzzy-match helm-lisp-fuzzy-completion
                     :persistent-action #'helm-yabai-switch-to-window
                     :action #'helm-yabai-switch-to-window)
          :buffer "*helm yabai windows*")))

(defun helm-yabai-switch-to-window (window)
  "Switch to the yabai window specified by WINDOW."
  (message "get window: %S" window)
  (let ((id (nth 1 window)))
    (shell-command (format "yabai -m window --focus %s" id))))

;(yabai-get-app-list-on-space 1)
;(("Google Chrome" "8541" "配备 GPT-4 的必应聊天 - Google Chrome (Incognito)") ("Microsoft Outlook" "248" "Inbox • chunweiy@nvidia.com") ("Preview" "23646" "Jose Raul Capablanca - Chess Fundamentals-Nabu Press (2010).pdf – Page 7 of 65") ("Google Chrome" "454" "vllm/vllm/entrypoints/llm.py at 90979c38f87c17d53a7cd0eb430373ecb0b64b9a · vllm-project/vllm - Google Chrome - Chunwei Yan (nvidia.com)") ("NetEaseMusic" "217" "NetEaseMusic") ("WeChat" "8371" "WeChat (Chats)") ("TIDAL" "175" "TIDAL") ("BibDesk" "142" "papers.bib") ("Preview" "126" "Efficiently Scaling Transformer Inference.pdf – Page 14 of 18"))


(defun yabai-move-app-to-space (window-id space)
  "Move the given application window to the given space using yabai commands."
  (shell-command-to-string
   (format "yabai -m window %s --space %d" window-id space)))
;; (yabai-move-app-to-space 8541 1)

;; To get specific windows

(defun yabai--filter-app-by-name (app-list app-name)
  "Return a list of window ids for the given app name from the given app list."
  (seq-filter (lambda (app-info)
                (equal (car app-info) app-name))
              app-list))

;;
(defun yabai--chrome-is-working-profile (window-title)
  "window-title: str
Returns: bool
"
  (--string-endswith window-title yabai-chrome-of-working-profile-suffix))

(defun yabai--chrome-is-personal-profile (window-title)
  (--string-endswith window-title yabai-chrome-of-personal-profile-suffix))

(defun yabai--chrome-is-incognito-profile (window-title)
  (--string-endswith window-title yabai-chrome-of-incognito-profile-suffix))


(require 'subr-x)

(defun --string-endswith (string this)
  "Return t if string ends with this string, nil otherwise."
  (string-suffix-p this string))
