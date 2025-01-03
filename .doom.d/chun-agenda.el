(require 'subr-x)
(require 'json)

;; ref https://systemcrafters.net/emacs-from-scratch/organize-your-life-with-org-mode/
;; ref https://webusers.i3s.unice.fr/~malapert/emacs_orgmode.html
;;

(use-package! org-agenda
:bind
  ("C-c o a" . org-agenda)
  ("C-c x x" . org-capture)

:init
  ;; Seems the latest version need the 'org-directory`
  (setq org-directory chun-mode/org-roam-dir)
  (setq-default org-agenda-files '(
                           ;; "20230629103055-agenda_inbox.org"
                           "20220408141044-book_or_paper_agenda.org"
                           ;; "20221017102352-english_agenda_inbox.org"
                           "20230324163903-random_ideas.org"
                           "20230214102434-read_list.org"
                           "20231204105512-writing_or_ideas_input_box.org"
                           "20241027152311-work_agenda.org"
                           "20241106091348-personal_agenda_inbox.org"
                           ))

  (setq chun-agenda--inbox-path (concat chun-mode/org-roam-dir "/20230629103055-agenda_inbox.org"))
  (setq chun-agenda--paper-or-book-path (concat chun-mode/org-roam-dir "/20220408141044-book_or_paper_agenda.org"))
  (setq chun-agenda--english-inbox-path (concat chun-mode/org-roam-dir "/20221017102352-english_agenda_inbox.org"))
  (setq chun-agenda--random-idea-path (concat chun-mode/org-roam-dir "/20230324163903-random_ideas.org"))
  (setq chun-agenda--read-list-path (concat chun-mode/org-roam-dir "/20230214102434-read_list.org"))
  (setq chun-work-inbox-path (concat chun-mode/org-roam-dir "/20241027152311-work_agenda.org"))
  (setq chun-bookmark-path (concat chun-mode/org-roam-dir "/20210921113038-bookmarks.org"))
  (setq chun-anki-inbox-path (concat chun-mode/org-roam-dir "/20230608135539-anki_inbox.org"))
  (setq chun-blog-writing-input (concat chun-mode/org-roam-dir "/20231204105512-writing_or_ideas_input_box.org"))
  (setq chun-agenda-misc-info-path (concat chun-mode/org-roam-dir "/20240612100701-info_inbox.org"))
  (setq chun-personal-inbox-path (concat chun-mode/org-roam-dir "/20241106091348-personal_agenda_inbox.org"))

;; (custom-set-faces '(org-todo ((t
;;                                  (:foreground "red"
;;                                   :weight bold))))
;;                     '(org-doing ((t
;;                                   (:foreground "yellow"
;;                                    :weight bold))))
;;                     '(org-done ((t
;;                                  (:foreground "grey"
;;                                         :weight bold))))
;;                     ;; Add more faces for other keywords as needed
;;                     )
;; (setq org-todo-keyword-faces
;;       '(("TODO" . org-todo)
;;         ("DOING" . org-doing)
;;         ("DONE" . org-done)))

:custom ; execute code after a package is loaded
  ;; (org-agenda-files '(
  ;;                     chun-agenda--inbox-path
  ;;                     chun-agenda--paper-or-book-path
  ;;                     chun-agenda--english-inbox-path
  ;;                     chun-agenda-random-idea-path
  ;;                     chun-agenda--read-list-path))

  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IDEA"  "|" "DONE(d!)")
                       (sequence "READY(r)" "DOING(i)" "WAIT(w@/!)" "HOLD(h@)" "|" "CANCELLED(c@)")))

:config
  (setq org-capture-templates `(

                                ;; menu for inbox ;;
                                ("i" "Inbox")
                                ;;("it" "Temp" entry (file chun-agenda--inbox-path) "* TODO %?")
                                ("it" "Work" entry (file chun-work-inbox-path) ,(string-join '("* TODO %? :work:" ":PROPERTIES:" ":ADDED-DATE: %U"
                                                 ":END:")
                                               "\n"))
                                ;; ("ix" "idea"
                                ;;  entry
                                ;;  (file+headline chun-agenda--inbox-path "Ideas")
                                ;;  ,(string-join '("* IDEA %?" ":PROPERTIES:" ":ADDED-DATE: %U"
                                ;;                  ":END:")
                                ;;                "\n"))
                                ("ix" "Personal" entry (file chun-personal-inbox-path) ,(string-join '("* TODO %? :agenda:personal:" ":PROPERTIES:" ":ADDED-DATE: %U"
                                                 ":END:")
                                               "\n"))
                                ("il" "Life" entry (file+headline chun-agenda--inbox-path "Life") "* TODO %? :life:")
                                ("ic" "Child" entry (file+headline chun-agenda--inbox-path "Life") "* TODO %? :life:baby:")

                                ("if" "Info Misc" entry (file chun-agenda-misc-info-path) "* TODO %?")

                                ;; menu for English related ;;
                                ("e" "English")

                                ;; ("ea" "Anki" entry (file+headline chun-anki-inbox-path "Phrases")
                                ;;  ,(string-join
                                ;;  '("* %^{Title} :anki:"
                                ;;  ":PROPERTIES:"
                                ;;  ":ANKI_NOTE_TYPE: Basic (and reversed card)"
                                ;;  ":ANKI_DECK: English-learn-org"
                                ;;  ":END:"
                                ;;  "** Front"
                                ;;  "%(identity \"%^{Title}\")"
                                ;;  "** Back"
                                ;;  "%^{Body}\n")
                                ;;  "\n"))

                                ("ev" "Word" entry (file+headline chun-agenda--english-inbox-path "Words")
                                 "* TODO %? :english:")
                                ("ep" "Phrase" entry (file+headline chun-agenda--english-inbox-path "Phrases")
                                                                "* TODO %? :english:")
                                ("eo" "Pronunciation" entry (file+headline chun-agenda--english-inbox-path "Pronunciation")
                                                                "* TODO %? :english:")

                                ("ea" "Abbreviation" entry (file+headline chun-agenda--english-inbox-path "Abbreviation")
                                                                "* TODO %? :english:")

                                ;; menu for Reading related ;;
                                ("r" "Read")
                                ;; ("rl" "link without proxy"
                                ;;  entry
                                ;;  (file+headline chun-agenda--read-list-path "Web")
                                ;;  "* TODO %(org-cliplink-capture)"
                                ;;  :immediate-finish t)
                                ("rl" "link through proxy"
                                 entry
                                 (file+headline chun-agenda--read-list-path "Web")
                                 "* TODO %(chun-org-insert-link-smart-str)"
                                 :immediate-finish t)

                                ("rp" "Paper"
                                 entry
                                 (file+headline chun-agenda--read-list-path "Paper")
                                 "* TODO %?")

                                ("rc" "Concept"
                                 entry
                                 (file+headline chun-agenda--read-list-path "Concept")
                                 "* TODO %?")

                                ("rs" "Sharing"
                                 entry
                                 (file+headline chun-agenda--read-list-path "Sharing")
                                 "* TODO %?")

                                ("b" "Bookmark")
                                ("bl" "Link"
                                 entry
                                 (file+headline chun-bookmark-path "misc")
                                 "* %(org-cliplink-capture)"
                                 :immediate-finish t)

                                ("bt" "Text"
                                 entry
                                 (file+headline chun-bookmark-path "misc")
                                 "* [[%(current-kill 0 t)][%?]]")

                                ;; menu for writing related
                                ("w" "Writing")
                                ("wb" "Blog"
                                 entry
                                 (file+headline chun-blog-writing-input "Blog")
                                 "* TODO %?")))


  (setq org-agenda-prefix-format "%i %-2:c %-14t% s%-6e %/b ")
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  )

;; This changes the tags' appearance in org-mode file only.
(setq org-tag-faces
      '(("work" . (:foreground "blue" :weight bold))
        ("personal" . (:foreground "green" :weight bold))
        ("health" . (:foreground "orange" :weight bold))))

(setq org-agenda-tag-faces
      '(("work" . (:foreground "blue" :weight bold))
        ("personal" . (:foreground "green" :weight bold))
        ("health" . (:foreground "orange" :weight bold)))
      )


(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


;; enable org-habit
(add-to-list 'org-modules 'org-habit t)

;; enable state logging
(setq org-treat-insert-todo-heading-as-state-change t)
(setq org-log-into-drawer t)

;; borrowed from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(setq org-agenda-custom-commands
      '(("c" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))
        ("r" "Recently Added Items in Two Weeks"
           alltodo ""
           ((org-agenda-skip-function
             (lambda ()
               (org-agenda-skip-if-not-recent 3)))
            (org-agenda-overriding-header "Recently Added Items in Two Weeks")
            ))))



(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; Speed exporting by disable eval the code blocks
(setq org-babel-default-header-args '((:eval . "never-export")))

(setq org-html-table-default-attributes
      '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))


(defcustom chun-agenda-view-html-path
  "agenda.html"
  "The html path to export org-agenda view"
  :type 'string
  :group 'chun-agenda)

(defun chun-export-agenda-view-to-html ()
  "Export the specified Org Agenda view to an HTML file."
  (interactive)
  (save-excursion
    (call-interactively 'org-agenda)
    (org-agenda-write chun-agenda-view-html-path)
    (call-interactively 'org-agenda-quit)
    (message (format "Exported agenda to %s" chun-agenda-view-html-path))))


;; Change the style of the date headlines in agenda view.
(set-face-attribute 'org-agenda-date nil
                    :foreground "blue"
                    :background "grey"
                    :weight 'bold)

(set-face-attribute 'org-agenda-date-today nil
                    :foreground "red"
                    :background "grey"
                    :weight 'bold)

(defun org-agenda-skip-if-not-recent (n-weeks)
  "Skip entries that do not have an :ADDED-DATE: property within the last N weeks."
  (let* ((added-date (org-entry-get (point) "ADDED-DATE"))
         (date (and added-date (org-time-string-to-time added-date)))
         (cutoff (time-subtract (current-time) (days-to-time (* n-weeks 7)))))
    (if (or (not date) (time-less-p date cutoff))
        (org-end-of-subtree t)
      nil)))


;; dedicated workspace for agenda
;;
(defun chun-agenda/org-agenda-in-workspace ()
  "Open org-agenda in a dedicated workspace."
  (interactive)
  (let ((agenda-workspace-name "Agenda"))
    (unless (+workspace-exists-p agenda-workspace-name)
      (+workspace/new agenda-workspace-name))
    (+workspace-switch agenda-workspace-name)
    ;; Clear exising buffers
    ;;(dolist (buf (+workspace-buffer-list agenda-workspace-name))
      ;;(kill-buffer buf))
    (org-agenda nil "c")))
