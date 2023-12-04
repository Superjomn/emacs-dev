(require 'subr-x)

;; ref https://systemcrafters.net/emacs-from-scratch/organize-your-life-with-org-mode/
;;
;;


(use-package! org-agenda
:bind
  ("C-c o a" . org-agenda)
  ("C-c x x" . org-capture)

:init
  ;; Seems the latest version need the 'org-directory`
  (setq org-directory chun-mode/org-roam-dir)
  (setq org-agenda-files '(
                           "20230629103055-agenda_inbox.org"
                           "20220408141044-book_or_paper_agenda.org"
                           ;; "20221017102352-english_agenda_inbox.org"
                           "20230324163903-random_ideas.org"
                           "20230214102434-read_list.org"
                           "20231204105512-writing_or_ideas_input_box.org"
                           ))

  (setq chun-agenda--inbox-path (concat chun-mode/org-roam-dir "/20230629103055-agenda_inbox.org"))
  (setq chun-agenda--paper-or-book-path (concat chun-mode/org-roam-dir "/20220408141044-book_or_paper_agenda.org"))
  (setq chun-agenda--english-inbox-path (concat chun-mode/org-roam-dir "/20221017102352-english_agenda_inbox.org"))
  (setq chun-agenda--random-idea-path (concat chun-mode/org-roam-dir "/20230324163903-random_ideas.org"))
  (setq chun-agenda--read-list-path (concat chun-mode/org-roam-dir "/20230214102434-read_list.org"))
  (setq chun-bookmark-path (concat chun-mode/org-roam-dir "/20210921113038-bookmarks.org"))
  (setq chun-anki-inbox-path (concat chun-mode/org-roam-dir "/20230608135539-anki_inbox.org"))
  (setq chun-blog-writing-input (concat chun-mode/org-roam-dir "/20231204105512-writing_or_ideas_input_box.org"))


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
                                ("it" "temp" entry (file chun-agenda--inbox-path) "* TODO %?")
                                ("ix" "idea"
                                 entry
                                 (file+headline chun-agenda--inbox-path "Ideas")
                                 ,(string-join '("* IDEA %?" ":PROPERTIES:" ":ADDED-DATE: %U"
                                                 ":END:")
                                               "\n"))
                                ("il" "Life" entry (file+headline chun-agenda--inbox-path "Life") "* TODO %? :life:")
                                ("ic" "Child" entry (file+headline chun-agenda--inbox-path "Life") "* TODO %? :life:baby:")

                                ;; menu for English related ;;
                                ("e" "English")

                                ("ea" "Anki" entry (file+headline chun-anki-inbox-path "Phrases")
                                 ,(string-join
                                 '("* %^{Title} :anki:"
                                 ":PROPERTIES:"
                                 ":ANKI_NOTE_TYPE: Basic (and reversed card)"
                                 ":ANKI_DECK: English-learn-org"
                                 ":END:"
                                 "** Front"
                                 "%(identity \"%^{Title}\")"
                                 "** Back"
                                 "%^{Body}\n")
                                 "\n"))

                                ("ev" "Word" entry (file+headline chun-agenda--english-inbox-path "Words")
                                 "* TODO %? :english:")
                                ("ep" "Phrase" entry (file+headline chun-agenda--english-inbox-path "Phrases")
                                                                "* TODO %? :english:")

                                ;; menu for Reading related ;;
                                ("r" "Read")
                                ("rl" "link"
                                 entry
                                 (file+headline chun-agenda--read-list-path "Web")
                                 "* TODO %(org-cliplink-capture)"
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
                                 "* TODO %?")

                                ))


  (setq org-agenda-prefix-format "%i %-2:c %-14t% s%-6e %/b ")
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

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
         ((org-agenda-compact-blocks t)))))



(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
