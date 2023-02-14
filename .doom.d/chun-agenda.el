(require 'subr-x)

;; ref https://systemcrafters.net/emacs-from-scratch/organize-your-life-with-org-mode/

(use-package! org-agenda
:bind
  ("C-c o a" . org-agenda)
  ("C-c x x" . org-capture)
:init
  (setq chun-agenda-inbox-path (concat chun-mode/org-roam-dir "/20210807163552-agenda_inbox.org"))
  (setq chun-agenda-random-idea-path (concat chun-mode/org-roam-dir "/20221118080431-random_idea.org"))
  (setq chun-agenda-read-list-path (concat chun-mode/org-roam-dir "20230214102434-read_list.org"))
:custom
  (org-agenda-files '(chun-agenda-inbox-path chun-agenda-random-idea-path
                                                     chun-agenda-read-list-path))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d!)")
                       (sequence "IDEA(i)" "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCEL(k@)")))
:config
  (setq org-capture-templates `(("i" "inbox"
                                 entry
                                 (file chun-agenda-inbox-path)
                                 "* TODO
%?")
                                ("x" "idea"
                                 entry
                                 (file chun-agenda-random-idea-path)
                                 ,(string-join '("* IDEA %?" ":PROPERTIES:" ":ADDED-DATE: %U"
                                                 ":END:")
                                               "\n"))
                                ("l" "link"
                                 entry
                                 (file chun-agenda-read-list-path)
                                 "* TODO %(org-cliplink-capture)"
                                 :immediate-finish t)
                                ("r" "read"
                                 entry
                                 (file chun-agenda-read-list-path)
                                 "* TODO %?"))))
