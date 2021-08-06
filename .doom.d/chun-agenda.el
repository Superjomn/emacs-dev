;;; ../project/emacs-dev/.doom.d/chun-agenda.el -*- lexical-binding: t; -*-

;; https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                                   :file-path "refile\\.org")
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "Today's tasks"
                                   :file-path "journal/")
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Meetings"
                                   :and (:todo "MEET" :scheduled future)
                                   :order 10)
                            (:discard (:not (:todo "TODO")))))))))))
  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  :config
  (org-super-agenda-mode))


(setq org-agenda-prefix-format
      '(
        (agenda . "%-8:i %-8:c * ")
        (timeline . " % s")))

(setq org-agenda-files
      (list
       "~/OneDrive/org-roam/20210803124941-inference_2021_q3_enhancement_agenda.org"
       "~/OneDrive/org-roam/20210803112751-paddle_inference_agenda.org"))

(let ((org-agenda-span 'day)
      (org-super-agenda-groups
       '((:name "TODAY"
          :time-grid t
          :todo "TODAY"
          :face (:background "white" :underline t)
          :transformer (--> it
                            (upcase it)))
         (:name "Important"
          :priority>= "B")

         (:name "Others"
          :face (:background "white" :underline t)
          :not (:priority>= "B")
          :order 100))))
  (org-agenda nil "a"))
