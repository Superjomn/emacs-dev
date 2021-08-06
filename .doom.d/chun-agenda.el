;;; ../project/emacs-dev/.doom.d/chun-agenda.el -*- lexical-binding: t; -*-
;;;
;;;

;; (use-package! org-superstar
;;   :init
;;   ;; Every non-TODO headline now have no bullet
;;   (setq org-superstar-headline-bullets-list '("\u200b"))
;;   (setq org-superstar-leading-bullet "\u200b")
;;   (setq org-superstar-item-bullet-alist
;;         '((?* . ?•)
;;           (?+ . ?➤)
;;           (?- . ?•)))
;;   ;; Enable custom bullets for TODO items
;;   (setq org-superstar-headline-bullets-list '(?\s))
;;   (setq org-superstar-special-todo-items t)
;;   (setq org-superstar-remove-leading-stars nil)
;;   (setq org-superstar-todo-bullet-alist
;;         '(("TODO" . ?☐)
;;           ("NEXT" . ?✒)
;;           ("HOLD" . ?✰)
;;           ("WAITING" . ?☕)
;;           ("CANCELLED" . ?✘)
;;           ("DONE" . ?✔)))
;;   (org-superstar-restart))

(use-package! org-superstar
  :custom (add-hook 'org-mode-hook (lambda ()
                                     (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '(?\s))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars nil)
  (setq org-superstar-todo-bullet-alist '(("TODO" . ?☐)
                                          ("NEXT" . ?✒)
                                          ("HOLD" . ?✰)
                                          ("WAITING" . ?☕)
                                          ("CANCELLED" . ?✘)
                                          ("DONE" . ?✔)))
  (org-superstar-restart))


;; https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package! org-super-agenda
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
           ((agenda "" ((org-agenda-span 'day)
                        ;; (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 0)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 3)
                            (:name "Important"
                                   :priority "A"
                                   :order 1)
                            ;; (:name "Due Today"
                            ;;        :deadline today
                            ;;        :order 2)
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 4)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:discard (:not (:todo "TODO")))))))))))
  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-agenda-prefix-format "%i %-15:c %-14t% s%-6e %/b ")
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  :config
  (org-super-agenda-mode))


;; (setq org-agenda-prefix-format
;;       '(
;;         (agenda . "%-8:i %-8:c * ")
;;         (timeline . " % s")))

(setq org-agenda-files
      (list
       "~/OneDrive/org-roam/20210803124941-inference_2021_q3_enhancement_agenda.org"
       "~/OneDrive/org-roam/20210803112751-paddle_inference_agenda.org"
       "~/OneDrive/org-roam/20210806130344-cinn_compiler_agenda.org"
       ))

(use-package! org-agenda
  :bind
  ("C-c o a" . org-agenda))
