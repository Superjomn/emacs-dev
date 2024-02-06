(defcustom chun-anki-card-kinds
  '("Basic (and reversed card)" "Basic (optional reversed card)")
  "Anki card kinds"
  :type 'list
  :group 'chun-anki)

(defcustom chun-anki-deck-candidates
  '("English-learn-org" "gpu-related" "Algorithm")
  "Anki docs"
  :type 'list
  :group 'chun-anki)

(defvar chun-anki--org-heading-level 1
  "chun-anki card org level")

(defun chun-anki-reset-org-level (&optional level)
  (interactive "nEnter level: ")
  (if (> level 0)
      (setq chun-anki--org-heading-level level)
    (setq chun-anki--org-heading-level (+ 1 (org-current-level)))))


(defun chun-anki-new-card (deck card)
  (interactive (list
                (helm :sources (helm-build-sync-source "anki-deck"
                                 :candidates chun-anki-deck-candidates
                                 :fuzzy-match t)
                      :buffer "*anki deck*")
                (helm :sources (helm-build-sync-source "anki-card"
                                 :candidates chun-anki-card-kinds
                                 :fuzzy-match t)
                      :buffer "*anki card*")))

  (let* ((input (read-string "Input: "))
         (org-root-heading-prefix (make-string chun-anki--org-heading-level ?*))
         (org-body-heading-prefix (make-string (+ 1 chun-anki--org-heading-level) ?*))
         )
    (insert (format "%s %s :phase:
:PROPERTIES:
:ANKI_NOTE_TYPE: %s
:ANKI_DECK: %s
:END:

%s Front
%s
%s Back
"
                    org-root-heading-prefix
                    input
                    card
                    deck
                    org-body-heading-prefix
                    input
                    org-body-heading-prefix
                    ))))


(defun chun-anki-simple-card (deck card)
  (interactive (list
                (helm :sources (helm-build-sync-source "anki-deck"
                                 :candidates chun-anki-deck-candidates
                                 :fuzzy-match t)
                      :buffer "*anki deck*")
                (helm :sources (helm-build-sync-source "anki-card"
                                 :candidates chun-anki-card-kinds
                                 :fuzzy-match t)
                      :buffer "*anki card*")))

  (let* ((label (read-string "Label: "))
         (org-root-heading-prefix (make-string chun-anki--org-heading-level ?*))
         )
    (insert (format "%s %s :anki:
:PROPERTIES:
:ANKI_NOTE_TYPE: %s
:ANKI_DECK: %s
:END:

"
                    org-root-heading-prefix
                    label
                    card
                    deck
                    ))))

(defun chun-anki-add-properties (deck card)
        (interactive (list
                (helm :sources (helm-build-sync-source "anki-deck"
                                 :candidates chun-anki-deck-candidates
                                 :fuzzy-match t)
                      :buffer "*anki deck*")
                (helm :sources (helm-build-sync-source "anki-card"
                                 :candidates chun-anki-card-kinds
                                 :fuzzy-match t)
                      :buffer "*anki card*")))
    (insert (format ":PROPERTIES:
:ANKI_NOTE_TYPE: %s
:ANKI_DECK: %s
:END:
"
                    card
                    deck
                    ))

    )


(provide 'chun-anki)
