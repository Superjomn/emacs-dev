;; (headline
;;     (:raw-value "a heading" :begin 1 :end 25 :pre-blank 0 :contents-begin 13 :contents-end 25 :robust-begin 15 :robust-end 23 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 1 :title (#("a heading" 0 9 (:parent (headline #1)))) :parent (org-data (:begin 1 :contents-begin 1 :contents-end 25 :end 25 :robust-begin 3 :robust-end 23 :post-blank 0 :post-affiliated 1 :path nil :mode org-data :CATEGORY nil :parent nil :cached t)) :cached t))
(defun chun/-parse-origin-content (org-element)
  "Parse the original org content and get label, content and properties.
Args:

org-element: element
"
  (let* (label body properties)
    (setq label (org-element-property :raw-value org-element))
    (setq body
          (buffer-substring
           (org-element-property :contents-begin org-element)
           (org-element-property :contents-end org-element)))
    (setq properties (org-entry-properties))
    (message "%S" org-element)
    (message "content: %S" (org-element-contents org-element))
    (message "label: %s" label)
    (message "body:[%S] %s" (type-of body) body)
    (message "properties: [%S] %S" (type-of properties) properties)
    (list label body properties)))


(defun chun/-build-anki-format-in-tmp-buffer (label body properties)
  "Build the content for org-anki in a temporary buffer
Args:
  label: str
  body: str
  properties: nil | plist
"
  (let* ((buffer (get-buffer-create "*chun-anki2*")))
    (message "buffer: %S" buffer)
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (insert (format "* %s" label))
      (when properties
        (--insert-property properties :ANKI_NOTE_ID ":ANKI_NOTE_ID:")
        (--insert-property properties :ANKI_DECK ":ANKI_DECK:")
        (--insert-property properties :ANKI_NOTE_TYPE ":ANKI_NOTE_TYPE:")
        )
      (message "get body: %S" body)
      (insert body))))

(defun chun/anki-convert ()
  (interactive)
  (let* ((cur (org-element-at-point))
         (contents (chun/-parse-origin-content cur))
         (label (nth 0 contents))
         (body (nth 1 contents))
         (properties (nth 2 contents))
         )
    (message "body2: %S" body)

    (chun/-build-anki-format-in-tmp-buffer label body properties)
    ))

'(("CATEGORY" . "???") ("BLOCKED" . "") ("FILE") ("PRIORITY" . "B") ("ITEM" . "a heading"))

(defun --insert-property (properties key key-s)
  (let* ((prop (plist-get properties key)))
    (when prop
      (insert (format "%s %s" key-s prop)))))
