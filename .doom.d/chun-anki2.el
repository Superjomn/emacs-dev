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

    ;; get the body content without properties
    (save-excursion
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (when (re-search-forward "^:PROPERTIES:")
          (cl-assert (re-search-forward "^:END:"))
          (forward-line)
          (setq body (buffer-substring (point) (point-max))))))

    (setq properties (org-entry-properties))
    ;; (message "%S" org-element)
    ;; (message "content: %S" (org-element-contents org-element))
    ;; (message "label: %s" label)
    ;; (message "body:[%S] %s" (type-of body) body)
    ;; (message "properties: [%S] %S" (type-of properties) properties)
    (list label body properties)))


(defun chun/-build-anki-format-in-tmp-buffer (label body properties)
  "Build the content for org-anki in a temporary buffer
Args:
  label: str
  body: str
  properties: nil | plist

Returns:
  anki-id: str | nil
  anki-failed-reason: str | nil
"
  (let* ((buffer (get-buffer-create "*chun-anki2*")))
    ;; (message "buffer: %S" buffer)
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (insert (format "* %s\n" label))
      (when properties
        (insert (format ":PROPERTIES:\n"))
        ;; (message "to get property: %S" (assoc "ANKI_NOTE_ID" properties))
        (--insert-property properties "ANKI_NOTE_ID")
        (--insert-property properties "ANKI_DECK")
        (--insert-property properties "ANKI_NOTE_TYPE")
        (insert (format ":END:\n"))
        )
      (insert (format "** Front\n"))
      (insert (format "%s\n" label))
      (insert (format "** Back\n"))
      (insert body)
      (org-mode)
      ;; move to the tail and extract the images from the body
      (let* ((element (org-element-parse-buffer))
             org-content
             )
        (chun/anki--collect-and-push-images element)
        (message "*element: %S" element)
        (setq org-content (org-element-interpret-data element))
        (erase-buffer)
        (message "org-content: %s" org-content)
        (insert org-content)
        )

      (anki-editor-push-notes)

      (let* (cur-element
             cur-properties
             anki-id
             anki-failed-reason)
        (goto-char (point-min))
        (setq cur-element (org-element-at-point))
        (setq cur-properties (org-entry-properties))
        (setq anki-id (--properties-get cur-properties "ANKI_NOTE_ID"))
        (setq anki-failed-reason (--properties-get cur-properties "ANKI_FAILURE_REASON"))
        ;; (message "anki-id: %S" anki-id)
        ;; (message "anki-failed-reason: %S" anki-failed-reason)

        (list anki-id anki-failed-reason)))))

(defun chun/anki--collect-and-push-images (element)
    (org-element-map element 'link
        (lambda (link)
          (message "** link: %S" link)
          (let* (image-path)
                  (when (and (string-equal (org-element-property :type link) "file")
                          (string-match-p "\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|svg\\)$"
                                          (file-name-extension (org-element-property :path link))))
                          (setq image-path (org-element-property :path link))
                          (assert-file-exists image-path)
                          (message "to push image: %s" image-path)
                          (setq image-path (anki-editor--anki-connect-store-media-file image-path))
                          (message "new image path: %s" image-path)
                          (org-element-put-property link :path image-path)
                          (org-element-put-property link :raw-link (format "file:%s" image-path))
                          (message "new link: %S" link)
                          ))))
    (message "element: %S" element)
    ;; (org-element-interpret-data element)
    )

;; (let* ((org-content "* Headline with images
;;    [[file:image1.png]]
;;    Some text here.
;;    [[file:image2.jpg]]
;;    More text with an image: [[file:image3.svg]]
;;    * Another headline without images")
;;        org-element
;;        )
;;   (with-temp-buffer
;;     (insert org-content)
;;     (setq org-element (org-element-parse-buffer))

;;         (message "element: %S" org-element)
;;         (chun/anki--collect-images org-element)
;;     )
;;   )

(defun chun/anki-convert ()
  (interactive)
  (let* ((cur (org-element-at-point))
         (contents (chun/-parse-origin-content cur))
         (label (nth 0 contents))
         (body (nth 1 contents))
         (properties (nth 2 contents))
         (build-res (chun/-build-anki-format-in-tmp-buffer label body properties))
         (anki-id (nth 0 build-res))
         (anki-failure-reason (nth 1 build-res))
         )
    ;; (message "body2: %S" body)
    ;; (message "build res: %S" build-res)
    (when anki-id ;; successfully pushed an anki
      (--add-tag "anki_added")
      (org-set-property "ANKI_NOTE_ID" anki-id)
      )
    (when anki-failure-reason
      (org-set-property "ANKI_FAILURE_REASON" anki-failure-reason)
      )))

(require 'chun-anki)
(defun chun-anki-transform-headline (deck card)

  "Transform the current headline to be an simple anki card.

e.g. An heading is like

* <front>
<back>

Will be transformed to

* <front> :anki:
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic (and reversed card)
:ANKI_DECK: gpu-related
:END:

<back> "


  (interactive (list
                (helm :sources (helm-build-sync-source "anki-deck"
                                 :candidates chun-anki-deck-candidates
                                 :fuzzy-match t)
                      :buffer "*anki deck*")
                (helm :sources (helm-build-sync-source "anki-card"
                                 :candidates chun-anki-card-kinds
                                 :fuzzy-match t)
                      :buffer "*anki card*")))

  (progn
    (org-set-property "ANKI_NOTE_TYPE" card)
    (org-set-property "ANKI_DECK" deck)
    (--add-tag "anki")
    )
)

(defun --add-tag (tag)
  (let* ((current-tags (org-get-tags)))
    (unless (member tag current-tags)
      (org-set-tags (cons tag current-tags)))))

(defun --insert-property (properties key)
  (let* ((prop (assoc key properties)))
    (when prop
      (insert (format ":%s: %s\n" key (cdr prop))))))

(defun --properties-get (properties key)
  "Get value from the properties.
Args:
  properties: result from org-element-property
  key: str
"
  (let* ((pair (assoc key properties)))
    (when pair
      (cdr pair))))


;; borrowed from https://github.com/louietan/anki-editor/issues/30#issuecomment-450463083
(defun anki-editor--anki-connect-store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (unless (-all? #'executable-find '("base64" "sha1sum"))
    (error "Please make sure `base64' and `sha1sum' are available from your shell, which are required for storing media files"))

  (let* ((hash (secure-hash 'sha1 path))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t)))
         content)
    (when (equal :json-false (anki-editor--anki-connect-invoke-result
                              "retrieveMediaFile"
                              `((filename . ,media-file-name))))
      (message "Storing media file to Anki for %s..." path)
      (setq content (string-trim
		     (base64-encode-string
		      (with-temp-buffer
			(insert-file-contents path)
			(buffer-string)))))
      (anki-editor--anki-connect-invoke-result
       "storeMediaFile"
       `((filename . ,media-file-name)
         (data . ,content))))
    media-file-name))

(defun assert-file-exists (file-path)
  "Assert that the file at FILE-PATH exists."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path)))



;; (anki-editor--anki-connect-store-media-file "/Users/yanchunwei/Library/CloudStorage/OneDrive-Personal/org-roam/images/gpu_related/2024-02-25_15-18-34_screenshot.png")
