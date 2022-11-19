(defcustom chun-bili-publish-file "org-file to dump" nil
  :type 'string
  :group 'chun-bili)

(defcustom chun-bili--anki-note-type "Basic (and reversed card)" nil
  :type 'string
  :group 'chun-bili)

(defcustom chun-bili--anki-dock "English-translate" nil
  :type 'string
  :group 'chun-bili)

(defcustom chun-bili--en "En" nil
  :type 'string
  :group 'chun-bili)

(defcustom chun-bili--ch "Ch" nil
  :type 'string
  :group 'chun-bili)

(defconst chun-bili-anki-note-type-property-key "ANKI_NOTE_TYPE")
(defconst chun-bili-anki-dock-property-key "ANKI_DOCK")


(defun chun-bili-get-content-at-this-point ()
  "Get En and Ch content from the headline at this point."
  (interactive)
  (message "%S"
           (chun-bili--get-ch-en-content-at-this-point)))


(defun chun-bili--get-ch-en-content-at-this-point ()
  "Get the content within this heading.
There should be two subsection in this section, the first is 'En', the second is 'Ch'.
"
  (let* ((root (org-element-at-point))
         (res (ht-create))
         (title (org-element-property :raw-value root))
         (content (buffer-substring-no-properties (org-element-property :contents-begin root)
                                                  (org-element-property :contents-end root)))
         content-element)
    (save-excursion
      (with-temp-buffer
        (insert content)
        (setq content-element (org-element-parse-buffer))
        (org-element-map content-element
            'headline
          (lambda (elem)
            (let* ((elem-title (string-trim (org-element-property :raw-value elem))) elem-content)
              (setq elem-content (buffer-substring-no-properties (org-element-property :contents-begin elem)
                                                                 (org-element-property :contents-end elem)))
              (message "elem: %S" content-element)
              (message "title: %S" elem-title)
              (when (string-equal elem-title chun-bili--en)
                (ht-set! res chun-bili--en elem-content))
              (when (string-equal elem-title chun-bili--ch)
                (ht-set! res chun-bili--ch elem-content)))))))
    res))


(defun chun-bili--get-anki-properties (element)
  "Get the anki-related properties from the org-element.

Inputs:
- element: org-element instance.
Returns:
- '(anki-type dock), both are string.
"
  (let* ((note-type (org-element-property :ANKI_NOTE_TYPE element))
         (dock (org-element-property :ANKI_DOCK element))))

  '(note-type dock))


(defun chun-bili--create-anki-org-record (line0 line1 note-type anki-dock headline-level)
  "Create an anki record in org-mode.

Inputs:

- line0: string
- line1: string
- anki-doc: string
- headline-level:int

Returns:

- string
"
  (let* ((element (org-element-create 'headline))
         (child-element-0 (org-element-create 'headline))
         (child-element-1 (org-element-create 'headline))
         )
    ;; set properties for parent headline
    (org-element-put-property element :level headline-level)
    (org-element-put-property element :raw-value line0)
    (org-element-put-property element :title line0)
    (org-element-put-property element :tags '("anki"))

    ;; set properties for children headlines
    (org-element-put-property child-element-0 :raw-value "Front")
    (org-element-put-property child-element-0 :title "Front")
    (org-element-put-property child-element-0 :level (+ headline-level 1))
    (let* ((paragraph (org-element-create 'paragraph))
           (child-paragraph0 (org-element-create 'paragraph))
           (child-paragraph1 (org-element-create 'paragraph))
           )
      (org-element-set-contents paragraph
                                (format "
:PROPERTIES:
:ANKI_NOTE_TYPE: %s
:ANKI_DECK: %s
:END:
" note-type anki-dock))

      (org-element-set-contents child-paragraph0 line0)
      (org-element-set-contents child-paragraph1 line1)

      (org-element-adopt-elements element paragraph)
      (org-element-adopt-elements child-element-0 child-paragraph0)
      (org-element-adopt-elements child-element-1 child-paragraph1)
      )

    (org-element-put-property child-element-1 :raw-value "Back")
    (org-element-put-property child-element-1 :title "Back")
    (org-element-put-property child-element-1 :level (+ headline-level 1))

    (org-element-adopt-elements element child-element-0 child-element-1)

    ;; (chun-bili--org-element-put-anki-properties element "EnCh" "Basic")

    (org-element-interpret-data element)
    ))


(defun chun-bili--pair-data-to-org (title lines0 lines1 note-type anki-dock headline-level)
  "Transpose two list to org-mode content."
  (assert (eq (length lines0) (length lines1)))
  (let* ((i 0)
         (n (length lines0))
         line0
         line1
         (org-content (format "* %s\n" title))
         )
    (while (< i n)
        (setq line0 (nth i lines0))
        (setq line1 (nth i lines1))
        (setq org-content (concat org-content "\n" (chun-bili--create-anki-org-record line0 line1
                                                             note-type anki-dock headline-level)))
        (incf i))

    org-content))


(defun chun-bili--write-to-file (content path)
  "Write content to file."
  (save-excursion
    (let*
        ((buffer (get-file-buffer chun-bili-publish-file)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert content)
        ))))

(defun chun-bili--string-valid (str)
  (if (stringp str) (not (string= (string-trim str) ""))))

(defun chun-bili--get-anki-note-type (element)
  "Get anki note-type property. Return it if set or get the global variable `chun-bili--anki-note-type' instead."
  (let* ((anki-note (org-element-property :ANKI_NOTE_TYPE element)))
    (if (chun-bili--string-valid anki-note) anki-note
      chun-bili--anki-note-type)))

(defun chun-bili--get-anki-dock-name (element)
  "Get anki note-type property. Return it if set or get the global variable `chun-bili--anki-dock' instead."
  (let* ((anki-note (org-element-property :ANKI_DOCK element)))
    (if (chun-bili--string-valid anki-note) anki-note
      chun-bili--anki-dock)))

(defun chun-bili--split-valid-lines (str)
  (-filter #'chun-bili--string-valid
           (mapcar #'string-trim (split-string str "\n"))))

(defun chun-bili--pair-data-this ()
  (interactive)
  (let* ((pair-dic (chun-bili--get-ch-en-content-at-this-point))
         (content-en (ht-get pair-dic chun-bili--en))
         (content-ch (ht-get pair-dic chun-bili--ch))
         (lines-en (chun-bili--split-valid-lines content-en))
         (lines-ch (chun-bili--split-valid-lines content-ch))
         (element (org-element-at-point))
         (title (org-element-property :raw-value element))
         org-content
         )

         (setq anki-note-type (chun-bili--get-anki-note-type element))
         (message "anki-note type %s" anki-note-type)
         (setq anki-dock (chun-bili--get-anki-dock-name element))
         (message "anki-dock %s" anki-dock)

    (setq org-content (chun-bili--pair-data-to-org title lines-en lines-ch anki-note-type
                                      anki-dock 2))
    (message "%S" org-content)
    (chun-bili--write-to-file org-content chun-bili-publish-file)))

