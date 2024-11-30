;; export events to web

(load! "./chun-core.el")

(defun org-agenda-events-to-json (output-file)
  "Export Org agenda events to a JSON file."
  (interactive "FExport JSON to file: ")
  (let ((events '())
        (today (format-time-string "%Y-%m-%d"))
        )
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (let* ((raw-title (org-element-property :raw-value headline))
                   (title (if raw-title
                              (org-no-properties
                               (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" raw-title)) ;; Extract plain text from links
                            ""))
                   ;;(file-title (chun-org--get-file-title-from-buffer))
                   (abs-title (chun-org--get-hierarchical-title headline " / "))

                   (todo-state (org-element-property :todo-keyword headline))
                   (scheduled (org-element-property :scheduled headline))
                   (deadline (org-element-property :deadline headline))
                   (timestamp (or scheduled deadline))
                   (start (if scheduled
                              (org-timestamp-format scheduled "%Y-%m-%dT%H:%M:%S")
                            (let ((time (org-timestamp-to-time deadline))) ;; end - 1hour
                              (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                  (time-subtract time (seconds-to-time 3600))))))
                   (end (if deadline
                              (org-timestamp-format deadline "%Y-%m-%dT%H:%M:%S")
                          (let ((time (org-timestamp-to-time timestamp))) ;; start + 1hour
                            (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                (time-add time (seconds-to-time 3600))))))
                   (days-before-today (if timestamp
                                          (floor (org-agenda-days-before-today timestamp))
                                        0))
                   (delayed-task (> days-before-today 0))
                   (is-all-day (and timestamp (not (org-element-property :hour-start timestamp))))
                   )
                   ;;(tags (org-element-property :tags headline))

              ;;(setq is-all-day (or is-all-day delayed-task))
              ;;(when delayed-task (setq start today)) ;; We only track the delayed tasks on today

              ;; For all-day event, just start date is necessary
              (when (and is-all-day (not delayed-task))
                (setq start (org-timestamp-format timestamp "%Y-%m-%d")))

              ;; If one is nil, get the other's value
              (setq start (or start end))
              (setq end (or end start))

              ;;(message "title: %s, start: %S, end: %S" title start end)
              (when (and (org-agenda-event-include-p todo-state)
                         (not (string-empty-p abs-title))
                         (or start end))

                (if (not is-all-day)
                    (push `(("title" . ,abs-title)
                        ("start" . ,start)
                        ("end" . ,end)
                        )
                          events)
                  (push `(("title" . ,abs-title)
                        ("start" . ,start)
                        )
                          events)))

              ;; deal with the delayed task, push additional record to today
              (when (and delayed-task (org-agenda-event-include-p todo-state))
                  (setq new-title (format "%dX: %s" days-before-today abs-title))
                  (setq start today)
                  (push `(("title" . ,new-title) ("start" . ,start)) events)
                  )

              )))))

    (with-temp-file output-file
      (insert "var events = ")
      (insert (json-encode (nreverse events))))
    (message "Exported to %s" output-file)))

(defun org-agenda-event-include-p (state)
  "Determine if an event with the given STATE should be included.
Customize this function to adjust filtering logic."
  (member state '("TODO" "DOING")))

(defun chun-org-get-event (title start end)
  "
Inputs:
  - title: string
  - start: optional[str]
  - end: optional[str]

Returns:
 a list of (title start end)"
  )

(defun org-agenda-days-before-today (timestamp)
  "Calculate the number of days before today for a given TIMESTAMP."
  (let ((time (org-timestamp-to-time timestamp)))
    (/ (float-time (time-subtract (current-time) time)) 86400)))


(defun chun-agenda-lanuch-web-view ()
  "Launch a web view for org-agenda."
  (interactive)
    (org-agenda-events-to-json "/Users/chunwei/emacs-dev/org-agenda-web-view/events.js")
    (browse-url (concat "file://" (expand-file-name "~/emacs-dev/org-agenda-web-view/web.html")))
  )

(provide 'chun-agenda-web-view)
