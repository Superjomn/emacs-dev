;;; rsync-project.el --- Minor mode for synchronizing projects with rsync -*- lexical-binding: t; -*-

;; Author: Chunwei Yan
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools, sync, rsync, project

;;; Commentary:

;; This minor mode provides functionality to synchronize specified local
;; projects to a remote server using rsync. Synchronization is triggered
;; automatically upon saving files within the projects and can also be
;; invoked manually.

;;; Code:

(defgroup rsync-project nil
  "Synchronize projects with a remote server using rsync."
  :prefix "rsync-project-"
  :group 'tools)

;; Customizable variables

(defcustom rsync-project-projects '("~/myproject/")
  "List of project root directories to synchronize."
  :type '(repeat directory)
  :group 'rsync-project)

(defcustom rsync-project-remote-host "user@remote.host"
  "Remote host in the format user@hostname."
  :type 'string
  :group 'rsync-project)

(defcustom rsync-project-remote-root "/workspace/project"
  "Root directory on the remote host where projects are synchronized."
  :type 'string
  :group 'rsync-project)

(defcustom rsync-project-remote-password ""
  "Password for the remote host."
  :type 'string
  :group 'rsync-project)

(defcustom rsync-project-rsync-options '("-avhz" "--delete")
  "List of additional rsync options."
  :type '(repeat string)
  :group 'rsync-project)

;; Internal variables

(defvar rsync-project--sync-in-progress nil
  "Indicator if a sync operation is currently in progress.")

;; Helper Functions

(defun rsync-project--find-project-root (file)
  "Find the project root from `rsync-project-projects` that contains FILE.
Returns the project root path or nil if not found."
  (cl-some (lambda (project)
             (progn
               (when (file-in-directory-p file (expand-file-name project))
                 (expand-file-name project))
               )
             )
           rsync-project-projects))

(defun rsync-project--relative-path (file project-root)
  "Return the relative path of FILE with respect to PROJECT-ROOT."
  (file-relative-name file project-root))

(defun rsync-project--remote-path (relative-path)
  "Construct the remote path for RELATIVE-PATH based on `rsync-project-remote-root`."
  (concat (file-name-as-directory rsync-project-remote-root)
          relative-path))

(defun rsync-project--construct-rsync-command (local-file remote-file)
  "Construct the rsync command to sync LOCAL-FILE to REMOTE-FILE."
  (let ((options (mapconcat 'identity rsync-project-rsync-options " "))
        (remote (concat rsync-project-remote-host ":" (shell-quote-argument remote-file))))
    (format "sshpass -p %s rsync %s %s %s" rsync-project-remote-password options (shell-quote-argument local-file) remote)))

(defun rsync-project--sync-file (file async)
  "Synchronize FILE to the remote location.
If ASYNC is non-nil, perform the synchronization asynchronously."
  (let* ((abs-file (expand-file-name file))
         (project-root (rsync-project--find-project-root abs-file)))
    (if project-root
        (let* ((relative (rsync-project--relative-path abs-file project-root))
               (remote (rsync-project--remote-path relative))
               (command (rsync-project--construct-rsync-command abs-file remote)))
          (if async
              (progn
                (message "Starting async rsync: %s" command)
                (let ((process (start-process-shell-command
                                "rsync-project-async" "*rsync-project-async*"
                                command)))
                  (set-process-sentinel
                   process
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (message "Async rsync completed: %s" (process-command proc))))))
                (setq rsync-project--sync-in-progress t))
            (progn
              (message "Starting rsync: %s" command)
              (let ((exit-code (shell-command command)))
                (if (= exit-code 0)
                    (message "Rsync successful for %s" file)
                  (message "Rsync failed for %s with exit code %d" file exit-code))))))
      (message "File %s is not within any configured project." file))))

(defun rsync-project--after-save-hook ()
  "Hook function to synchronize the current buffer's file after saving."
  (when rsync-project-mode
    (let ((file (buffer-file-name)))
      (when file
        (rsync-project--sync-file file t)))))

;; Minor Mode Definition

;;;###autoload
(define-minor-mode rsync-project-mode
  "Minor mode to synchronize specified projects with a remote server using rsync.

\\{rsync-project-mode-map}"
  :lighter " RsyncProj"
  :group 'rsync-project
  (if rsync-project-mode
      (add-hook 'after-save-hook #'rsync-project--after-save-hook nil t)
    (remove-hook 'after-save-hook #'rsync-project--after-save-hook t)))

;; Interactive Functions

;;;###autoload
(defun rsync-project-upload ()
  "Force upload the current file to the remote server if it is within a configured project."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (progn
          (rsync-project--sync-file file nil))
      (message "Current buffer is not visiting a file."))))

;;;###autoload
(defun rsync-project-upload-async ()
  "Force upload the current file to the remote server asynchronously if it is within a configured project."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (progn
          (rsync-project--sync-file file t))
      (message "Current buffer is not visiting a file."))))

;; Provide the feature
(provide 'rsync-project)

;;; rsync-project.el ends here
