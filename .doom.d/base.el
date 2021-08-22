;;; ../project/emacs-dev/.doom.d/base.el -*- lexical-binding: t; -*-
;;;
;;; Some basic utility functions

;; OS related
(defun chun/os/is-linux ()
  "Tell whether this system is Linux."
  (string-equal system-type "gnu/linux"))
(defun chun/os/is-macos ()
  "Tell whether this system is Mac."
  (string-equal system-type "darwin"))
(defun chun/os/is-windows ()
  "Tell whether this system is Windows."
  (string-equal system-type "windows-nt"))

;; show file name
(defun chun/get-file-path ()
  "Get the path to the file."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))


(after! chun-mode
  (defun chun/os/on-wsl-p ()
    "Tell whether it runs on the WSL of my Windows PC"
    (string-prefix-p "/mnt" chun-mode/org-roam-dir))

  (defun chun/os/on-mac ()
    "Tell whether it runs on my Mac"
    (and (string-prefix-p "~/" chun-mode/org-roam-dir)
         (chun/os/is-macos))))
