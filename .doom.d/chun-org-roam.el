(require 'org-roam)

(defcustom chun-mode/org-roam-dir "~/OneDrive/org-roam"
  "The org-roam cloud directory."
  :type 'string
  :group 'chun)


(setq org-roam-v2-ack t)

(use-package! org-roam
      :custom
      (org-roam-directory (file-truename chun-mode/org-roam-dir))
      (org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
      (org-roam-complete-everywhere t)
      (org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol)
      (org-id-update-id-locations)

      (setq org-roam-capture-templates '(
                                         ("d" "default" plain "%?"
                                          :if-new
                                          (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+STARTUP: overview indent\n#+bind: org-image-actual-width 400")
                                          :unnarrowed t)
                                         ("b" "beamer" plain "%?"
                                          :if-new (file+head "%<%Y%m%d%H%M%S>-beamer-${slug}.org"
                                                             "#+STARTUP: beamer
#+TITLE:     ${title}
#+AUTHOR:    Chunwei Yan
#+EMAIL:     yanchunwei@baidu.com
#+DATE:      %Y-%m-%d
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE:  en
#+LaTeX_CLASS_OPTIONS: [aspectratio=169,8pt]
#+OPTIONS:   H:2 num:t toc:t :nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:https://orgmode.org/org-info.js
#+EXPORT_SELECT_TAGS: export
#+EXPORT_EXCLUDE_TAGS: noexport
#+HTML_LINK_UP:
#+HTML_LINK_HOME:
#+BEAMER_THEME: default
#+BEAMER_FRAME_LEVEL: 2")))

            ;; end of org-roam-capture-templates
            ))
