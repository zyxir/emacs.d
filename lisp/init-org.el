;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This file contains all configurations about org mode.

;;; Code:

;;;; Org Mode

(use-package org
  :hook
  (org-mode . auto-fill-mode)
  :general
  ("C-c C-i" nil
   "C-c c" 'org-capture
   "C-c a" 'org-agenda)
  :init
  ;; Allow Chinese around markups, maybe unstable, from
  ;; https://emacs-china.org/t/org-mode/597/4
  (setq org-emphasis-regexp-components
	;; markup 记号前后允许中文
	(list (concat " \t('\"{"            "[:nonascii:]")
	      (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
	      " \t\r\n,\"'"
	      "."
	      1))
  ;; Put attachments in an obvious directory.
  (setq org-attach-id-dir "org-attachments/"))

;;;; Export to HTML

(use-package org
  :config
  ;; Remove spaces for Chinese output.
  (defun clear-single-linebreak-in-cjk-string (string)
    "clear single line-break between cjk characters that is usually soft line-breaks"
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
	   (start (string-match regexp string)))
      (while start
	(setq string (replace-match "\\1\\2" nil nil string)
	      start (string-match regexp string start))))
    string)

  (require 'ox-man)

  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string)))

  (add-to-list 'org-export-filter-final-output-functions
	       'ox-html-clear-single-linebreak-for-cjk))

;;;; Export to LaTeX

(use-package org
  :config
  ;; Main class for Chinese articles.
  (add-to-list 'org-latex-classes
	       '("cn-article"
		 "\\documentclass[lang=cn]{elegantpaper}
\\usepackage{ctex}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; End of config.

;;;; Org-Journal and Calendar

(use-package calendar
  :ensure nil
  :general
  ("C-c g" 'calendar))

(use-package org-journal
  :straight t
  :general
  ("C-c j" 'org-journal-new-entry)
  :config
  (setq org-journal-dir (concat zy/zybox-path "org-journal"))
  (unless (file-exists-p org-journal-dir)
    (make-directory org-journal-dir))
  ;; Configure the template.
  (setq org-journal-file-format "%F"
	org-journal-date-format "%F %a W%V\n"
	org-journal-date-prefix "#+TITLE: "
	org-journal-time-format "%R "
	org-journal-time-format-post-midnight "%R (midnight) "
	org-journal-time-prefix "\n* "
	org-journal-file-header "")
  ;; If it is early than 3 a.m., it is still yesterday.
  (setq org-extend-today-until 3))

;;;; GTD

(use-package org
  :init
  ;; GTD file paths.
  (defvar zy/gtd-path nil
    "The path of my GTD system root.")
  (defvar zy/gtd-inbox-path nil
    "The path of `inbox.org' of my GTD system.")
  (defvar zy/gtd-gtd-path nil
    "The path of `gtd.org' of my GTD system.")
  (defvar zy/gtd-someday-path nil
    "The path of `someday.org' of my GTD system.")
  (setq zy/gtd-path
	(concat zy/zybox-path "org-gtd/")
	zy/gtd-inbox-path
	(concat zy/gtd-path "inbox.org")
	zy/gtd-gtd-path
	(concat zy/gtd-path "gtd.org")
	zy/gtd-someday-path
	(concat zy/gtd-path "someday.org"))
  ;; TODO states.
  (setq org-todo-keywords
	'((sequence "TODO(t)"
		    "IN PROCESS(i)"
		    "POSTPONED(p)"
		    "|"
		    "DONE(d)")
	  (sequence "|"
		    "CANCELED(c)")))

  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#B71C1C" :weight bold))
	  ("IN PROCESS" . (:foreground "#8BC34A" :weight bold))
	  ("POSTPONED" . (:foreground "#F57C00" :weight bold))
	  ("DONE" . (:foreground "#33691E" :weight bold))
	  ("CANCELED" . (:foreground "#757575" :weight bold)))))

(use-package org-capture
  :ensure nil
  :after org
  :config
  (add-to-list 'org-capture-templates
	       `("i" "inbox" entry
		 (file+headline ,zy/gtd-inbox-path "inbox")
		 "* TODO [#B] %U %i%?"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       `("s" "someday" entry
		 (file+headline ,zy/gtd-someday-path "someday")
		 "* TODO [#C] %U %i%?"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       `("g" "GTD" entry
		 (file+datetree ,zy/gtd-gtd-path)
		 "* TODO [#B] %U %i%?"
		 :empty-lines 1)))

(use-package org-refile
  :ensure nil
  :after org
  :config
  (add-to-list 'org-refile-targets `(,zy/gtd-gtd-path :maxlevel . 3))
  (add-to-list 'org-refile-targets `(,zy/gtd-someday-path :level . 1)))

(use-package org-agenda
  :ensure nil
  :after org
  :general
  ("C-c a" 'org-agenda)
  :config
  (setq org-agenda-files `(,zy/gtd-inbox-path
			   ,zy/gtd-gtd-path
			   ,zy/gtd-someday-path)))

(provide 'init-org)