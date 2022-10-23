;;; init-org.el --- Org mode settings -*- lexical-binding: t -*-


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Basic settings of Org mode

;;; Code:

(require 'init-common)
(require 'init-keybinding)
(require 'init-load)


;;;; Basic Org

;; Customized Org paths when Zybox is available

(zy/incload-register 'org :level 3 :after
		     'calendar 'ol 'org-table 'org-list 'org-src 'ob
		     ('org-agenda :level 2))

(when zy/zybox-path
  (setq-default
   org-directory (expand-file-name "org" zy/zybox-path)
   org-journal-dir (expand-file-name "org/org-journal" zy/zybox-path)))

(setq-default org-startup-truncated nil)

(zy/define-key :prefix zy/leader-keys
  "a" 'org-agenda
  "c" 'org-capture)

(with-eval-after-load 'org
  (setq-default
   org-hide-emphasis-markers t
   org-tag-column 0)
  (when (boundp 'org-format-latex-options)
    (plist-put org-format-latex-options ':scale 1.2))
  ;; Consult Org commands
  (zy/define-key :keymap 'org-mode-map
    "M-g h" 'consult-org-heading))

(straight-use-package 'org-appear)

(add-hook 'org-mode-hook
	  (lambda ()
	    (when (fboundp 'org-appear-mode)
	      (org-appear-mode +1))
	    (visual-line-mode +1)))

;; Org-attach

(with-eval-after-load 'org-attach
  (setq-default
   org-attach-id-dir "_org-att"))


;;;; Org-Export to LaTeX

(zy/defsnip 'snip-ox-latex
  (require 'ox)

  ;; Try to obtain Zyxir's LaTeX style file

  (defvar zy/zylatex-file
    (file-truename
     (expand-file-name "etc/zylatex.sty" user-emacs-directory))
    "Default LaTeX style file to use.")

  (defvar zy/zylatex-available-p nil
    "If `zy/zylatex-file' is available.")

  (defun zy/update-zylatex-file ()
    "Update the `zy/zylatex-file' from GitHub or Zybox."
    (interactive)
    (let (ego-found
          std-latex-found
          possible-paths
          path-to-examine
	  project-name)
      ;; Try to find my 'ego' or 'std-latex' repo via project.el.
      (require 'project)
      (defvar project--list)
      (when (and (equal project--list 'unset)
		 (fboundp 'project--read-project-list))
        (project--read-project-list))
      (setq possible-paths project--list)
      (while possible-paths
        (setq path-to-examine (caar possible-paths)
              project-name (file-name-nondirectory
                            (directory-file-name path-to-examine)))
        (cond
         ((equal project-name "std-latex")
          (setq std-latex-found path-to-examine
                possible-paths nil))
         ((equal project-name "ego")
          (setq ego-found path-to-examine
                possible-paths nil))
         (t (setq possible-paths (cdr possible-paths)))))
      ;; When something is found, copy zylatex.sty from it, otherwise
      ;; download zylatex.sty from GitHub.
      (cond
       (ego-found
        (copy-file (expand-file-name
                    "std/std-latex/zylatex.sty" ego-found)
                   zy/zylatex-file 'ok-if-already-exists 'keep-time
                   'preserve-uid-gid 'preserve-permissions)
        (message "\"zylatex.sty\" copied from project \"ego\""))
       (std-latex-found
        (copy-file (expand-file-name
                    "zylatex.sty" std-latex-found)
                   zy/zylatex-file 'ok-if-already-exists 'keep-time
                   'preserve-uid-gid 'preserve-permissions)
        (message "\"zylatex.sty\" copied from project \"std-latex\""))
       (t
        (url-copy-file
         "https://raw.githubusercontent.com/zyxir/std-latex/main/zylatex.sty"
         zy/zylatex-file 'ok-if-already-exists)
        (message "\"zylatex.sty\" downloaded.")))))

  (if (file-exists-p zy/zylatex-file)
      (setq zy/zylatex-available-p t)
    (condition-case nil
	(zy/update-zylatex-file)
      (error (message "Error fetching \"zylatex.sty\"")))
    (setq zy/zylatex-available-p (file-exists-p zy/zylatex-file)))

  ;; Configure Org to LaTeX export

  (setq-default org-latex-compiler "xelatex"
		org-latex-default-class "article"
		;; Delete ".tex" file as well.
		org-latex-logfiles-extensions
		'("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log"
		  "nav" "out" "ptc" "run.xml" "snm" "tex" "toc" "vrb" "xdv"))

  (when zy/zylatex-available-p
    (setq-default org-latex-classes
		  `(;; 自用導出配置，用於個人日誌、散文等。
		    ("article"
		     ,(format "\
\\documentclass[12pt]{article}
\\usepackage[]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy/zylatex-file))
		     ("\\section{%s}" . "\\section*{%s}")
		     ("\\subsection{%s}" . "\\subsection*{%s}")
		     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		     ("\\paragraph{%s}" . "\\paragraph*{%s}")
		     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		    (;; 適合手機屏幕閱讀的配置。
		     "article-phone"
		     ,(format "\
\\documentclass[12pt]{article}
\\usepackage[layout=phone]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy/zylatex-file))
		     ("\\section{%s}" . "\\section*{%s}")
		     ("\\subsection{%s}" . "\\subsection*{%s}")
		     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		     ("\\paragraph{%s}" . "\\paragraph*{%s}")
		     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		    (;; 適用於簡體中文的配置。
		     "article-sc"
		     ,(format "\
\\documentclass[12pt]{article}
\\usepackage[style=tc, fontset=ctex]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy/zylatex-file))
		     ("\\section{%s}" . "\\section*{%s}")
		     ("\\subsection{%s}" . "\\subsection*{%s}")
		     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		     ("\\paragraph{%s}" . "\\paragraph*{%s}")
		     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

  ;; Export smartphone-friendly PDF

  (defun zy/org-export-to-pdf-phone
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to smartphone-friendly PDF.

The function works like `org-latex-export-to-pdf', except that
`org-latex-default-class' is set to \"article-phone\"."
    (dlet ((org-latex-default-class "article-phone"))
      (org-latex-export-to-pdf async subtreep visible-only
                               body-only ext-plist)))

  (org-export-define-derived-backend
      'latex-pdf-phone 'latex
    :menu-entry '(?l
		  "Export to LaTeX"
		  ((?j "As PDF file (phone-friendly)"
		       zy/org-export-to-pdf-phone)))))

(zy/lload-register 'snip-ox-latex 'ox-latex)
(zy/incload-register 'snip-ox-latex :level 2 :after 'org)


;;;; Org-Journal

(straight-use-package 'org-journal)

(zy/incload-register 'org-journal :after 'org)

(setq-default
 org-journal-prefix-key "C-c j")

(zy/define-key
  :prefix zy/leader-keys
  "J" 'calendar
  "j" '("journal" . org-journal-new-entry))

(with-eval-after-load 'org-journal
  (setq-default
   org-journal-extend-today-until 3
   org-journal-file-format "%F.org"
   org-journal-date-format "%F %a W%V\n"
   org-journal-date-prefix "#+title: "
   org-journal-time-format "%R "
   org-journal-time-format-post-midnight "%R (midnight) "
   org-journal-time-prefix "\n* "
   org-journal-file-header ""))


;;;; GTD: Getting Things Done

(zy/defsnip 'snip-gtd
  ;; Setup GTD paths

  (when zy/zybox-path
    (defvar zy/gtd-path (expand-file-name "org-gtd" org-directory)
      "Where my GTD files are located.")
    (defvar zy/gtd-inbox-file (expand-file-name "inbox.org" zy/gtd-path)
      "The path of \"inbox.org\" of the GTD system.")
    (defvar zy/gtd-gtd-file (expand-file-name "gtd.org" zy/gtd-path)
      "The path of \"gtd.org\" of the GTD system.")
    (defvar zy/gtd-someday-file (expand-file-name "someday.org" zy/gtd-path)
      "The path of \"someday.org\" of the GTD system.")
    (defvar zy/gtd-files `(,zy/gtd-inbox-file
			   ,zy/gtd-gtd-file
			   ,zy/gtd-someday-file)
      "List of all files of the GTD system.")

    ;; Make sure that all the files exist

    (mapc (lambda (file)
	    (unless (file-exists-p file)
	      (write-region "" nil file)))
	  zy/gtd-files)

    ;; Setup agenda, capture and refile

    (setq-default
     org-agenda-files `(,zy/gtd-inbox-file
			,zy/gtd-gtd-file
			,zy/gtd-someday-file)
     org-capture-templates `(("i" "inbox" entry
                              (file+headline ,zy/gtd-inbox-file "inbox")
                              "* TODO [#B] %u %i%?"
                              :empty-lines 1)
                             ("s" "someday" entry
                              (file+headline ,zy/gtd-someday-file "someday")
                              "* TODO [#C] %u %i%?"
                              :empty-lines 1)
                             ("t" "GTD" entry
                              (file+olp+datetree ,zy/gtd-gtd-file)
                              "* TODO [#B] %u %i%?"
                              :empty-lines 1))
     org-refile-targets `((,zy/gtd-gtd-file :maxlevel . 3)
                          (,zy/gtd-someday-file :level . 1))))

  ;; Tweak Org for GTD

  (setq-default
   org-log-done 'time
   org-log-refile 'time
   org-todo-keywords '((sequence "TODO(t)"
                                 "DOING(i)"
                                 "|"
                                 "DONE(d)")
                       (sequence "|"
                                 "CANCELED(c)"))
   org-todo-keyword-faces '(("TODO" . modus-themes-intense-red)
			    ("DOING" . modus-themes-active-cyan)
			    ("DONE" . modus-themes-intense-green)
			    ("CANCELED" . modus-themes-tab-inactive)))

  ;; Per-project TODO

  (with-eval-after-load 'project
    (defvar project--list)
    (defvar org-agenda-files)

    ;; Get project list

    (when (and (equal project--list 'unset)
	       (fboundp 'project--read-project-list))
      (project--read-project-list))

    ;; Add per-project TODO files to `org-agenda-files'

    (defvar zy/project-todo-regexp "^.*TODO\\.org$"
      "Possible TODO filenames for projects.")
    (mapc
     (lambda (proj)
       (let* ((proj-path (car proj))
              proj-todos)
         (setq proj-todos (directory-files proj-path
					   'full
					   zy/project-todo-regexp
					   'nosort))
	 (when proj-todos
	   (setq org-agenda-files
		 (nconc org-agenda-files
			proj-todos)))))
     project--list)
    (delete-dups org-agenda-files)))

(zy/incload-register 'snip-gtd :after 'org)


(provide 'init-org)

;;; init-org.el ends here
