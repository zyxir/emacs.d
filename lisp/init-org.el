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

(zy/snip-from-feature 'org :weight 0
  :dependencies '(calendar ol org-table org-list org-src ob org-agenda))

(when zy/zybox-path
  (setq-default
   org-directory (expand-file-name "org" zy/zybox-path)
   org-journal-dir (expand-file-name "org/org-journal" zy/zybox-path)))

(setq-default org-startup-truncated nil
	      org-startup-numerated t)

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
	    (org-appear-mode +1)
	    (visual-line-mode +1)))

;; Org-attach

(with-eval-after-load 'org-attach
  (setq-default
   org-attach-id-dir "_org-att"))


;;;; Org-Journal

(straight-use-package 'org-journal)

(zy/snip-from-feature 'org-journal :weight 0
  :dependencies 'org)

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

(zy/defsnip snip-gtd
    (:weight 0 :dependencies 'org)
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
                                 "CANCELED(c)")))

  (when (fboundp 'modus-themes-color)
    (set-face-attribute 'org-todo nil
			:weight 'bold
			:foreground (modus-themes-color 'fg-active)
			:background (modus-themes-color 'bg-active))
    (setq-default
     org-todo-keyword-faces
     `(("TODO" .
	(:inherit org-todo :background ,(modus-themes-color 'red-intense-bg)))
       ("DOING" .
	(:inherit org-todo :background ,(modus-themes-color 'blue-intense-bg)))
       ("DONE" .
	(:inherit org-todo :background ,(modus-themes-color 'green-intense-bg)))
       ("CANCELED" .
	(:inherit org-todo :background ,(modus-themes-color 'yellow-subtle-bg))))))

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


(provide 'init-org)

;;; init-org.el ends here
