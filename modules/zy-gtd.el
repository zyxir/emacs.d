;;; zy-gtd.el --- Org-based GTD system. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+gtd' module of the configuration.

;; Org-mode can be used as a powerful GTD (getting things done) system via its
;; todo management and agenda features. This file sets up my personal GTD system
;; with processes including idea capturing, entry refiling, and agenda viewing.

;;; Code:

(require 'zylib)

(defvar +gtd-dir nil
  "My directory for the GTD system.")

(defvar +gtd-inbox-file nil
  "My inbox file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(defvar +gtd-gtd-file nil
  "My GTD file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(defvar +gtd-someday-file nil
  "My someday file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(after! 'org
  ;; Track the time of an entry set to done.
  (setq org-log-done 'time)

  ;; Refile entries from inbox to other GTD files.
  (setq-default org-refile-targets `((,+gtd-gtd-file :level . 2)
                                     (,+gtd-someday-file :maxlevel . 3)))

  ;; Customized todo keywords.
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "DOING(i)"
                                      "|"
                                      "DONE(d)"
                                      "CANCELED(c)"))
        org-todo-keyword-faces '(("TODO" . org-todo)
                                 ("DOING" . package-status-available)
                                 ("DONE" . org-done)
                                 ("CANCELED" . shadow))))

(after! 'org-agenda
  ;; Add GTD files to agenda files.
  (add-to-list 'org-agenda-files +gtd-inbox-file)
  (add-to-list 'org-agenda-files +gtd-gtd-file)
  (add-to-list 'org-agenda-files +gtd-someday-file)

  ;; Use normal state for the agenda buffer.
  (after! 'evil
    (evil-set-initial-state 'org-agenda-mode 'normal))

  ;; Use variable pitch font for the agenda.
  (add-hook! 'org-agenda-mode-hook #'variable-pitch-mode))

(after! 'org-capture
  ;; Capture template for todo entries.
  (add-to-list 'org-capture-templates
               `("i" "GTD inbox" entry
                 (file+headline ,+gtd-inbox-file "Inbox")
                 "* TODO %i%? %^G\nCREATED: %U"
                 :kill-buffer t)))

(provide 'zy-gtd)

;;; zy-gtd.el ends here
