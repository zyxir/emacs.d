;;; zy-gtd.el --- Org-based GTD system. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+gtd' module of the configuration.

;; Org-mode can serve as a powerful GTD (Getting Things Done) system via its
;; todo management and agenda features. This file configures my personal GTD
;; system centered around "gtd.org".

;;; Code:

(require 'zylib)

(defvar +gtd-file nil
  "The gtd.org file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(after! 'org
  ;; Track the time of an entry set to done.
  (setq org-log-done 'time)

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
  ;; Add the GTD file to agenda files.
  (add-to-list 'org-agenda-files +gtd-file)

  ;; Use normal state for the agenda buffer.
  (after! 'evil
    (evil-set-initial-state 'org-agenda-mode 'normal))

  ;; Use variable pitch font for the agenda.
  (add-hook! 'org-agenda-mode-hook #'variable-pitch-mode))

(after! 'org-capture
  ;; Capture templates (in accordance with gtd.org headings).
  (let ((templates
         `(("c" "Computer" entry
            (file+headline ,+gtd-file "Computer")
            "* TODO %i%?\nCREATED: %U"
            :kill-buffer t)
           ("l" "Life" entry
            (file+headline ,+gtd-file "Life")
            "* TODO %i%?\nCREATED: %U"
            :kill-buffer t)
           ("g" "Gaming" entry
            (file+headline ,+gtd-file "Gaming")
            "* TODO %i%?\nCREATED: %U"
            :kill-buffer t))))
    (dolist (template templates)
      (add-to-list 'org-capture-templates template))))

(provide 'zy-gtd)

;;; zy-gtd.el ends here
