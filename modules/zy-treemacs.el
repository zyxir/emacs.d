;;; zy-treemacs.el --- Treemacs sidebar. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+treemacs' module of the configuration.

;; Treemacs provides a sidebar of a tree layout file explorer. It is both heavy
;; and feature-rich, and is the closest package to the file explorers in other
;; text editors.

;;; Code:

(require 'zylib)

(pin-to! "melpa" 'treemacs)

(pkg! 'treemacs)
(pkg! 'treemacs-evil)
(pkg! 'treemacs-magit)
(pkg! 'treemacs-tab-bar)

(after! '+leader
  (keybind! nil 'global
    [remap +leader-sidebar] (cons "Treemacs" #'treemacs-select-window)))

;; Treemacs is frequently used. Load it now on daemon sessions.
(daemon-require! 'treemacs)

(after! 'treemacs
  ;; Follow the current file.
  (treemacs-follow-mode 1)

  ;; Automatically switch to the current project.
  (treemacs-project-follow-mode 1)

  ;; Auto-refresh the tree on file system changes.
  (treemacs-filewatch-mode 1)

  ;; Show commit differences with remote like VS Code does.
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode 1))

  ;; Track git status of files.
  (when (executable-find "git")
    (if treemacs-python-executable
        (treemacs-git-mode 'deferred)
      (treemacs-git-mode 'simple)))

  ;; Remap some window commands to their Treemacs counterparts.
  (keybind! nil 'global
    [remap delete-other-windows] #'treemacs-delete-other-windows))

;; Integrate with Evil.
(after! '(treemacs evil)
  (require 'treemacs-evil))

;; Integrate with Magit.
(after! '(treemacs magit)
  (require 'treemacs-magit))

;; Integrate with Tab-bar.
(after! '(treemacs)
  (require 'treemacs-tab-bar)
  (treemacs-set-scope-type 'Tabs))

(provide 'zy-treemacs)

;;; zy-treemacs.el ends here
