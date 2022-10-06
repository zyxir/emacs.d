;;; init-project.el --- Project settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-keybinding)
(require 'init-search)

;; Command on a switched project

(with-eval-after-load 'project
  (setq project-switch-commands '((project-find-file "Find file" "f")
				  (consult-ripgrep "Ripgrep" "g")
				  (magit-project-status "Magit" "m")
				  (project-find-dir "Find directory" "d")
				  (project-eshell "Eshell" "s"))))

;; Leader project commands

(define-prefix-command 'zy/project-map)
(zy/leader-def
  "p" '(zy/project-map :which-key "project"))
(general-def ctl-x-map
  "p" #'zy/project-map)
(general-def zy/project-map
  ;; Standard project commands
  "!" #'project-shell-command
  "&" #'project-async-shell-command
  "D" #'project-dired
  "F" #'project-or-external-find-file
  "G" #'rg-project
  "b" #'project-switch-to-buffer
  "c" #'project-compile
  "d" #'project-find-dir
  "e" #'project-eshell
  "f" #'project-find-file
  "g" #'consult-ripgrep
  "k" #'project-kill-buffers
  "m" #'magit-project-status
  "p" #'project-switch-project
  "r" #'project-query-replace-regexp
  "s" #'project-shell
  "v" #'project-vc-dir
  "x" #'project-execute-extended-command)


(provide 'init-project)

;;; init-project.el ends here.
