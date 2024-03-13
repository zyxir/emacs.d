;;; init-highlight.el --- Highlighting text.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'hl-todo)
(require-package 'consult-todo)

;; Highlight todo and similar keywords.
(global-hl-todo-mode 1)

;; Jump between keywords.
(general-def
  :states 'normal
  "[t" #'hl-todo-previous
  "]t" #'hl-todo-next)

;; Jump to keywords with consult.
(general-def
  :states 'motion
  "g k" #'consult-todo
  "g K" #'consult-todo-project)

(provide 'init-highlight)

;;; init-highlight.el ends here
