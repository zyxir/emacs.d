;;; init-highlight.el --- Highlighting text.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'hl-todo)
(require-package 'consult-todo)

;; Highlight todo and similar keywords.
(global-hl-todo-mode 1)

;; Jump between keywords.
(general-def
  :states 'normal
  "[t" #'hl-todo-previous
  "]t" #'hl-todo-next)

(provide 'init-highlight)

;;; init-highlight.el ends here
