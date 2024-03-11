;;; init-highlight.el --- Highlighting text.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Highlight todo and similar keywords.
(hl-todo-mode 1)

;; Jump between highlighted keywords.
(general-def
  :states 'normal
  "[t" #'hl-todo-previous
  "]t" #'hl-todo-next)

(provide 'init-highlight)

;;; init-highlight.el ends here
