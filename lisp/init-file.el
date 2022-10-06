;;; init-file.el --- File, buffer and project utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-keybinding)


;; Advanced scratch buffers with Scratch.el

(with-eval-after-load 'scratch
  (dolist (pair '((fundamental-mode . lisp-interaction-mode)
		  (emacs-lisp-mode . lisp-interaction-mode)))
    (add-to-list 'scratch-mode-alist pair)))


;; Leader buffer commands

(define-prefix-command 'zy/buffer-map)
(zy/leader-def
  "b" '(zy/buffer-map :which-key "buffer"))
(general-def zy/buffer-map
  "s" #'scratch)


;; Leader file commands

(define-prefix-command 'zy/file-map)
(zy/leader-def
  "f" '(zy/file-map :which-key "file"))
(general-def zy/file-map
  ;; Jump across files
  "b" #'consult-bookmark
  "f" #'find-file
  "r" #'consult-recent-file
  ;; File operations
  "d" #'crux-delete-file-and-buffer
  "R" #'crux-rename-file-and-buffer
  "s" #'save-buffer
  "S" #'save-some-buffers
  "w" #'write-file
  ;; Quick access
  "," #'crux-find-user-custom-file)


(provide 'init-file)

;;; init-file.el ends here.
