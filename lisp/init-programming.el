;;; init-programming.el --- Features for writing programs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-keybinding)
(require 'init-load)


;; Setup Flycheck for syntax checking

(zy/delay-till after-find-file
 (setq-default flycheck-emacs-lisp-load-path 'inherit)
 (global-flycheck-mode +1)
 (general-def flycheck-mode-map
   "M-p" #'flycheck-previous-error
   "M-n" #'flycheck-next-error))


;; Setup Yasnippet

(zy/delay-till after-find-file
 (require 'yasnippet)
 (yas-global-mode +1)
 (blackout 'yas-minor-mode)
 (general-def zy/edit-map
   "s" #'consult-yasnippet
   "S" #'consult-yasnippet-visit-snippet-file))


(provide 'init-programming)

;;; init-programming.el ends here
