;;; init-keybinding.el --- Setup key bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-load)


;; Default key tweaks

(general-def
  [remap move-beginning-of-line] #'crux-move-beginning-of-line
  [remap just-one-space] #'cycle-spacing
  [remap delete-horizontal-space] #'cycle-spacing)


;; Leader key setup

(general-create-definer zy/leader-def :prefix "C-c")
(general-def "C-z" (general-simulate-key "C-c"))
(general-def "M-m" (general-simulate-key "C-c"))

;; Several prefix commands

(define-prefix-command 'zy/edit-map)
(define-prefix-command 'zy/manage-map)
(zy/leader-def
  "e" #'(zy/edit-map :which-key "edit")
  "m" #'(zy/manage-map :which-key "manage"))


;; Use Which-key to provide hints

(run-with-idle-timer
 0.5 nil
 (lambda ()
   (require 'which-key)
   (which-key-mode)
   (blackout 'which-key-mode)))


(provide 'init-keybinding)

;;; init-keybinding.el ends here.
