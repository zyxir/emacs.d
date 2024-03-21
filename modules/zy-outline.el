;;; zy-outline.el --- File outlining. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+outline' module of the configuration.

;; Outline is a built-in feature of Emacs. It enables structuring the buffer
;; into sections and collapse/expand grammatical parts.
;;
;; This file enables toggling `outline-minor-mode' with a key, but even when the
;; minor mode is not enabled, it is possible to jump to any outline heading with
;; `consult-outline', which is bound to "g o" in normal state in the `+evil'
;; module.

;;; Code:

(require 'zylib)

;; Toggle `outline-minor-mode' with key.
(after! '+leader
  (keybind! nil +leader-y-map
    "o" #'outline-minor-mode))

(after! 'outline
  ;; Always show a button when using outline.
  (setq outline-minor-mode-use-buttons t))

(provide 'zy-outline)

;;; zy-outline.el ends here
