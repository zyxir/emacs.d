;;; zy-eldoc.el --- Doc in echo area. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+eldoc' module of the configuration.

;; Eldoc is the built-in utility that displays documentation in the echo area.
;; With simple tweaking it can become very powerful and handy.

;; Eldoc-box is used to display eldoc in a popup frame when needed.

;;; Code:

(require 'zylib)

(daemon-require! 'eldoc)
(pkg! 'eldoc-box)

(after! 'eldoc
  ;; Display multiple Eldoc sources simultaneously, and display each source
  ;; eagerly as long as it's ready. This is useful if we want to display
  ;; function help and syntax checker reports simultaneously.
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Show Eldoc in a dedicated buffer with a handy keychord.
  (after! '+leader
    (keybind! nil +leader-h-map "." '("At Point" . eldoc-doc-buffer)))

  ;; Toggle eldoc-box with M-h.
  (keybind! nil 'global "M-h" '("Eldoc Box" . eldoc-box-help-at-point)))

(provide 'zy-eldoc)

;;; zy-eldoc.el ends here
