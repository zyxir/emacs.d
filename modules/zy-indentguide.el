;;; zy-indentguide.el --- Indentation guides. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+indentguide' module of the configuration.

;; Indentation guidelines are useful while editing complicated, nested code.
;; This module sets them up via the Highlight-indent-guides package, an outdated
;; but reliable package.
;;
;; It is worth noting that that is no Emacs package for indentation guidelines
;; that is both performant, accurate, and pretty. Highlight-indent-guide is used
;; by Doom Emacs and is relatively more reliable, but it is far from robust. Two
;; new packages, Indent-bars and Hl-indent-scope, look promising, but are far
;; from being reliable by now (as of 2024-04-01). The `+indentbar' module is
;; written around Indent-bars, but it is disabled currently. Hope these new
;; packages become better.

;;; Code:

(require 'zylib)

(pkg! 'highlight-indent-guides)

;; Enable indent guides for all prog-modes, except those Lisp modes. Lisp modes
;; use parenthesis for scopes, thus having little need for indentation guides.
;;
;; If we enable this too early, enabling the minor mode for the scratch buffer
;; while running as a daemon will make Emacs unable to start (and the actual
;; reason is still unknown to me). However, doing this prevent the scratch
;; buffer, which is created before a frame is created, from enabling the minor
;; mode. This is not a problem though, considering we don't want indentation
;; guidelines for Lisp modes.
(after-frame!
  (add-hook! 'prog-mode-hook
    (defun +indentguide-activate-h (&rest _)
      "Activate indent guides for any non-Lisp prog-mode."
      (unless (derived-mode-p 'lisp-data-mode)
        (highlight-indent-guides-mode 1)))))

(after! 'highlight-indent-guides
  ;; Use characters for highlighting, which is conventional.
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character #x2502)

  ;; Use colors from the background.
  (set-face-attribute 'highlight-indent-guides-character-face nil
                      :foreground
                      (lighten! (face-attribute 'shadow :foreground) 120))
  (set-face-attribute 'highlight-indent-guides-top-character-face nil
                      :foreground
                      (lighten! (face-attribute 'shadow :foreground) 40))

  ;; Highlight the current indentation.
  (setq highlight-indent-guides-responsive 'top))

(after! '(highlight-indent-guides +leader)
  (keybind! nil +leader-y-map
    "i" (cons "Indent Guide" #'highlight-indent-guides-mode)))

(provide 'zy-indentguide)

;;; zy-indentguide.el ends here
