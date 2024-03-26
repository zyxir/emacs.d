;;; zy-git.el --- Git integration. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+git' module of the configuration.

;; This module makes Git available in various aspects of the editing experience.
;; You can perform all kinds of Git operations with a keyboard-driven interface
;; via Magit, viewing and operating on changes in real time via Hl-diff, and
;; view the file in any historic state via Git-timemachine.

;;; Code:

(require 'zylib)

(pkg! 'magit)
(pkg! 'diff-hl)
(pkg! 'git-timemachine)

(after! '+leader
  (keybind! nil +leader-f-map
    "g" (cons "Git" #'magit-file-dispatch))

  (keybind! nil +leader-g-map
    "g" (cons "Magit" #'magit)
    "f" (cons "File" #'magit-file-dispatch)
    "F" (cons "Pull" #'magit-pull)
    "t" (cons "Timemachine" #'git-timemachine)
    "h" (cons "Show Hunk" #'diff-hl-show-hunk))

  (keybind! nil +leader-y-map
    "d" (cons "Diff Fringe" #'diff-hl-mode)))

;; Magit is used very frequently. Load it at startup if running as a daemon.
(daemon-require! 'magit)

(after! 'magit
  ;; Do not bind keys automatically.
  (setq magit-define-global-key-bindings nil)

  ;; Show commit time in the status buffer.
  (setq magit-status-margin '(t age magit-log-margin-width nil 18)))

;; Turn Diff-hl on globally (for every file managed with Git).
(global-diff-hl-mode 1)

;; Diff-hl is almost always used while editing code, so load it immediately
;; if running as a daemon.
(daemon-require! 'diff-hl)

(after! 'diff-hl
  ;; Synchronize Git status with Magit.
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  ;; Diff as you type.
  (diff-hl-flydiff-mode 1)

  ;; Allow mouse interaction in the fringe.
  (diff-hl-show-hunk-mouse-mode 1)

  ;; Also diff Dired buffers (to indicate file changes).
  (add-hook! 'dired-mode-hook (diff-hl-dired-mode 1))

  ;; Turn on `diff-hl-margin-mode' in terminal frames. It is worth noting that
  ;; `diff-hl-margin-mode' is global and it's very hard, if not impossible, to
  ;; make it window-local or frame-local. So manually toggle it if this behavior
  ;; causes trouble.
  (add-hook! 'diff-hl-mode-hook
    (defun +git-maybe-turn-on-margin-for-diff-hl-h ()
      (unless (display-graphic-p)
        (diff-hl-margin-mode 1)))))

(provide 'zy-git)

;;; zy-git.el ends here
