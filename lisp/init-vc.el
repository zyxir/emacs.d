;;; init-vc.el --- Version control settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'magit)
(require-package 'diff-hl)
(require-package 'git-timemachine)

;; Visit symbolic links automatically.
(setq vc-follow-symlinks t)

;; Configure Magit.
(setq
 ;; Do not bind keys automatically.
 magit-define-global-key-bindings nil
 ;; Show commit time in the status buffer.
 magit-status-margin '(t age magit-log-margin-width nil 18))

;; Recover leader key in Magit.
(general-unbind :keymaps 'magit-mode-map "SPC")

;; Highlight file changes with Diff-hl.
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-diff-hl-mode 1)
;; Diff on the fly.
(diff-hl-flydiff-mode 1)
;; Use the left margin to show diff, which supports terminal.
(diff-hl-margin-mode 1)
;; Allow mouse interaction in the fringe.
(diff-hl-show-hunk-mouse-mode)
;; Also diff Dired buffers.
(add-hook! dired-mode (diff-hl-dired-mode 1))

;; "<leader> g" for git operations.
(zy/create-definer zy/leader-g-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-g-map
  :prefix "g")
(zy/leader-g-def
  "f" #'magit-file-dispatch
  "g" #'magit-status
  "t" #'git-timemachine
  "s" #'diff-hl-stage-dwim
  "h" #'diff-hl-show-hunk
  "=" #'diff-hl-diff-goto-hunk
  "R" #'diff-hl-revert-hunk
  "p" #'diff-hl-previous-hunk
  "n" #'diff-hl-next-hunk
  "P" #'diff-hl-show-hunk-previous
  "N" #'diff-hl-show-hunk-next)
(zy/leader-def
  "G" #'magit-dispatch)

(provide 'init-vc)

;;; init-vc.el ends here
