;;; init-vc.el --- Version control settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'magit)
(pkg! 'diff-hl)
(pkg! 'git-timemachine)

;; Visit symbolic links automatically.
(setq vc-follow-symlinks t)

;; "<leader> g" for git operations.
(zy/create-definer zy/leader-g-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-g-map
  :prefix "g")
(zy/leader-def
  "G" #'magit-dispatch)
(zy/leader-g-def
  "f" #'magit-file-dispatch
  "g" #'magit-status
  "t" #'git-timemachine)

;; Configure Magit.
(after-or-now! 'magit
  (setq
   ;; Do not bind keys automatically.
   magit-define-global-key-bindings nil
   ;; Show commit time in the status buffer.
   magit-status-margin '(t age magit-log-margin-width nil 18))

  ;; Recover leader key in the Magit status buffer.
  (general-unbind :keymaps 'magit-mode-map "SPC"))

;; Highlight file changes with Diff-hl.
(after-deferred! 'diff-hl
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1)
  ;; Diff on the fly.
  (diff-hl-flydiff-mode 1)
  ;; Allow mouse interaction in the fringe.
  (diff-hl-show-hunk-mouse-mode)
  ;; Also diff Dired buffers.
  (add-hook! dired-mode (diff-hl-dired-mode 1))

  ;; Turn on `diff-hl-margin-mode' in terminal frames. It is worth noting that
  ;; `diff-hl-margin-mode' is global and it's very hard, if not impossible, to
  ;; make it window-local or frame-local. So manually toggle it if this behavior
  ;; causes trouble.
  (add-hook! diff-hl-mode
    (defun zy/-maybe-turn-on-margin-for-diff-hl-h ()
      (unless (display-graphic-p)
        (diff-hl-margin-mode 1))))

  (zy/leader-t-def
    "d" #'diff-hl-mode
    "D" #'diff-hl-margin-mode)

  (zy/leader-g-def
    "s" #'diff-hl-stage-dwim
    "h" #'diff-hl-show-hunk
    "=" #'diff-hl-diff-goto-hunk
    "R" #'diff-hl-revert-hunk
    "p" #'diff-hl-previous-hunk
    "n" #'diff-hl-next-hunk
    "P" #'diff-hl-show-hunk-previous
    "N" #'diff-hl-show-hunk-next))
(provide 'init-vc)

;;; init-vc.el ends here
