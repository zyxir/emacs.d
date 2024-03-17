;;; init-prose.el --- Distraction-free writting.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures features for prose-editing.

;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'olivetti)

;;;; Distraction Free Mode

(zy/leader-t-def
  "d" #'olivetti-mode)

(after! 'olivetti
  ;; Use larger font when Olivetti is on.
  (let ((olivetti-inc 2))
    (add-hook! olivetti-mode-on
      (text-scale-adjust olivetti-inc))
    (add-hook! olivetti-mode-off
      (text-scale-adjust (- olivetti-inc))))

  ;; HACK Save/restore window margins and Hl-diff state when toggling Olivetti.

  (defadvice! zy/-olivetti-remember-a (&rest _)
    "Remember original states upon entering `olivetti-mode'."
    :before #'olivetti-mode
    (unless olivetti-mode
      (setq-local zy/olivetti-original-margins
                  (window-margins)
                  zy/olivetti-original-diff-hl
                  (bound-and-true-p diff-hl-mode)
                  zy/olivetti-original-diff-hl-margin
                  (bound-and-true-p diff-hl-margin-mode))
      (when (fboundp 'diff-hl-mode)
        (diff-hl-mode -1))))

  (defadvice! zy/-olivetti-restore-per-window-a (window-or-frame)
    "Restore window margins upon exiting `olivetti-mode'.
WINDOW-OR-FRAME is the window or frame Olivetti is currently
setting."
    :after #'olivetti-set-window
    (unless olivetti-mode
      (when (and (boundp 'zy/olivetti-original-margins)
                 (windowp window-or-frame))
        (set-window-margins window-or-frame
                            (car zy/olivetti-original-margins)
                            (cdr-safe zy/olivetti-original-margins)))))

  (defadvice! zy/-olivetti-restore-a (&rest _)
    "Restore Diff-hl settings upon exiting `olivetti-mode'."
    :after #'olivetti-mode
    (unless olivetti-mode
      (when (and (fboundp 'diff-hl-mode)
                 (bound-and-true-p zy/olivetti-original-diff-hl))
        (diff-hl-mode 1)
        (when (bound-and-true-p zy/olivetti-original-diff-hl-margin)
          (diff-hl-margin-mode 1))))))

(provide 'init-prose)

;;; init-prose.el ends here
