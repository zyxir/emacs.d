;;; init-common.el --- Configure common things -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set flags.

(setq auto-save-default nil
      case-fold-search nil
      case-replace nil
      disabled-command-function nil
      frame-title-format '("" "ZyEmacs" " [%b]")
      inhibit-compacting-font-caches t
      make-backup-files nil
      read-process-output-max (* 1024 1024)
      system-time-locale "C"
      use-dialog-box nil
      word-wrap-by-category t)

(setq-default fill-column 80)

;; Inbuilt modes.

(delete-selection-mode +1)
(global-auto-revert-mode +1)
(global-display-line-numbers-mode +1)
(global-subword-mode +1)
(recentf-mode +1)
(save-place-mode +1)
(load "kinsoku" 'noerror 'nomessage)


(provide 'init-common)

;;; init-common.el ends here.
