;;; init-misc.el --- Miscellaneous setup.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file includes miscellaneous settings that is hard to categorize.

;;; Code:

(eval-and-compile (require 'init-basic))

;; Don't grep in these directories and files.
(after-or-now! 'grep
  (setq grep-find-ignored-directories
        (append grep-find-ignored-directories '("elpa"))
        grep-find-ignored-files
        (append grep-find-ignored-files '("history"))))

(defer!
  ;; Always delete selection like other editors do.
  (delete-selection-mode 1)

  ;; Always use subword movement (move between camel case words).
  (global-subword-mode 1)

  ;; Always match parenthesis.
  (electric-pair-mode 1)

  ;; Always show matching parenthesis.
  (show-paren-mode 1)

  ;; Enable mouse support in terminal.
  (xterm-mouse-mode 1))

;; Always record recent files.
(after-deferred! recentf
  (setq
   ;; Save more recent files (default is 20).
   recentf-max-saved-items 200
   ;; Do periodic cleanup when running in daemon mode, otherwise cleanup only
   ;; when turning the mode on.
   recentf-auto-cleanup (if (daemonp) 300 'mode))
  (recentf-mode 1))

;; Always persist variables across sessions.
(after-deferred! 'savehist
  (savehist-mode 1)
  (setq savehist-additional-variables '(kill-ring
                                        register-alist
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring)))

;; Always save buffer locations.
(save-place-mode 1)

;; Display line numbers in most modes.
(setq-default
 ;; Explicitly define a width to reduce the cost of on-the-fly computation. 4 is
 ;; a good default, as most text files do not exceed 10k lines.
 display-line-numbers-width 4
 ;; Show absolute line numbers for narrowed regions to make it easier to tell
 ;; the buffer is narrowed, and where you are, exactly.
 display-line-numbers-widen t
 ;; Set this value for a consistent line number width. Read the documentation.
 display-line-numbers-width-start 60)
(add-hook! (prog-mode text-mode conf-mode) (display-line-numbers-mode 1))

(provide 'init-misc)

;;; init-misc.el ends here
