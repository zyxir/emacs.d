;;; zy-persist.el --- Persist variables across sessions. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+persist' module of the configuration.

;; Most modern text editors persist certain information across sessions,
;; including recently visited files, cursor positions in files, clipboard,
;; search history, and so on. Emacs does not do this by default, but it can do
;; this with several minor modes.

;;; Code:

(require 'zylib)

;; Enable these persisting modes at startup.
(add-hook! 'window-setup-hook
  ;; Record recently visited files.
  (recentf-mode 1)

  ;; Persist variables.
  (savehist-mode 1)

  ;; Save cursor locations in files.
  (save-place-mode 1))

(after! 'recentf
  ;; Save more recent files (default is 20).
  (setq recentf-max-saved-items 200)
  ;; Do periodic cleanup when running in daemon mode, otherwise cleanup only
   ;; when turning the mode on.
  (setq recentf-auto-cleanup (if (daemonp) 300 'mode)))

(after! 'savehist
  ;; Savehist only saves minibuffer history by default. I want these variables
  ;; also saved.
  (setq savehist-additional-variables '(kill-ring
                                        register-alist
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring)))

(provide 'zy-persist)

;;; zy-persist.el ends here
