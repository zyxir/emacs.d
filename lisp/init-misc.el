;;; init-misc.el --- Miscellaneous setup.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file includes simple settings that is hard to categorize.

;;; Code:

(require-package 'gcmh)

;; Enable GCMH to reduce GC lags.
(gcmh-mode 1)

(setq
 ;; Do not create auto save or backup files.
 auto-save-default nil
 make-backup-files nil
 ;; Suppress warning messages generated by ad-handle-definition.
 ad-redefinition-action 'accept
 ;; By default, Emacs asks the user if it should kill processes on exit. VS Code
 ;; does not do that, and I find it annoying to do that, so just let Emacs kill
 ;; processes automatically.
 confirm-kill-processes nil
 ;; Also kill buffers which have a running process without confirmation.
 kill-buffer-query-functions (delq 'process-kill-buffer-query-function
				   kill-buffer-query-functions)
 ;; As an experienced Emacs user, I don't want any command disabled.
 disabled-command-function nil
 ;; A informational frame title. Besides, my AutoHotkey scripts recognize my
 ;; Emacs window by the \"ZyEmacs\" prefix.
 frame-title-format '((if *wsl* "ZyEmacsWSL@" "ZyEmacs@")
                      (:eval (or
                              (file-remote-p default-directory 'host)
                              system-name))
                      " [%b]")
 ;; I usually use a Hybrid font like Sarasa Gothic, which contains tremendous
 ;; amout of CJK glyphs. Disable compacting of font makes redisplay faster.
 inhibit-compacting-font-caches t
 ;; Do not report native compilation warnings and errors. Those do not matter in
 ;; most occasions.
 native-comp-async-report-warnings-errors nil
 ;; The default (4 kB) is too low considering some language server responses are
 ;; in 800 kB to 3 MB. So it is set to 1 MB as suggested by Lsp-mode.
 read-process-output-max (* 1024 1024)
 ;; Save existing clipboard text into kill ring before replacing it. This saves
 ;; me so many time!
 save-interprogram-paste-before-kill t
 ;; Uniquify buffer names in a saner way.
 uniquify-buffer-name-style 'forward
 ;; Do not use GUI dialog boxes (they cause questions on WSLg).
 use-dialog-box nil)

;; Always delete selection like other editors do.
(delete-selection-mode 1)

;; Always use subword movement (move between camel case words).
(global-subword-mode 1)

;; Always match parenthesis.
(electric-pair-mode 1)

;; Always show matching parenthesis.
(show-paren-mode 1)

;; Always record recent files.
(recentf-mode 1)

;; Always persist variables across sessions.
(setq savehist-additional-variables '(kill-ring
                                      register-alist
                                      mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring))
(savehist-mode 1)

;; Always save buffer locations.
(save-place-mode 1)

;; Enable mouse support in a terminal.
(xterm-mouse-mode 1)

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
