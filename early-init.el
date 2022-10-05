;;; early-init.el --- Pre-initialization config

;;; Commentary:
;;; Code:

(setq default-frame-alist '(;; Disable menu bar.
                            (menu-bar-lines . nil)
                            ;; Disable scroll bars.
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            ;; Disable tool bar.
                            (tool-bar-lines . 0))
      menu-bar-mode nil
      scroll-bar-mode nil
      tool-bar-mode nil
      frame-inhibit-implied-resize t
      inhibit-startup-message t
      native-comp-async-report-warnings-errors nil
      pacakge-enable-at-startup nil)


;; Automatically compile Emacs Lisp libraries

(setq load-prefer-newer t)
(dolist (pkg-name '("auto-compile" "compat" "packed"))
  (add-to-list 'load-path
	       (expand-file-name (format "lib/%s" pkg-name)
				 user-emacs-directory)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)


(provide 'early-init)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
