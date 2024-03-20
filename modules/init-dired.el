;;; init-dired.el --- Dired settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'dired-sidebar)

(zy/leader-d-def
  "d" #'dired-sidebar-toggle-sidebar)

;; Configure Dired.
(after-or-now! 'dired
  (setq
   ;; Revert Dired buffers if the directory has changed.
   dired-auto-revert-buffer 'dired-directory-changed-p
   ;; Guess the target directory.
   dired-dwim-target t
   ;; Command line switches used for `ls'.
   dired-listing-switches (eval-when-compile
                            (string-join
                             '(;; Natural sort of version numbers.
                               "-v"
                               ;; Group directories and show them first.
                               "--group-directories-first"
                               ;; Show human readable sizes.
                               "--human-readable"
                               ;; Show ISO 8601 timestamps.
                               "--time-style=long-iso"
                               ;; Must be included for dired.
                               "-l")
                             " "))
   ;; Make directories at the title bar clickable.
   dired-make-directory-clickable t
   ;; Allow mouse to drag files.
   dired-mouse-drag-files t
   ;; Do not ask for recursive operations, just like any other modern file
   ;; manager will do.
   dired-recursive-copies 'always
   dired-recursive-deletes 'always)

  ;; Auto revert Dired buffer.
  (add-hook! dired-mode
    (defun zy/-dired-auto-revert-a ()
      "Auto revert Dired buffer if not on remote."
      (unless (file-remote-p default-directory)
        (auto-revert-mode 1)))))

;; Configure Dired-sidebar.
(after-or-now! 'dired-sidebar)

(provide 'init-dired)

;;; init-dired.el ends here
