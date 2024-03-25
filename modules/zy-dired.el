;;; zy-dired.el --- Dired settings. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+dired' module of the configuration.

;; Dired is the built-in file manager of Emacs. It is very powerful, providing
;; features like multi-file operations, displaying advanced file information,
;; and text-editing-like mass file renaming. Dired-sidebar leverages the
;; advantage of Dired by putting it into a sidebar, providing a modern
;; file-exploring sidebar like any other text editor does.

;;; Code:

(require 'zylib)

(pkg! 'dired-sidebar)

(after! '+leader
  (keybind! nil +leader-map
    "D" (cons "Dired Sidebar" #'dired-sidebar-toggle-sidebar)))

(daemon-require! 'dired 'dired-sidebar)

(after! 'dired
  ;; Revert Dired buffers on re-visits if the directory has changed.
  (setq dired-auto-revert-buffer 'dired-directory-changed-p)

   ;; Guess the target directory if there is another Dired buffer open in
   ;; another window.
  (setq dired-dwim-target t)

   ;; Command line switches used for `ls'.
  (setq dired-listing-switches (eval-when-compile
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
                                  " ")))

   ;; Make directories at the title bar clickable.
   (setq dired-make-directory-clickable t)

   ;; Allow using mouse to drag files.
   (setq dired-mouse-drag-files t)

   ;; Do not ask for recursive operations, just like any other modern file
   ;; manager does.
   (setq dired-recursive-copies 'always
         dired-recursive-deletes 'always)

   (add-hook! 'dired-mode-hook
     (defun +dired-auto-revert-a ()
       "Auto revert Dired buffers if not on remote."
       (unless (file-remote-p default-directory)
         (auto-revert-mode 1)))))

(provide 'zy-dired)

;;; zy-dired.el ends here
