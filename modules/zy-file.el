;;; zy-file.el --- File-visiting behaviors. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+file' module of the configuration.

;; Like project management, there are some things that a text editor does to
;; individual files. This module controls such behaviors of Emacs.

;;; Code:

(require 'zylib)

;; Always auto revert files if it is not remote.
(add-hook! 'find-file-hook
  (defun +file-auto-revert-a ()
    "Auto revert a buffer if it is not remote."
    (unless (file-remote-p (buffer-file-name))
      (auto-revert-mode 1))))

(defun +file-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(after! '+leader
  (keybind! nil +leader-f-map
    "D" (cons "Delete" #'+file-delete-file-and-buffer)))

(provide 'zy-file)

;;; zy-file.el ends here
