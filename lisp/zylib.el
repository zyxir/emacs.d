;;; zylib.el --- Useful commands.  -*- lexical-binding: t -*-

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen
;; Version: 0.1
;; Package-Requires: ()
;; Homepage:
;; Keywords: commands

;;; Commentary:

;; This file provides autoloaded commands available to other modules of Zyxir's
;; Emacs config.

;;; Code:

;;;; File-Related

;;;###autoload
(defun zy/delete-file-and-buffer (&optional trash)
  "Kill the current buffer and deletes the file it is visiting.

If TRASH is non-nil, move the file to trash."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;;###autoload
(defun zy/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (when-let* ((filename (buffer-file-name))
              (new-name (or (read-file-name "New name: "
                                            (file-name-directory filename)
                                            nil 'confirm)))
              (containing-dir (file-name-directory new-name)))
    ;; Make sure the current buffer is saved and backed by some file.
    (when (or (buffer-modified-p) (not (file-exists-p filename)))
      (when (y-or-n-p "You have to save it before moving it. Save now? ")
        (save-buffer)))
    (if (get-file-buffer new-name)
        (message "There is already a buffer named %s" new-name)
      (progn
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename)
          (let ((vc-filename (if (tramp-tramp-file-p filename)
                                 (tramp-file-local-name filename)
                               filename))
                (vc-new-name (if (tramp-tramp-file-p new-name)
                                 (tramp-file-local-name filename)
                               new-name)))
            (vc-rename-file vc-filename vc-new-name)))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(provide 'zylib)

;;; zylib.el ends here
