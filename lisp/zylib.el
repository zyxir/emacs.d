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

(provide 'zylib)

;;; zylib.el ends here
