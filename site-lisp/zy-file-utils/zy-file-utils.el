;;; zy-file-utils.el --- Handy file manipulation utilities. -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2021 Eric Zhuo Chen


;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Created: 2021-09-28

;; This file is not part of GNU Emacs.


(defun zy/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(progn
	  (delete-file filename)
	  (message "Delete file %s" filename)
	  (kill-buffer))))))

(defun zy/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(provide 'zy-file-utils)
