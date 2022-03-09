;;; zy-commands.el --- Handy commands for ZyEmacs.


;; Windows and Frames.

;;;###autoload
(defun zy/transpose-windows (arg)
  "Transpose the buffers shown in two windows.

If ARG is positive, swap the current window with the ARG-th next
window; otherwise, swap with the ARG-th previous window."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))


;; Buffers and Files.

;;;###autoload
(defun zy/rename-file-and-buffer ()
  "Rename both current buffer and the file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a valid file!" name)
      (let ((new-name (read-file-name "New name: "
				      (file-name-directory filename)
				      basename
				      nil
				      basename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))
