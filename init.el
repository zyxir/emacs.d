(defvar my/init.org-message-depth 3
  "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file (expand-file-name "init.org"
				 user-emacs-directory))
  (goto-char (point-min))
  (search-forward "\n* Emacs Configuration")
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report headers.
     ((looking-at
       (format "\\*\\{2,%s\\} +.*$"
	       my/init.org-message-depth))
      (message "%s" (match-string 0)))
     ;; Evaluate code blocks.
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp.*$*")
      (let ((l (match-end 0)))
	(search-forward "\n#+END_SRC")
	(eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header.
     ((looking-at "^\\* ")
      (goto-char (point-max))))))
