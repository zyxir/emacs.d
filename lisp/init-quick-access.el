;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Quick accessing several important files.

;;; Code:

;; Quick accessing is only available when Zybox is available.

(when zy/zybox-path
  (defvar zy/quick-access-choices nil
    "A list of quick access shortcuts, names and paths.")
  (setq zy/quick-access-choices
	`((?z "Zybox" ,zy/zybox-path)
	  (?p "Projects" ,zy/projects-path)
	  (?s "Std-Proj" ,zy/std-proj-path)))

  (defun zy/quick-access (arg)
    "`find-file' a quick access path if ARG is nil.

Prefix it with C-u to `find-file-other-window'. And prefix it with double C-u
to `file-file-other-frame'.

Quick access paths are defined in `zy/quick-access-choices'"
    (interactive "P")
    (let* ((find-file-function
	    (cond
	     ((equal arg '(4)) 'find-file-other-window)
	     ((equal arg '(16)) 'find-file-other-frame)
	     (t 'find-file)))
	   (choice
	    (read-multiple-choice
	     "Choose a quick access target."
	     zy/quick-access-choices))
	   (target
	    (nth 2 choice)))
      (funcall find-file-function target)))

  (general-define-key "C-c o" #'zy/quick-access))

;; End of config.

(provide 'init-quick-access)
