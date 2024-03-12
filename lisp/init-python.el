;;; init-python.el --- Python development.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'python-black)

(with-eval-after-load 'python
  (zy/local-leader-def
    ;; Import management.
    "i a" #'python-add-import
    "i f" #'python-fix-imports
    "i r" #'python-remove-import
    "i s" #'python-sort-imports
    ;; TODO Inferior Python shell.
    )

  ;; Use the Black profile for Isort.
  (defadvice! zy/python-sort-imports-a ()
    "Sort Python imports in the current buffer.
Always use the Black profile."
    :override #'python-sort-imports
    (interactive)
    (if (python--do-isort "--profile" "black")
        (message "Sorted imports")
      (message "(No changes in Python imports needed)"))))

(provide 'init-python)

;;; init-python.el ends here
