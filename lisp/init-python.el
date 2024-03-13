;;; init-python.el --- Python development.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'python-black)

(with-eval-after-load 'python
  ;; Use the Black profile for Isort.
  (defadvice! zy/python-sort-imports-a ()
    "Sort Python imports in the current buffer.
Always use the Black profile."
    :override #'python-sort-imports
    (interactive)
    (if (python--do-isort "--profile" "black")
        (message "Sorted imports")
      (message "(No changes in Python imports needed)")))

  ;; Wrap some commands to mimic `eglot-format'.
  (defun zy/python-format (&optional beg end)
    "Format region BEG END.
This uses `python-black-buffer' or `python-black-region'."
    (interactive (and (region-active-p)
                      (list (region-beginning) (region-end))))
    (if beg
        (python-black-region beg end)
      (python-black-buffer)))

  (defun zy/switch-to-python-shell ()
    "Switch to inferior Python process buffer.
When there is no Python process running, start one before
switching to it."
    (interactive)
    (let ((proc (python-shell-get-process)))
      (unless proc
        (run-python)
        (setq proc (python-shell-get-process)))
      (pop-to-buffer (process-buffer proc))))

  ;; Local keys.
  (zy/local-leader-def
    ;; Import management.
    "i a" #'python-add-import
    "i f" #'python-fix-imports
    "i r" #'python-remove-import
    "i s" #'python-sort-imports
    ;; Inferior Python shell.
    "s s" #'zy/switch-to-python-shell
    "s R" #'python-shell-restart
    "s b" #'python-shell-send-buffer
    "s f" #'python-shell-send-file
    "s r" #'python-shell-send-region
    "s d" #'python-shell-send-defun
    "s e" #'python-shell-send-statement)

  ;; Remap some code actions in Python mode
  (add-hook! python-base-mode
    (general-def
      :keymaps 'local
      [remap zy/do-format] #'zy/python-format
      [remap zy/do-organize-imports] #'python-sort-imports)))

(provide 'init-python)

;;; init-python.el ends here
