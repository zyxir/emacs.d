;;; zy-python.el --- Python development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+python' module of the configuration.

;; Python is a very popular language, and Emacs has decent built-in support for
;; it. This file simply rebinds some of the keys and wrap some features to make
;; them more smooth.

;;; Code:

(require 'zylib)

(pkg! 'python-black)

(after! 'python
  ;; Use the Black profile for Isort.
  (advice-add
   #'python-sort-imports :override
   (defun +python-sort-imports-a ()
     "Sort Python imports in the current buffer.
Always use the Black profile."
     (interactive)
     (if (python--do-isort "--profile" "black")
         (message "Sorted imports")
       (message "(No changes in Python imports needed)"))))

  (eval-and-compile
    ;; Start and switch to the Python shell.
    (defun +python-switch-to-python-shell ()
      "Switch to inferior Python process buffer.
When there is no Python process running, start one before
switching to it."
      (interactive)
      (when-let ((proc (python-shell-get-process)))
        (unless proc
          (run-python)
          (setq proc (python-shell-get-process)))
        (pop-to-buffer (process-buffer proc))))

    ;; Wrap some commands to mimic `eglot-format'.
    (defun +python-format (&optional beg end)
      "Format region BEG END.
This uses `python-black-buffer' or `python-black-region'."
      (interactive (and (region-active-p)
                        (list (region-beginning) (region-end))))
      (if beg
          (python-black-region beg end)
        (python-black-buffer))))

  (defprefix! +python-map "Python"
              nil python-base-mode-map "<localleader>")

  (defprefix! +python-import-map "Import"
              nil +python-map "i"
    "a" (cons "Add" #'python-add-import)
    "f" (cons "Fix" #'python-fix-imports)
    "r" (cons "Remove" #'python-remove-import)
    "s" (cons "Sort" #'python-sort-imports))

  (defprefix! +python-shell-map "Shell"
              nil +python-map "s"
    "s" (cons "Open" #'+python-switch-to-python-shell)
    "R" (cons "Restart" #'python-shell-restart)
    "b" (cons "Send Buffer" #'python-shell-send-buffer)
    "f" (cons "Send File" #'python-shell-send-file)
    "r" (cons "Send Region" #'python-shell-send-region)
    "d" (cons "Send Defun" #'python-shell-send-defun)
    "e" (cons "Send Statement" #'python-shell-send-statement))

  ;; Remap some code actions in Python mode.
  (add-hook! 'python-base-mode-hook
    (keybind! nil 'local
      [remap zy/do-format] #'+python-format
      [remap zy/do-organize-imports] #'python-sort-imports)))

(provide 'zy-python)

;;; zy-python.el ends here
