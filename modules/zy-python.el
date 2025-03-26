;;; zy-python.el --- Python development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+python' module of the configuration.

;; Python is a very popular language, and Emacs has decent built-in support for
;; it. This file simply rebinds some of the keys and wrap some features to make
;; them more smooth.

;;; Code:

(require 'zylib)

(pkg! 'python-black)
(pkg! 'python-pytest)

(after! 'python
  ;; Fill docstring with according to PEP-257.
  (setq python-fill-docstring-style 'pep-257-nn)

  ;; No additional indentation for def blocks, like Black does.
  (setq python-indent-def-block-scale 1)

  ;; Set the compile command to executing the file itself in the project root.
  (add-hook! 'python-base-mode-hook
    (defun +python-set-compile-command-h (&rest _)
      "Set up compile command."
      (when (bound-and-true-p buffer-file-name)
        ;; Set up the compile command.
        (setq-local compile-command (format "python %s" buffer-file-name))
        ;; Do not read an explicit command since we have set an implicit one. If
        ;; we want to use another command, use a prefix argument.
        (setq-local compilation-read-command nil))))

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
switching to it.

When in a project, start one associated with the project."
      (interactive)
      (unless (python-shell-get-process)
        (run-python (python-shell-calculate-command)
                    (if (project-current) 'project nil)))
      (pop-to-buffer (process-buffer (python-shell-get-process))))

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
              nil python-base-mode-map "<localleader>"
    "s" (cons "Open" #'+python-switch-to-python-shell)
    "R" (cons "Restart" #'python-shell-restart)
    "b" (cons "Send Buffer" #'python-shell-send-buffer)
    "f" (cons "Send File" #'python-shell-send-file)
    "r" (cons "Send Region" #'python-shell-send-region)
    "d" (cons "Send Defun" #'python-shell-send-defun)
    "e" (cons "Send Statement" #'python-shell-send-statement)
    "t" (cons "Pytest" #'python-pytest-dispatch))

  (defprefix! +python-import-map "Import"
              nil +python-map "i"
    "a" (cons "Add" #'python-add-import)
    "f" (cons "Fix" #'python-fix-imports)
    "r" (cons "Remove" #'python-remove-import)
    "s" (cons "Sort" #'python-sort-imports))

  ;; Remap some code actions in Python mode.
  (keybind! nil python-base-mode-map
    [remap +leader-do-format] #'+python-format
    [remap +leader-do-organize-imports] #'python-sort-imports))

(provide 'zy-python)

;;; zy-python.el ends here
