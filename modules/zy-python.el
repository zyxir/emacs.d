;;; zy-python.el --- Python development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+python' module of the configuration.

;; Python is a very popular language, and Emacs has decent built-in support for
;; it. This file simply rebinds some of the keys and wrap some features to make
;; them more smooth.

;;; Code:

(require 'zylib)

(pkg! 'python-black)
(pkg! 'python-docstring)

(after! 'python
  ;; Fill docstring with according to PEP-257. However I am not sure whether
  ;; this is overriden by the `python-docstring' package.
  (setq python-fill-docstring-style 'pep-257-nn)

  ;; `python-docstring-mode' remaps `fill-paragraph' to its own command, which
  ;; is bad. I think that setting `fill-paragraph-function' is a better and
  ;; safer way.
  (add-hook! 'python-base-mode-hook
    (setq-local fill-paragraph-function #'python-docstring-fill))

  ;; No additional indentation for def blocks, like Black does.
  (setq python-indent-def-block-scale 1)

  ;; Call "isort" instead of "python -m isort".
  (advice-add
   #'python--do-isort :override
   (defun +python-do-isort-a (&rest args)
     "Edit the current buffer using isort called with ARGS.
Return non-nil if the buffer was actually modified."
     (let ((buffer (current-buffer)))
       (with-temp-buffer
         (let ((temp (current-buffer)))
           (with-current-buffer buffer
             (let ((status (apply #'call-process-region
                                  (point-min) (point-max)
                                  "isort"
                                  nil (list temp nil) nil
                                  "-" args))
                   (tick (buffer-chars-modified-tick)))
               (unless (eq 0 status)
                 (error "%s exited with status %s" "isort" status))
               (replace-buffer-contents temp)
               (not (eq tick (buffer-chars-modified-tick))))))))))

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
      (let ((proc (python-shell-get-process)))
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
  (keybind! nil python-base-mode-map
    [remap +leader-do-format] #'+python-format
    [remap +leader-do-organize-imports] #'python-sort-imports))

(provide 'zy-python)

;;; zy-python.el ends here
