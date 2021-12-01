;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about LaTeX files.

;;; Code:

(use-package tex
  :defer t
  :straight auctex
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX"
				   "%`xelatex%(mode)%' %t"
				   TeX-run-TeX
				   nil
				   t))
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-show-compilation t
	TeX-command-force "LaTeX")
  (setq-default TeX-master nil)
  ;; From URL `https://emacs.stackexchange.com/questions/38258/close-latex-compilation-window-when-successful'
  (setq TeX-buf-close-at-warnings-only t)
  (defun my-tex-close-TeX-buffer (_output)
    "Close compilation buffer if there are no errors.
Hook this function into `TeX-after-compilation-finished-functions'."
    (let ((buf (TeX-active-buffer)))
      (when (buffer-live-p buf)
	(with-current-buffer buf
          (when (progn (TeX-parse-all-errors)
                       (or
			(and TeX-buf-close-at-warnings-only
                             (null (cl-assoc 'error TeX-error-list)))
			(null TeX-error-list)))
            (cl-loop for win in (window-list)
                     if (eq (window-buffer win) (current-buffer))
                     do (delete-window win)))))))

  (add-hook 'TeX-after-compilation-finished-functions #'my-tex-close-TeX-buffer))

;; End of config.

(provide 'init-latex)
