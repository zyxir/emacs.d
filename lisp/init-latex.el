;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about LaTeX files.

;;; Code:

(use-package tex
  :defer t
  :straight auctex
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX"
				   "%`xelatex%(mode)%' --synctex=1%(mode)%' %t"
				   TeX-run-TeX
				   nil
				   t))
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-show-compilation t
	TeX-command-force "XeLaTeX"
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq-default TeX-master nil
		font-latex-fontify-script nil
		TeX-PDF-mode t)

  ;; PDF Preview with PDF Tools.
  (setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
		TeX-source-correlate-mode t
		TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

  ;; Find master file automatically.
  (defun TeX-find-master-file ()
    "Finds the master file for TeX/LaTeX project by searching for
  'main.tex' in the good directories"
    (let (foundFiles (currPath (expand-file-name "./")) foundFile)
      (while (not (or foundFiles (equal currPath "/")))
	(setq foundFiles (directory-files currPath t "main\\.tex"))
	(setq currPath (expand-file-name (concat currPath "../"))))
      (and
       (setq foundFile (car foundFiles))
       (file-exists-p foundFile)
       (setq foundFile (file-name-sans-extension foundFile))
       foundFile)))
  (defun TeX-set-master-file (&optional ignore1 ignore2 ignore3)
    "Finds the master file by means of TeX-find-master-file and
  set TeX-master to it value"
    (setq TeX-master (or (TeX-find-master-file) TeX-master)))
  (add-hook 'TeX-mode-hook 'TeX-set-master-file)

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
