;;; zy-tex.el --- TeX and LaTeX. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+tex' module of the configuration.

;; I only know the approximate differences between TeX and LaTeX, but I use
;; LaTeX almost 100% of my time. Therefore, most settings in this file only
;; apply to LaTeX.

;;; Code:

(require 'zylib)

(pkg! 'auctex)

(after! 'tex
  (setq-default
  ;; Do not fontify superscripts and subscripts.
   font-latex-fontify-script nil)
  (setq
   ;; Automatically save style information when saving the buffer.
   TeX-auto-save t
   ;; Parse file after loading it if no style hook is found for it.
   TeX-parse-self t
   ;; Do not ask the user to save the file.  Do it automatically.
   TeX-save-query nil)

  ;; (declare-function TeX-source-correlate-mode 'tex)
  (add-hook! TeX-mode
    ;; Enable inverse search.
    #'TeX-source-correlate-mode)

  ;; Preview PDF with PDF Tools.
  (setq TeX-view-program-selection
        (cons '(output-pdf "PDF Tools")
              (cl-remove-if (lambda (x) (eq (car x) 'output-pdf))
                            TeX-view-program-selection)))

  ;; Revert document buffer after compilation.
  ;; (declare-function TeX-revert-document-buffer "tex")
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; Load and configure RefTeX for LaTeX.
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(after! 'reftex
  (setq
   ;; Integrate with AUCTeX.
   reftex-plug-into-AUCTeX t
   ;; Reparse only 1 file when asked to.
   reftex-enable-partial-scans t
   ;; Save parsed information.
   reftex-save-parse-info t
   ;; Use a separate selection buffer for each label type.
   reftex-use-multiple-selection-buffers t))

(after! 'latex
  (zy/local-leader-def
    :keymaps 'LaTeX-mode-map
    "a" #'TeX-command-run-all
    "c" #'TeX-command-master
    "e" #'LaTeX-environment
    "f" #'TeX-font
    "v" #'TeX-view
    "_" #'TeX-master-file-ask))

(provide 'zy-tex)

;;; zy-tex.el ends here
