;;; zy-bib.el --- Bibliography management. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+bib' module of the configuration.

;; Ebib is a package for managing BibTeX (or biblatex) based bibliography
;; libraries. It helps you collect bibliography entries from the Internet,
;; managing and filtering them, and attaching files as well as notes to them.
;; The Citar package, on the other hand, helps you cite bibliography entries in
;; Org and LaTeX files. These packages together create a feature-rich
;; bibliography management suite inside Emacs.

;;; Code:

(require 'zylib)

(pkg! 'ebib)
(pkg! 'biblio)
(pkg! 'citar)
(pkg! 'embark)
(pkg! 'citar-embark)

(after! '+leader
  (keybind! nil +leader-e-map
    "b" (cons "Ebib" #'ebib)))

(after! 'ebib
  ;; Generate BibTeX keys automatically. However the automatic generation may
  ;; not be reliable sometimes. Manually edit a key with TODO when necessary.
  (setq ebib-autogenerate-keys t)

  ;; Open file with my handy opener.
  (after! '+platform
    (setq ebib-file-associations
          (list (cons "caj" #'+platform/open-externally)
                (cons "pdf" #'+platform/open-externally)
                (cons "ps" #'+platform/open-externally))))

  (keybind! nil ebib-index-mode-map
    "+" #'ebib-import-file
    "B" #'ebib-biblio-import-doi)

  ;; Import file from one of these directories.
  (setq ebib-import-source-directory
        (some-path! (format "/mnt/c/Users/%s/Downloads/" user-login-name)
                    "~/Downloads/")))

(after! 'biblio
  ;; Generate BibTeX keys automatically for Biblio.
  (setq biblio-bibtex-use-autokey t))

;; Use a `+quickins' key to insert citations.
(after! '(+quickins)
  (keybind! nil +quickins-map
    "C-c" (cons "Cite" #'citar-insert-citation)))

;; Open file with my handy opener.
(after! '(+platform citar)
  (setq citar-file-open-functions '((t . +platform/open-externally))))

;; Load Citar early for relevant modes.
(after! '(tex-mode org markdown-mode)
  (require 'citar))

(after! 'citar
  ;; Embark integration for Citar.
  (citar-embark-mode 1))

;; Configure the built-in library `bibtex', which is used to edit BibTeX files
;; and generate BibTeX keys automatically.
(after! 'bibtex
  ;; It uses 2 digits for year by default. I prefer 4.
  (setq bibtex-autokey-year-length 4)

  ;; Using 5 words for title is too long. I prefer shorter keys.
  (setq bibtex-autokey-titlewords 2)

  ;; Do not use more than 2 words. Short keys are prettier!
  (setq bibtex-autokey-titlewords-stretch 0)

  ;; Separate everything with underscores.
  (setq bibtex-autokey-name-year-separator "_"
        bibtex-autokey-year-title-separator "_")

  ;; HACK: Add a "entrysubtype" field for "Book" entries, so that the "Standard"
  ;; type of GB/T 7714-2015 can be supported by biblatex. Biblatex emulates the
  ;; "Standard" type by using the "Book" type with its "entrysubtype" field as
  ;; "standard".
  (setq bibtex-biblatex-entry-alist
        (mapcar (lambda (entry)
             (if (string= (car entry) "Book")
                 (let ((modified-entry entry)
                       (optionals (nth 4 entry)))
                   (unless (member '("entrysubtype") optionals)
                     (push '("entrysubtype") optionals)
                     (setf (nth 4 modified-entry) optionals))
                   modified-entry)
               entry))
           bibtex-biblatex-entry-alist)))

(provide 'zy-bib)

;;; zy-bib.el ends here
