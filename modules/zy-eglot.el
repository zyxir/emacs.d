;;; zy-eglot.el --- LSP support with Eglot. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+eglot' module of the configuration.

;; Eglot is the built-in LSP (language server protocol) client of Emacs. It
;; works out of the box without much configuration. However, it requires
;; manually launching it via "M-x eglot" by default. In this file several smart
;; auto-launch mechanics are introduced, to make the LSP experience smoother.

;;; Code:

(require 'zylib)

(daemon-require! 'eglot)
(pkg! 'eglot-booster :url "https://github.com/jdtsmith/eglot-booster")

(after! 'eglot
   ;; Do not require confirmation on code actions.
  (setq eglot-confirm-server-edits nil)

  ;; Automatically shutdown the server.
  (setq eglot-autoshutdown t)

  ;; HACK: Fix Scala language server association.
  (when-let* ((old-entry (assoc 'scala-mode eglot-server-programs))
              (new-entry `((scala-mode scala-ts-mode) . ,(cdr old-entry))))
    ;; Remove the old entry and add the new entry.
    (setq eglot-server-programs
          (remove old-entry eglot-server-programs)
          eglot-server-programs
          (cons new-entry eglot-server-programs)))

  ;; HACK: Fix AucTeX language IDs.
  (put 'TeX-mode 'eglot-language-id "tex")
  (put 'LaTeX-mode 'eglot-language-id "latex")
  (put 'ConTeXt-mode 'eglot-language-id "context")

  ;; Some handy keys.
  (keybind! nil eglot-mode-map
    "<f1>" #'eglot-code-action-quickfix
    "<f2>" #'eglot-rename))

;; Try to enable Eglot for all prog-modes if possible, unless explicitly
;; blacklisted.

(defcustom +eglot-blacklist '()
  "Don't activate Eglot for these languages.
Each entry is a LANG-ID string returned by
`eglot--guess-contact'."
  :type '(repeat string)
  :group 'zyemacs)

(add-hook! 'prog-mode-hook
  (defun +eglot-try-to-ensure-h (&rest _)
    "Try to ensure Eglot if possible."
    (require 'eglot)
    (when-let* ((guessed (ignore-errors (eglot--guess-contact)))
                (managed-mode (nth 0 guessed))
                (lang-id (nth 4 guessed))
                (not-blacklisted (not (member lang-id +eglot-blacklist)))
                ;; According to the docstring of `eglot-server-programs', there
                ;; are several possible forms of `contact'. Here we only deal
                ;; with the most basic type: a list starting with a string
                ;; PROGRAM. Personally this covers 100% of my use case, but this
                ;; may not work (or even break) in some cases.
                (contact (nth 3 guessed))
                (program (and (stringp (car contact))
                              (car contact)))
                (exists (executable-find program)))
      (eglot-ensure))))

(after! 'envrc
  (advice-add
   #'envrc--apply :after
   (defun +eglot-try-to-ensure-a (buf result)
     "Try to ensure Eglot when Direnv updates.
This is an advice for `envrc--apply' and only when RESULT is a
list, which indicates a successful update as indicated by the
source code. BUF is used as the current buffer."
     (when (and result
                (listp result)
                (not (memq result '(none error))))
       (with-current-buffer buf
         (+eglot-try-to-ensure-h))))))

;; Enhance Eglot performance with eglot-booster. This will not work until the
;; emacs-lsp-booster program is avaiable. See README for more info.
(add-hook! 'window-setup-hook
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode)))

(provide 'zy-eglot)

;;; zy-eglot.el ends here
