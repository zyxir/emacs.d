;;; init-completion.el --- Minibuffer and text completion.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures completion in the minibuffer and in text,
;; which is powered by the Orderless completion style and the Vertico suite.

;;; Code:

(require-package 'orderless)
(require-package 'vertico)
(require-package 'marginalia)


;; Completion Styles

;; Setup general completion styles.
(setq
 ;; Prefer default completion styles but also use Orderless.
 completion-styles '(basic substring initials flex orderless)
 ;; Category-based completion styles based on Protesilaos's config.
 completion-category-overrides
 '((file (styles . (basic partial-completion orderless)))
   (bookmark (styles . (basic substring)))
   (library (styles . (basic substring)))
   (embark-keybinding (styles . (basic substring)))
   (imenu (styles . (basic substring orderless)))
   (consult-location (styles . (basic substring orderless)))
   (kill-ring (styles . (emacs22 orderless)))
   (eglot (styles . (emacs22 substring orderless))))
 ;; Ignore case for most completion.
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t)

(defun zy/orderless-literal (word _index _total)
  "Read WORD= as a literal string."
  (when (string-suffix-p "=" word)
    `(orderless-literal . ,(substring word 0 -1))))

(defun zy/orderless-file-ext (word _index _total)
  "Expand WORD. to a file suffix when completing file names."
  (when (and minibuffer-completing-file-name
             (string-suffix-p "." word))
    `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

(setq orderless-matching-styles '(orderless-prefixes orderless-regexp)
      orderless-style-dispatchers '(zy/orderless-literal
                                    zy/orderless-file-ext))


;; Minibuffer

;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t)
;; And show minibuffer depth.
(minibuffer-depth-indicate-mode 1)

;; Use shorter format string for default value.
(setq minibuffer-default-prompt-format " [%s]")
;; And do not show default value if a user value is being typed.
(minibuffer-electric-default-mode 1)

;; Do not allow the cursor to move inside the minibuffer prompt.
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Use Vertico for minibuffer completion.
(vertico-mode 1)

;; Indicator for completing-read-multiple.
(defun crm-indicator (args)
  "Indicator for `completing-read-multiple'.

ARGS are the arguments passed."
  (defvar crm-separator)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Show candidate info with Marginalia.
(marginalia-mode 1)

;;; init-completion.el ends here
