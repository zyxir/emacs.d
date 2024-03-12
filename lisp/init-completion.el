;;; init-completion.el --- Minibuffer and text completion.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures completion in the minibuffer and in text,
;; which is powered by the Orderless completion style and the Vertico suite.

;;; Code:

(require-package 'orderless)
(require-package 'vertico)
(require-package 'marginalia)
(require-package 'corfu)
(require-package '(corfu-terminal
                   :url "https://codeberg.org/akib/emacs-corfu-terminal"))
(require-package 'cape)
(require-package 'consult-yasnippet)


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
(defun zy/crm-indicator (args)
  "Indicator for `completing-read-multiple'.

ARGS are the arguments passed."
  (defvar crm-separator)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'zy/crm-indicator)

;; Show candidate info with Marginalia.
(marginalia-mode 1)


;; Text Completion

(add-hook! prog-mode #'corfu-mode)
(setq
 ;; Enable auto completion.
 corfu-auto t
 ;; No delay for auto completion.
 corfu-auto-delay 0)
(general-def
  :keymaps 'corfu-map
  ;; Do not intefere with cursor movement keys.
  "RET" nil
  "<up>" nil
  "<down>" nil
  [remap previous-line] nil
  [remap next-line] nil)

;; Remember completion history.
(corfu-history-mode 1)

;; Enable Corfu in the minibuffer.
(defun zy/enable-corfu-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'zy/enable-corfu-in-minibuffer)

;; Echo info of candidates in the echo area after a short delay.
(corfu-echo-mode 1)

;; Enable terminal support.
(corfu-terminal-mode 1)

;; Bind some Consult commands to "g".
(general-def
  :states 'motion
  "g o" #'consult-outline
  "g j" #'consult-imenu)

;; Use many CAPFs (`completion-at-point-functions's) with C-v.
(zy/C-v-def
 "C-f" #'cape-file
 "C-e" #'cape-emoji
 "C-s" #'consult-yasnippet)

;;; init-completion.el ends here
