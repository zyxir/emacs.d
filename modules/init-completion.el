;;; init-completion.el --- Minibuffer and text completion.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures completion in the minibuffer and in text,
;; which is powered by the Orderless completion style and the Vertico suite.

;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'orderless)
(pkg! 'vertico)
(pkg! 'marginalia)
(pkg! 'corfu)
(pkg! 'popon)
(pkg! corfu-terminal "https://codeberg.org/akib/emacs-corfu-terminal")

;;;; Completion Styles

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

(after-or-now! 'orderless
  (defun zy/orderless-literal (word _index _total)
    "Read WORD= as a literal string."
    (when (string-suffix-p "=" word)
      `(orderless-literal . ,(substring word 0 -1))))

  (defun zy/orderless-prefix (word _index _total)
    "Read WORD^ as a prefix."
    (when (string-suffix-p "^" word)
      `(orderless-regexp . ,(format "^%s" (substring word 0 -1)))))

  (defun zy/orderless-initialism (word _index _total)
    "Read WORD% as an initialism."
    (when (string-suffix-p "%" word)
      `(orderless-initialism . ,(substring word 0 -1))))

  (defun zy/orderless-file-ext (word _index _total)
    "Expand WORD. to a file suffix when completing file names."
    (when (and minibuffer-completing-file-name
               (string-suffix-p "." word))
      `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)
        orderless-style-dispatchers '(zy/orderless-literal
                                      zy/orderless-prefix
                                      zy/orderless-initialism
                                      zy/orderless-file-ext)))

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
(after-deferred! 'vertico
  ;; Enable Vertico.
  (vertico-mode 1)

  ;; Use "C-j" for force exit since "M-RET" is occupied in Windows Terminal.
  (general-def
    :keymaps 'vertico-map
    "C-j" #'vertico-exit-input)

  ;; Indicator for completing-read-multiple.
  (eval-and-compile
    (defun zy/crm-indicator (args)
      "Indicator for `completing-read-multiple'.

ARGS are the arguments passed."
      (defvar crm-separator)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args))))

  ;; Show candidate info with Marginalia.
  (marginalia-mode 1)

  (advice-add #'completing-read-multiple :filter-args #'zy/crm-indicator))

;; Text Completion

(add-hook! prog-mode #'corfu-mode)
(after-or-now! 'corfu
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

  ;; Echo info of candidates in the echo area after a short delay.
  (corfu-echo-mode 1)

  ;; Enable terminal support.
  (corfu-terminal-mode 1))

;; Enable Corfu in the minibuffer.
(defun zy/enable-corfu-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'zy/enable-corfu-in-minibuffer)

;; Close the completion UI whenever C-g or ESC is pressed.
(defun zy/quit-corfu-a (&rest _)
  "Advice function used to call `corfu-quit'."
  :before #'evil-force-normal-state
  (when (fboundp 'corfu-quit) (corfu-quit)))
(advice-add 'evil-force-normal-state :before #'zy/quit-corfu-a)
(advice-add 'keyboard-quit :before #'zy/quit-corfu-a)
(general-unbind :keymaps 'corfu-map "C-g")

(provide 'init-completion)

;;; init-completion.el ends here
