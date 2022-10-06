;;; init-completion.el --- Configure minibuffer
;;; Commentary:
;;; Code:

(require 'init-keybinding)


;; Setup minibuffer completion with Vertico

(zy/delay-till-user-input
 (vertico-mode +1)
 (savehist-mode +1)
 (marginalia-mode +1))

;; Intangible minibuffer

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt)
      enable-recursive-minibuffers t)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Indicator for completing-read-multiple

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


;; Setup Orderless

(setq completion-styles '(substring orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))


;; Setup Consult

(setq completion-in-region-function
      #'consult-completion-in-region)
(general-def 'search-map
  "r" #'consult-ripgrep)


;; Setup Embark

(general-def
  [remap tmm-menubar] #'embark-act
  "C-`" #'embark-dwim)


;; Setup Corfu and its accesories for in-point completion

(zy/delay-till-user-input
 (global-corfu-mode +1)
 (setq corfu-auto t
       corfu-auto-prefix 2
       corfu-echo-documentation nil)
 (general-def 'corfu-map
   "M-SPC" #'corfu-insert-separator
   "M-\\" #'corfu-insert-separator))

;; Enable Corfu in the minibuffer

(defun zy/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active.

From URL`https://kristofferbalintona.me/posts/202202270056/'."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode +1)))
(add-hook 'minibuffer-setup-hook #'zy/corfu-enable-always-in-minibuffer 1)

;; Corfu accesories

(with-eval-after-load 'corfu
  ;; Enable Corfu extensions
  (require 'corfu-indexed)
  (add-hook 'corfu-mode-hook #'corfu-indexed-mode)
  (require 'corfu-info)
  ;; Enable Corfu-terminal if in terminal
  (unless (display-graphic-p)
    (require 'corfu-terminal)
    (corfu-terminal-mode +1))
  ;; Enable Corfu-doc
  (require 'corfu-doc)
  (general-def corfu-map
    "M-p" #'corfu-doc-scroll-up
    "M-n" #'corfu-doc-scroll-down
    "M-d" #'corfu-doc-toggle)
  ;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  ;; Enable Corfu-doc-terminal if in terminal
  (unless (display-graphic-p)
    (require 'corfu-doc-terminal)
    (corfu-doc-terminal-mode +1)))


(provide 'init-completion)

;;; init-completion.el ends here.
