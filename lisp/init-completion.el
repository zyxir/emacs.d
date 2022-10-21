;;; init-completion.el --- Configure minibuffer

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup minibuffer completion and at-point completion.

;;; Code:

(require 'init-common)
(require 'init-keybinding)


;; Setup minibuffer completion with Vertico

(straight-use-package 'vertico)
(straight-use-package 'marginalia)

(zy/defsnip snip-vertico
    (:events 'pre-command :weight 80)
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

(straight-use-package 'orderless)

(setq completion-styles '(substring orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))


;; Setup Consult

(straight-use-package 'consult)

(setq completion-in-region-function 'consult-completion-in-region)
(with-eval-after-load 'consult
  (with-no-warnings
    (consult-customize consult-recent-file
		       :preview-key (kbd "M-."))))


;; Setup Embark

(straight-use-package 'embark)

(zy/define-key
  [remap tmm-menubar] 'embark-act)


;; Setup Corfu and its accesories for in-point completion

(straight-use-package 'corfu)
(straight-use-package 'corfu-doc)
(straight-use-package '(corfu-terminal
			:repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(straight-use-package '(corfu-doc-terminal
			:repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(zy/defsnip snip-corfu
    (:events 'after-command :weight 20)
  (global-corfu-mode +1)
  (setq-default corfu-auto t
		   corfu-auto-prefix 2
		   corfu-echo-documentation nil)
  (zy/define-key :keymap 'corfu-map
    "SPC" 'corfu-insert-separator))

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
  (require 'corfu-indexed "extensions/corfu-indexed")
  (add-hook 'corfu-mode-hook #'corfu-indexed-mode)
  (require 'corfu-info "extensions/corfu-info")
  ;; Enable Corfu-terminal if in terminal or WSL
  (when (or (not (display-graphic-p))
	    (zy/wsl-p))
    (require 'corfu-terminal)
    (corfu-terminal-mode +1))
  ;; Enable Corfu-doc
  (require 'corfu-doc)
  (zy/define-key :keymap 'corfu-map
    "M-d" #'corfu-doc-toggle)
  ;; Enable Corfu-doc-terminal if in terminal
  (when (or (not (display-graphic-p))
	    (zy/wsl-p))
    (require 'corfu-doc-terminal)
    (corfu-doc-terminal-mode +1)))


;; Setup Cape for more completion-at-point functions

(straight-use-package 'cape)

(mapc (lambda (hook)
	(add-hook hook
		  (lambda ()
		    (add-to-list 'completion-at-point-functions
				 #'cape-file))))
      '(prog-mode-hook text-mode-hook))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (add-to-list 'completion-at-point-functions
			 #'cape-keyword)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-to-list 'completion-at-point-functions
			 #'cape-symbol)))

(add-hook 'tex-mode-hook
	  (lambda ()
	    (add-to-list 'completion-at-point-functions
			 #'cape-tex)))


(provide 'init-completion)

;;; init-completion.el ends here.
