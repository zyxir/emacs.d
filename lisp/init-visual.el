;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Visual configurations like themes and fonts.

;;; Code:

;; Install and enable spacemacs theme.

(use-package spacemacs-common
  :straight spacemacs-theme
  :config
  (load-theme 'spacemacs-light t))

;; Solaire mode.

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

;; Enable icons support.
;; Remember to run `all-the-icons-install-fonts'.

(use-package all-the-icons
  :straight t)

;; Font setter.

(defconst zy/default-fonts
  '("Sarasa Mono TC"
    "Source Han Sans HW TC"
    "WenQuanYi Zen Hei Mono")
  "Default fallback font list.")

(defvar zy/custom-font nil
  "Custom font assigned by the user.")

(defvar zy/current-font nil
  "The current enabled font. Would be nil if it is the system
fallback font.")

(defvar zy/default-font-size 14
  "Default font pixel size.

Chinese characters and latin letters will align perfectly only
when the font pixel size is an even number, even if a hybrid
monospace font (like Source Han Sans HW) is used.")

(defun zy/font-exists-p (font)
  "Check if FONT exists in the system.

If it does exist, return itself.  If it doesn't, return nil."
  (if (null (x-list-fonts font))
      nil
    font))

(defun zy:set-font (font &optional size)
  "Set FONT as the universal font.

SIZE is the font size. If it is nil, `zy/default-font-size' will
be used."
  (let* ((size (or size zy/default-font-size)))
    (set-face-attribute 'default nil :font
			(font-spec :family font
				   :size size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec :family font
				   :size size)))
    (setq zy/current-font font)))

(defun zy/set-font ()
  "Set font wisely.

If `window-system' is nil, this function only pops up a warning
message, as it is impossible to set font in a terminal
environment.

The font will be `zy/custom-font', if it is not nil, or the first
available font listed in `zy/default-fonts'. If non of them is
available, this function does nothing except popping up a warning
message.

If proper font is set, these two lines of Chinese characters and
latin letters would have the same length:

想讓中文和英文對齊可真難呀
abcdefghijklmnopqrstuvwxyz"
  (interactive)
  (if window-system
      (let* ((font (or zy/custom-font
		       (cl-some
			#'zy/font-exists-p
			zy/default-fonts))))
	(if font
	    (zy:set-font font zy/default-font-size)
	  (message "No custom font or preset font available.")))
    (message "Cannot set font without window system.")))

(add-hook 'after-init-hook #'zy/set-font)

;; End of config.

(provide 'init-visual)
