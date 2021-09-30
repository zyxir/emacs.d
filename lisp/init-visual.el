;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Visual configurations like themes and fonts.

;;; Code:

;;;; Themes and Icons

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (defvar zy/default-theme 'doom-tomorrow-day
    "Default doom theme to use.")
  (load-theme zy/default-theme t))

;; Solaire mode.

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

;; Enable icons support.
;; Remember to run `all-the-icons-install-fonts'.

(use-package all-the-icons
  :straight t)

;;;; Fonts

;; Font setter.

(defconst zy/default-fonts
  '("Sarasa Mono TC"
    "Source Han Sans HW TC"
    "WenQuanYi Zen Hei Mono")
  "Default fallback font list.")

(defconst zy/default-vpfonts
  '("Verdana"
    "Times New Roman"
    "Georgia"
    "Helvetica"
    "Aria")
  "Default fallback variable pitch font list.

Fonts are selected according to recommendations from Bureau of
Internet Accessibility.")

(defvar zy/custom-font nil
  "Custom font assigned by the user.")

(defvar zy/custom-vpfont nil
  "Custom variable pitch font assigned by the user.")

(defvar zy/current-font nil
  "The current enabled font. Would be nil if it is the system
fallback font.")

(defvar zy/current-vpfont nil
  "The current enabled variable pitch font. Would be nil if it is
the system fallback font.")

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

(defun zy:set-font (font &optional size vpfont)
  "Set FONT as the universal font.

SIZE is the font size. If it is nil, `zy/default-font-size' will
be used.

Optionally set the variable pitch font as VPFONT."
  (let* ((size (or size zy/default-font-size)))
    (set-face-attribute 'default nil :font
			(font-spec :family font
				   :size size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec :family font
				   :size size)))
    (when vpfont
      (set-face-attribute 'variable-pitch nil :font
			  (font-spec :family vpfont
				     :size size)))
    (setq zy/current-font font)))

(defun zy/set-font ()
  "Set font wisely.

If `window-system' is nil, this function only pops up a warning
message, as it is impossible to set font in a terminal
environment.

The default font will be set to `zy/custom-font', if it is not
nil, or the first available font listed in `zy/default-fonts'. If
non of them is available, this function does nothing except
popping up a warning message.

The same thing is applied to variable pitch font, which defaults
to `zy/custom-vpfont', and will choose one from
`zy/default-vpfonts' by default.

If proper font is set, these two lines of Chinese characters and
latin letters would have the same length:

想讓中文和英文對齊可真難呀
abcdefghijklmnopqrstuvwxyz"
  (interactive)
  (if window-system
      (let* ((font (or zy/custom-font
		       (cl-some
			#'zy/font-exists-p
			zy/default-fonts)))
	     (vpfont (or zy/custom-vpfont
			 (cl-some
			  #'zy/font-exists-p
			  zy/default-vpfonts))))
	(if (and font vpfont)
	    (zy:set-font font zy/default-font-size vpfont)
	  (message "Some font is not available.")))
    (message "Cannot set font without window system.")))

(zy/set-font)

;; End of config.

(provide 'init-visual)
