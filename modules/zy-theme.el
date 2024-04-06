;;; zy-theme.el --- Theme setup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+theme' module of the configuration.

;; The theme and related packages are declared and set here. Customize the
;; desired theme with the `zy-theme' variable. This variable currently supports
;; the following themes:
;;
;; - All Modus themes: modus-operandi/vivendi[-tinted/tritanopia/deuteranopia]
;;
;; It is worth noting that theme-specific customizations are automatically
;; loaded if the corresponding theme is chosen.

;;; Code:

(require 'zylib)

;; Although Modus Themes are included in Emacs 29, the ELPA version has more
;; features and variants.
(pkg! 'modus-themes)
(pkg! 'rainbow-delimiters)
(pkg! 'hl-todo)

(defcustom +theme-theme 'modus-operandi-tinted
  "The theme to use for Emacs.

Set theme with this variable instead of `custom-enabled-themes'
to make the loading of themes more deterministic."
  :type '(choice (const modus-operandi)
                 (const modus-vivendi)
                 (const modus-operandi-tinted)
                 (const modus-vivendi-tinted)
                 (const modus-operandi-tritanopia)
                 (const modus-vivendi-tritanopia)
                 (const modus-operandi-deuteranopia)
                 (const modus-vivendi-deuteranopia))
  :group 'zyemacs)

;; Load (require) the user-specified theme.
(cond
 ((string-prefix-p "modus-" (symbol-name +theme-theme))
  (require 'modus-themes))
 (t nil))

;; Configure Modus Themes if necessary.
(after! 'modus-themes
  ;; Use more variants to enhance distinguishability.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)

  ;; Do not extend region highlight to the edge of the window.
  (set-face-attribute 'region nil :extend nil)

  ;; Modus Themes uses a unique strategy to make its `fill-column' indicator
  ;; pretty in GUI. However the indicator is very ugly in terminal. Let's use a
  ;; more "traditional" face for the indicator to achieve better consistency in
  ;; both GUI and terminal.
  (set-face-attribute 'fill-column-indicator nil
                      ;; Do not use the 1-pixel height specified by Modus
                      ;; Themes.
                      :height 'unspecified
                      ;; Attributes copied from the default theme.
                      :background 'unspecified
                      :weight 'normal
                      :slant 'normal
                      :overline nil
                      :underline nil
                      :strike-through nil
                      :box nil
                      :inverse-video nil
                      :stipple nil
                      :inherit 'shadow)

  ;; Use more prominent faces for errors/warnings/notes on terminal since
  ;; underlines cannot be colored there.
  (add-hook! '(window-setup-hook after-make-frame-functions)
    (defun +theme-setup-terminal-err-faces-h (&optional frame)
      "Setup more prominent error faces for FRAME.
Only works for non-graphical frames."
      (when-let* ((frame (or frame (selected-frame))))
        (unless (display-graphic-p frame)
          (set-face-attribute 'modus-themes-lang-error frame
                              :inherit 'error)
          (set-face-attribute 'modus-themes-lang-warning frame
                              :inherit 'warning)
          (set-face-attribute 'modus-themes-lang-note frame
                              :inherit 'note))))))

;; Load the theme.
(let ((enable-theme-functions nil))
 (load-theme +theme-theme 'no-confirm))

;; Enable rainbow delimeters for all prog modes.
(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable todo keyword highlight globally.
(add-hook! 'window-setup-hook (global-hl-todo-mode 1))

(provide 'zy-theme)

;;; zy-theme.el ends here
