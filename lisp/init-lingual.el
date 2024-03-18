;;; init-lingual.el --- Language-related features.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'rime)
(require-package 'sis)

;; Use integrated Rime as the input method.
(setq-default default-input-method "rime")
(after-deferred! 'rime
  (setq-default
   ;; I have my scheme data in my emacs directory.
   rime-user-data-dir (expand-file-name "rime" user-emacs-directory)
   ;; This data must be provided by the system package manager. The package name
   ;; is often rime-data.
   rime-share-data-dir "/usr/share/rime-data"
   ;; Show candidates with posframe.
   rime-show-candidate 'posframe
   rime-posframe-properties '(:internal-border-width 2)))

;; Indicate input method with different cursor color.
(after-gui!
  (eval-and-compile (require 'color))
  (defvar zy/theme)
  (cl-flet ((im-p ()
              "Return non-nil if input method is active."
              (if (featurep 'rime)
                  (and (rime--should-enable-p)
                       (not (rime--should-inline-ascii-p))
                       current-input-method)
                current-input-method))
            (alternate-color (color)
              "Get alternate color from COLOR."
              (cond
               ;; Use these presets for known themes.
               ((string-prefix-p "modus-" (symbol-name zy/theme))
                (eval-and-compile (require 'modus-themes))
                (modus-themes-get-color-value 'green-intense))
               ;; Otherwise, invert the color. Might be ugly!
               (t (let* ((rgb (color-name-to-rgb color))
                         (inverted-rgb (mapcar (lambda (x) (- 1.0 x)) rgb)))
                    (color-rgb-to-hex (nth 0 inverted-rgb)
                                      (nth 1 inverted-rgb)
                                      (nth 2 inverted-rgb)
                                      2))))))
    (let* ((cursor-color-default (frame-parameter frame 'cursor-color))
           (cursor-color-im (alternate-color cursor-color-default)))
      (add-hook! 'post-command-hook
        (defun zy/-im-change-cursor-color ()
          "Set cursor color depending on input method."
          (interactive)
          (set-cursor-color (if (im-p)
                                cursor-color-im
                              cursor-color-default)))))))

;; Automatically toggle input method with Sis.
(after-deferred! 'sis
  ;; Use native input method on Emacs.
  (sis-ism-lazyman-config nil "rime" 'native)
  (sis-global-respect-mode 1)
  (sis-global-context-mode 1))

(provide 'init-lingual)

;;; init-lingual.el ends here
