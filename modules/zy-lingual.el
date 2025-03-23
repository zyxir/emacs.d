;;; zy-lingual.el --- Linguistic features. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+lingual' module of the configuration.

;; The Rime input method, as well as other configurations about input methods is
;; put here.

;;; Code:

(require 'zylib)

(pkg! 'rime)

;; Use the integrated Rime input method as the default input method.
(setq-default default-input-method "rime")

(after! 'rime
  (setq-default
   ;; I have my scheme data in my emacs directory.
   rime-user-data-dir (expand-file-name "rime" user-emacs-directory)
   ;; This data directory is usually provided by a package manager. The package
   ;; name is often rime-data.
   rime-share-data-dir (some-path!
                        ;; By normal system package manager.
                        "/usr/share/rime-data"
                        ;; By Nix.
                        "~/.nix-profile/share/rime-data")
   ;; Show candidates with posframe.
   rime-show-candidate 'posframe
   rime-posframe-properties '(:internal-border-width 2)))

;; Indicate input method with different cursor color.
(after! '+theme
  (after-graphics!
    (eval-and-compile (require 'color))
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
                 ((string-prefix-p "modus-" (symbol-name +theme-theme))
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
          (defun +lingual-im-change-cursor-color-h ()
            "Set cursor color depending on input method."
            (interactive)
            (set-cursor-color (if (im-p)
                                  cursor-color-im
                                cursor-color-default))))))))

(provide 'zy-lingual)

;;; zy-lingual.el ends here
