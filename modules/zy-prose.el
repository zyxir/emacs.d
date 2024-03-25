;;; zy-prose.el --- Prose editing. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+prose' module of the configuration.

;; This file introduces several features for prose-editing, including a
;; distraction-free mode (`olivetti-mode').

;;; Code:

(require 'zylib)

(pkg! 'olivetti)

(after! '+leader
  (keybind! nil +leader-y-map
    "v" (cons "Distraction Free" #'olivetti-mode)))

(after! 'olivetti
  ;; Use larger font when Olivetti is on.
  (let ((olivetti-inc 2))
    (add-hook! 'olivetti-mode-on-hook
      (text-scale-adjust olivetti-inc))
    (add-hook! 'olivetti-mode-off-hook
      (text-scale-adjust (- olivetti-inc)))))

(provide 'zy-prose)

;;; zy-prose.el ends here
