;;; init-prose.el --- Distraction-free writting.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures features for prose-editing.

;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'olivetti)

;;;; Distraction Free Mode

(zy/leader-t-def
  "d" #'olivetti-mode)

;; Use larger font when Olivetti is on.
(let ((olivetti-inc 2))
  (add-hook! olivetti-mode-on
    (text-scale-adjust olivetti-inc))
  (add-hook! olivetti-mode-off
    (text-scale-adjust (- olivetti-inc))))

(provide 'init-prose)

;;; init-prose.el ends here
