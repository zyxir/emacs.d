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
   ;; Show candidates in the minibuffer. For Chinese users:
   ;; 作爲倉頡輸入法用戶，我輸入漢字一般是逐字輸入，所以我沒有沒有「同步詞庫」的
   ;; 需求；而倉頡中每個漢字的編碼幾乎是唯一的，我幾乎可以不看候選盲打，所以在
   ;; minibuffer 中展示候選也不會影響我的打字。
   rime-show-candidate 'minibuffer))

;; Automatically toggle input method with Sis.
(after-deferred! 'sis
  ;; Use native input method on Emacs.
  (sis-ism-lazyman-config nil "rime" 'native)
  (sis-global-respect-mode 1)
  (sis-global-context-mode 1))

(provide 'init-lingual)

;;; init-lingual.el ends here
