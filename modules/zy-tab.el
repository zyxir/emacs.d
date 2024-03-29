;;; zy-tab.el --- Tab-based workspaces. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+tab' module of the configuration.

;; Tabs of Emacs is different from tabs in other editors. A tab does not reflect
;; a buffer like it does in VS Code or other "normal" editors. It reflects
;; something more like a workspace instead. To make tabs more like isolated
;; workspaces, the package Bufferlo is incorporated to make commands like
;; `consult-buffer' only show buffers from the current workspace (tab).

;;; Code:

(require 'zylib)

(pkg! 'consult)
(pkg! 'bufferlo)

;; Load Bufferlo right after Tab-bar.
(after! 'tab-bar (require 'bufferlo))

;; And load them both for daemon sessions.
(daemon-require! 'tab-bar 'bufferlo)

(after! '(tab-bar bufferlo)
  ;; For a newly-created tab, display the dashboard if there is one, otherwise
  ;; create a new scratch buffer dedicated to that tab.
  (setq
   tab-bar-new-tab-choice
   (defun +tab-new-tab-fn ()
     "Return an appropriate buffer for a new tab.

This function creates a new dedicated scratch buffer for the new
tab via Bufferlo. After that, return the dashboard buffer if
there it exists, otherwise return the created scratch buffer."
     (let* ((scratch (bufferlo-create-local-scratch-buffer))
            (dashboard (get-buffer "*dashboard*")))
       (or dashboard scratch)))))

;; Only show local buffers in `consult-buffer'.
(after! '(bufferlo consult)
  (set 'consult--source-buffer
       `( :name "Local Buffers"
          :narrow ?l
          :category buffer
          :face consult-buffer
          :history buffer-name-history
          :state ,#'consult--buffer-state
          :default t
          :items #'(lambda () (consult--buffer-query
                               :predicate #'bufferlo-local-buffer-p
                               :sort 'visibility
                               :as #'buffer-name))))
  t)

(provide 'zy-tab)

;;; zy-tab.el ends here
