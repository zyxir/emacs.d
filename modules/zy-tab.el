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

;; Enable Bufferlo right after tab-bar is loaded.
(after! 'tab-bar
  (bufferlo-mode 1))

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
       (or dashboard scratch))))

  ;; Use a custom strategy for generating tab names.
  (setq
   tab-bar-tab-name-function
   (defun +tab-name-current()
     "Generate tab name smartly.
Try the current project name first, then the current buffer name."
     (if (and (project-current) (fboundp 'project-name))
         (project-name (project-current))
       (buffer-name (window-buffer (minibuffer-selected-window)))))))

;; Prefer local buffers in `consult-buffer'. Note that `consult-buffer' groups
;; buffers (as well as local files and recent files) into different sources, and
;; completion works for the first source by default until a narrow key is used.
(after! '(bufferlo consult)
  (defvar +tab-consult-source-local-buffer
    `( :name "Local Buffers"
       :narrow ?l
       :category buffer
       :face consult-buffer
       :history buffer-name-history
       :state ,#'consult--buffer-state
       :items ,#'(lambda () (consult--buffer-query
                             :predicate #'bufferlo-local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
    "Local buffer candidate source for `consult-buffer'.")

  (defvar +tab-consult-source-other-buffer
    `( :name "Other Buffers"
       :narrow ?l
       :category buffer
       :face consult-buffer
       :history buffer-name-history
       :state ,#'consult--buffer-state
       :items ,#'(lambda () (consult--buffer-query
                             :predicate #'bufferlo-non-local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
    "Non-local buffer candidate source for `consult-buffer'.")

  ;; Replace `consult--source-buffer' by the two entries defined, and keep
  ;; others unchanged.
  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-modified-buffer
                                 +tab-consult-source-local-buffer
                                 +tab-consult-source-other-buffer
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-bookmark
                                 consult--source-project-buffer-hidden
                                 consult--source-project-recent-file-hidden)))

(provide 'zy-tab)

;;; zy-tab.el ends here
