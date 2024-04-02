;;; zy-bench.el --- Benchmark startup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+bench' module of the configuration.

;; This module is for benchmarking the loading time of other modules. Load this
;; module before any other modules if a benchmark is required. After startup,
;; see benchmark result via the `+bench/show' command.
;;
;; In order to benchmark everything including the loading of Zylib components,
;; this module is the only one that does not require anything.

;;; Code:

(defvar +bench-time-alist '()
  "Alist of files and their load time.
Each element is a cons cell like (FILE . MSEC), where FILE is
exactly the FILE argument taken by `zy-load-rel', and MSEC is the
number of milliseconds taken to load FILE.")

;; Advise `zy-load-rel' to benchmark the loading of files.
(when (fboundp 'zy-load-rel)
  (advice-add
   #'zy-load-rel :around
   (defun +bench-time-a (oldfun file)
     "Benchmark time used to call OLDFUN on ARGS."
     (let* ((time-before (current-time))
            (result (funcall oldfun file))
            (time-elapsed (time-since time-before))
            (msec (* 1000 (float-time time-elapsed))))
       (add-to-list '+bench-time-alist (cons file msec))
       result))))

;; Sort `+bench-time-alist' after startup.
(add-hook
 'window-setup-hook
 (defun +bench-sort-h ()
   (setq
    +bench-time-alist
    (sort +bench-time-alist
          (lambda (elt1 elt2)
            (>= (cdr elt1) (cdr elt2)))))))

;; View benchmark result via `+bench/show'. TODO: Finish this command.
(defun +bench/show ()
  "Display benchmark result."
  (interactive)
  ;; This command is not finished. Display help instead.
  (describe-variable '+bench-time-alist))

(provide 'zy-bench)

;;; zy-bench.el ends here
