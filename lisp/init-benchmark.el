;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Startup time benchmark.

;;; Code:

(setq zy/init-benchmark-start-time (current-time))

;; Require common lisp utilities.

(require 'cl-lib)


(defun zy/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar zy/load-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun zy/load-times-wrapper (orig feature &rest args)
  "Note in `zy/load-times' the time taken to load each feature."
  (let* ((already-loaded (memq feature features))
	 (load-start-time (and (not already-loaded) (current-time))))
    (prog1
	(apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
	(let ((time (zy/time-subtract-millis (current-time) load-start-time)))
	  (add-to-list 'zy/load-times
		       (list feature load-start-time time)))))))

(advice-add 'zy/load :around 'zy/load-times-wrapper)


(define-derived-mode zy/load-times-mode tabulated-list-mode "Load-Times"
  "Display times taken to `zy/load' packages."
  (setq tabulated-list-format
	[("Start time (ms)" 20 zy/load-times-sort-by-start-time-pred)
	 ("Feature" 30 t)
	 ("Time (ms)" 12 zy/load-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-entries #'zy/load-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun zy/load-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun zy/load-times-sort-by-load-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun zy/load-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in zy/load-times
	   with order = 0
	   do (cl-incf order)
	   collect (list order
			 (vector
			  (format "%.3f" (zy/time-subtract-millis start-time before-init-time))
			  (symbol-name feature)
			  (format "%.3f" millis)))))

(defun zy/load-times ()
  "Show a tabular view of how long each feature took to load."
  (interactive)
  (with-current-buffer
   (get-buffer-create "*Load Times*")
   (zy/load-times-mode)
   (tabulated-list-revert)
   (display-buffer (current-buffer))))


(defun display-startup-echo-area-message ()
  (message "init completed in %.3f milliseconds."
	   (zy/time-subtract-millis after-init-time before-init-time)))

(add-to-list 'zy/load-times
	     (list
	      'init-benchmark
	      zy/init-benchmark-start-time
	      (zy/time-subtract-millis
	       (current-time)
	       zy/init-benchmark-start-time)))

;; End of config.

(provide 'init-benchmark)
