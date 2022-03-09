;;; zy-benchmark.el --- Benchmarking utilities of ZyEmacs.

;; This file should be loaded by zy-block.el. Do not load this solely.


;; Definitions.

(defvar zbch-result nil
  "Result of benchmarking, with each element being a (TIME-SINCE
NAME TIME-TAKEN) pair, where TIME-SINCE is the time before
loading the zy-block, NAME is the zy-block name, and TIME-TAKEN
is the time used to execute the zy-block.")


;; Benchmark as a zy-block flag keyword.

(defmacro zbch--time-since (time)
  "Return the time elapsed since TIME.

The result is in milliseconds, and is a string."
  `(format
    "%.2f"
    (* 1000
       (float-time (time-since ,time)))))

(defun zbch-zb-wrapper (name arg body)
  "Wrap BODY with benchmarking code if ARG is non-nil.

The code append (NAME . TIME) to `zb-benchmark-result', where
TIME is the time used to execute the body."
  (if arg
      `((let ((--time-start-- (current-time))
	      --result--)
	  (add-to-list '--result--
		       (zbch--time-since
			before-init-time))
	  ,@body
	  (add-to-list '--result-- ',name)
	  (add-to-list '--result--
		       (zbch--time-since
			--time-start--))
	  (add-to-list 'zbch-result
		       (nreverse --result--)
		       'append)))
    body))

(zb-define-keyword ':benchmark 'flag #'zbch-zb-wrapper
		   :after ':provide)


;; Display benchmark result in a dedicated mode.

(define-derived-mode zbch-time-list-mode
  tabulated-list-mode "Benchmark"
  "Show times taken to execute each zy-block."
  (setq tabulated-list-format
	[("Start time (ms)" 20 zbch-sort-by-time-since)
	 ("Feature" 30 t)
	 ("Time (ms)" 15 zbch-sort-by-time-taken)])
  (setq tabulated-list-entries #'zbch-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun zbch-sort-by-time-since (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun zbch-sort-by-time-taken (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun zbch-list-entries ()
  (cl-loop for (time-since feature time-taken) in zbch-result
	   with order = 0
	   do (cl-incf order)
	   collect (list order
			 (vector time-since
				 (symbol-name feature)
				 time-taken))))

(defun zbch/show-result ()
  "Show a tabular view of how long zy-blocks took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Benchmark Result*")
    (zbch-time-list-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))


(provide 'zy-benchmark)

;;; end of zy-benchmark.el
