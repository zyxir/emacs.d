;;; init-bench.el --- Benchmarking startup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun display-startup-echo-area-message ()
  (message
   (format "ZyEmacs ready in %.2f seconds."
           (float-time
            (time-subtract after-init-time before-init-time)))))

(provide 'init-bench)

;;; init-bench.el ends here.
