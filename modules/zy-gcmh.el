;;; zy-gcmh.el --- Garbage collection. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+gcmh' module of the configuration.

;; The garbage collection strategy is currently provided by the GCMH package
;; now.

;;; Code:

(require 'zylib)

(pkg! 'gcmh)

(require 'gcmh)

;; Configure the idle time before triggering GC. The default is 15 seconds,
;; which is too much in my opinion. Being idle for 15 seconds means you are not
;; using Emacs. I think 5 seconds is more subtle since you have to think for
;; seconds between typing from time to time.
(setq gcmh-idle-delay 5)

(gcmh-mode 1)

(provide 'zy-gcmh)

;;; zy-gcmh.el ends here
