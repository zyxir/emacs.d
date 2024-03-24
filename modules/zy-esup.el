;;; zy-esup.el --- Startup profiling. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+esup' module of the configuration.

;; The package Esup is incorporated to profile startup time. Executing `esup'
;; results in a full startup time report.

;;; Code:

(require 'zylib)

(pkg! 'esup)

(provide 'zy-esup)

;;; zy-esup.el ends here
