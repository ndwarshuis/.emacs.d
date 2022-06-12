;;;; init the straight package manager

(load-file (expand-file-name "straight-boot.el" user-emacs-directory))

;; configure all config paths before anything else is loaded
(use-package no-littering :straight t)

(defvar nd/conf-main (no-littering-expand-etc-file-name "conf.org")
  "The absolute path the main EMACS configuration file.")

;; ensure we use built-in org mode
;; (use-package org :straight org-plus-contrib)
(straight-use-package '(org :type built-in))

;; load everything else
(org-babel-load-file nd/conf-main)
