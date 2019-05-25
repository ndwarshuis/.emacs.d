;; init the straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; watch for repo modifications if we have python3 and watchexec
;; otherwise just use a save hook
(setq straight-check-for-modifications
      (if (and (executable-find "python3")
               (executable-find "watchexec"))
          '(watch-files find-when-checking)
        '(check-on-save find-when-checking)))

;; install use-package itself
(straight-use-package 'use-package)

;; configure all config paths before anything else is loaded
(use-package no-littering :straight t)

(defvar nd/conf-dir "~/.emacs.d/"
  "The absolute path to the EMACS configuration directory.")

(defvar nd/conf-main (no-littering-expand-etc-file-name "conf.org")
  "The absolute path the main EMACS configuration file.")

;; ensure we use built-in org mode
;; (use-package org :straight org-plus-contrib)
(straight-use-package '(org :type built-in))

;; load everything else
(org-babel-load-file nd/conf-main)
