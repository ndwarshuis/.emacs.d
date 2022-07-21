;; disable automatic package updates
(setq straight-check-for-modifications nil)

;; watch for repo modifications if we have python3 and watchexec
;; otherwise just use a save hook
;; (setq straight-check-for-modifications
;;       (if (and (executable-find "python3")
;;                (executable-find "watchexec"))
;;           '(watch-files find-when-checking)
;;         '(check-on-save find-when-checking)))

;; add pinned packages to straight
;; (setq straight-profiles
;;      '((nil . "default.el")
;;        ;; Packages which are pinned to a specific commit.
;;        (pinned . "pinned.el")))

;; bootstrap straight
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

;; install use-package itself
(straight-use-package 'use-package)
