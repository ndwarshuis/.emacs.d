(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/conf.org"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(yasnippet-snippets flycheck rainbow-delimiters-mode helm evil-collection haskell-mode fill-column-indicator gtklp delight browse-kill-ring evil-org-agenda evil-org evil calfw calfw-org yaml-mode which-key use-package typit systemd sudo-edit spaceline rainbow-mode rainbow-delimiters pkgbuild-mode pdf-tools org-bullets lua-mode ess elpy diff-hl beacon ace-window))))
(put 'dired-find-alternate-file 'disabled nil)
