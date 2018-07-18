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
 '(avy-background t)
 '(aw-background t)
 '(cfw:fchar-horizontal-line 9473)
 '(cfw:fchar-junction 9547)
 '(cfw:fchar-left-junction 9507)
 '(cfw:fchar-right-junction 9515)
 '(cfw:fchar-top-junction 9519)
 '(cfw:fchar-top-left-corner 9487)
 '(cfw:fchar-top-right-corner 9491)
 '(cfw:fchar-vertical-line 9475)
 '(evil-collection-setup-minibuffer t)
 '(fci-rule-use-dashes t)
 '(haskell-compile-command "ghc -dynamic -Wall -ferror-spans -fforce-recomp -c %s" t)
 '(haskell-interactive-popup-errors nil)
 '(helm-M-x-fuzzy-match t)
 '(helm-autoresize-max-height 40)
 '(helm-buffers-fuzzy-matching t)
 '(helm-imenu-fuzzy-match t t)
 '(helm-recentf-fuzzy-match t t)
 '(helm-scroll-amount 8)
 '(helm-semantic-fuzzy-match t t)
 '(package-selected-packages
   (quote
	(yasnippet-snippets flycheck rainbow-delimiters-mode helm evil-collection haskell-mode fill-column-indicator gtklp delight browse-kill-ring evil-org-agenda evil-org evil calfw calfw-org yaml-mode which-key use-package typit systemd sudo-edit spaceline rainbow-mode rainbow-delimiters pkgbuild-mode pdf-tools org-bullets lua-mode ess elpy diff-hl beacon ace-window)))
 '(powerline-default-separator (quote arrow))
 '(spaceline-buffer-size-p nil t)
 '(spacemacs-theme-custom-colors (quote ((lnum . "#64707c"))))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "#292b2e" :background "#bc6ec5" :height 1.0 :box nil)))))
