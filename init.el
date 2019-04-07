(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar nd/conf-dir "~/.emacs.d/"
  "The absolute path to the EMACS configuration directory.")

(defvar nd/conf-main (expand-file-name "conf.org" nd/conf-dir)
  "The absolute path the main EMACS configuration file.")

(org-babel-load-file nd/conf-main)

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
    (async auctex avy biblio biblio-core company dash ess evil-magit evil-surround f find-file-in-project flycheck flyspell-correct ghub git-commit goto-chg helm helm-bibtex helm-core hydra intero ivy js2-mode julia-mode magit magit-popup markdown-mode math-symbol-lists org-ref package-lint parsebib pdf-tools poly-markdown poly-noweb polymode powerline pyvenv spaceline spacemacs-theme tablist use-package websocket which-key with-editor org-super-agenda company-anaconda anaconda-mode evil evil-collection excorporate helm-org-rifle org-brain ruby-test-mode rocy-test-mode robe pkgbuild-mode sound-wav inf-ruby ruby-inf ruby-ing dired-narrow lua-mode js-comint dash-functional ein emacs-ipython-notebook flycheck-package dashboard android-mode poly-R outline-magic toc-org ebib company-auctex dired-du helm-swoop evil-replace-with-register evil-commentary helm-flyspell csv-mode calf-org rainbow-delimiters-mode gtklp delight evil-org-agenda calfw calfw-org elpy diff-hl beacon ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "#292b2e" :background "#bc6ec5" :height 1.0 :box nil)))))
(put 'upcase-region 'disabled nil)
