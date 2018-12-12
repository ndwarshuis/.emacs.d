(use-package spacemacs-theme
  :defer t
  :config
  (setq spacemacs-theme-custom-colors '((lnum . "#64707c"))))

(defvar nd/theme 'spacemacs-dark)
(defvar nd/theme-window-loaded nil)
(defvar nd/theme-terminal-loaded nil)

;; required for emacsclient/daemon setup
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (unless nd/theme-window-loaded
                      (if nd/theme-terminal-loaded
                          (enable-theme nd/theme)
                        (load-theme nd/theme t))
                      (setq nd/theme-window-loaded t))
                  (unless nd/theme-terminal-loaded
                    (if nd/theme-window-loaded
                        (enable-theme nd/theme)
                      (load-theme nd/theme t))
                    (setq nd/theme-terminal-loaded t)))))
  (progn
    (load-theme nd/theme t)
    (if (display-graphic-p)
        (setq nd/theme-window-loaded t)
      (setq nd/theme-terminal-loaded t))))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow
        spaceline-buffer-size-p nil)
  (spaceline-spacemacs-theme))

(line-number-mode 1)
(column-number-mode 1)

(use-package delight
  :ensure t)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq pop-up-windows nil) ; no popups (eg ediff)

(set-default 'truncate-lines t)

(setq scroll-conservatively 100)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defalias 'yes-or-no-p 'y-or-n-p) ; eliminate yes or no prompt on killing procs

(use-package beacon
  :ensure t
  :delight
  :init
  (beacon-mode 1))

(use-package which-key
  :ensure t
  :delight
  :init
  (which-key-mode))

(use-package helm
  :ensure t
  :delight
  :init
  (helm-mode 1)
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-max-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-scroll-amount 8)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  (helm-autoresize-mode 1)
  (require 'helm-config))

(use-package helm-swoop
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :delight
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (inferior-ess-mode . rainbow-delimiters-mode)
   (ess-mode . rainbow-delimiters-mode)
   (LaTeX-mode . rainbow-delimiters-mode)
   (Tex-latex-mode . rainbow-delimiters-mode)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-background t)
  (custom-set-faces '(aw-leading-char-face 
                      ((t (:foreground "#292b2e"
                           :background "#bc6ec5"
                           :height 1.0
                           :box nil))))))

(use-package avy
  :ensure t
  :config
  (setq avy-background t))

(use-package sudo-edit
  :ensure t)

(use-package undo-tree
  :ensure t
  :delight
  :config
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-use-dashes t)
  :hook
  (prog-mode . fci-mode))

(use-package rainbow-mode
  :ensure t)

(use-package async
  :ensure t
  :delight dired-async-mode
  :init
  (dired-async-mode 1))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . (lambda () (csv-align-fields nil (point-min) (point-max)))))

(use-package markdown-mode
  :ensure t)

(use-package polymode
  :ensure t
  :after markdown-mode
  :mode
  (("\\.Rmd\\'" . poly-markdown+r-mode)
   ("\\.rmd\\'" . poly-markdown+r-mode))
  :config
  (require 'poly-R)
  (require 'poly-markdown))

(org-babel-load-file (expand-file-name "lib/lib.org" nd/conf-dir))

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package company
  :ensure t
  :delight " ©"
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3))

(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save
                                              idle-change
                                              mode-enabled)
        flycheck-idle-change-delay 2
        flycheck-error-list-minimum-level 'warning
        flycheck-navigation-minimum-level 'warning))

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :hook
  ((prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;; (electric-pair-mode t)

(use-package flyspell-correct-helm
  :ensure t
  :after (helm flyspell))

;; (add-hook 'flyspell-mode-hook 'flyspell-buffer)

(add-hook 'prog-mode-hook #'prettify-symbols-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)

(add-hook 'emacs-lisp-mode-hook 'company-mode)

(defun nd/init-ess-company ()
  "Set the company backends for ess modes."
  (setq-local company-backends '((company-R-objects company-R-args))))

(use-package ess
  :ensure t
  :init
  (load "ess-site")
  :hook
  ((ess-mode . flycheck-mode)
   (ess-mode . company-mode)
   (ess-mode . nd/init-ess-company)
   (ess-mode . prettify-symbols-mode)
   (ess-mode . fci-mode)

   (inferior-ess-mode . company-mode)
   (inferior-ess-mode . nd/init-ess-company)
   (inferior-ess-mode . prettify-symbols-mode))
  :config
  (setq inferior-R-args "--quiet --no-save"
        ess-history-file "session.Rhistory"
        ess-history-directory (substitute-in-file-name "${XDG_CONFIG_HOME}/r/")))

(elpy-enable)

;; make python tabs 4 chars
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-offset 4)))
        
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--colors=Linux --profile=default")

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-errors nil))
  
(use-package intero
  :ensure t
  :after haskell-mode
  :hook
  (haskell-mode . intero-mode))

(add-hook 'haskell-mode-hook #'subword-mode)

(add-hook 'LaTeX-mode-hook #'flycheck-mode)
(add-hook 'Tex-latex-mode-hook #'flycheck-mode)

(defun nd/init-company-auctex ()
  "Set the company backends for auctex modes."
  (setq-local company-backends '((company-auctex-labels
                                  company-auctex-bibs
                                  company-auctex-macros
                                  company-auctex-symbols
                                  company-auctex-environments
                                  ;; company-latex-commands
                                  company-math-symbols-latex
                                  company-math-symbols-unicode))))

(use-package company-math
  :ensure t
  :after company
  :config
  (setq company-math-allow-unicode-symbols-in-faces '(font-latex-math-face)
        company-math-disallow-latex-symbols-in-faces nil))

(use-package company-auctex
  :ensure t
  :after (company company-math)
  :hook
  ((LaTeX-mode . company-mode)
   (LaTeX-mode . nd/init-company-auctex)
   (Tex-latex-mode . company-mode)
   (Tex-latex-mode . nd/init-company-auctex)))

(defun nd/turn-on-auto-fill-maybe ()
  "Prompts user to turn on `auto-fill-mode'."
  (when (y-or-n-p "Activate Auto Fill Mode? ")
    (turn-on-auto-fill)))
  
(add-hook 'LaTeX-mode-hook #'nd/turn-on-auto-fill-maybe)
(add-hook 'LaTeX-mode-hook #'fci-mode)

(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))

(org-babel-load-file (expand-file-name "org/org.org" nd/conf-dir))

(defun nd/helm-set-printer-name ()
  "Set the printer name using helm-completion to select printer."
  (interactive)
  (let ((pl (or helm-ff-printer-list (helm-ff-find-printers))))
    (if pl (setq printer-name (helm-comp-read "Printer: " pl)))))

(use-package magit
  :ensure t
  :config
  :delight auto-revert-mode
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50))

(setq dired-no-confirm '(move copy))

(if (file-exists-p "/usr/bin/7z")
    (add-to-list 'dired-compress-files-alist
                    '("\\.7z\\'" . "7z a %o %i")))

(if (file-exists-p "/usr/bin/lrzip")
    (progn
      (add-to-list 'dired-compress-files-alist
                   '("\\.lrz\\'" . "lrzip -L 9 -o %o %i &"))
      (add-to-list 'dired-compress-files-alist
                   '("\\.lzo\\'" . "lrzip -l -L 9 -o %o %i &"))
      (add-to-list 'dired-compress-files-alist
                   '("\\.zpaq\\'" . "lrzip -z -L 9 -o %o %i &"))))

;; NOTE: this must be after the shorter lrz algos otherwise it will
;; always default to .lrz and not .tar.lrz
(if (file-exists-p "/usr/bin/lrztar")
    (progn
      (add-to-list 'dired-compress-files-alist
                   '("\\.tar\\.lrz\\'" . "lrztar -L 9 -o %o %i &"))
      (add-to-list 'dired-compress-files-alist
                   '("\\.tar\\.lzo\\'" . "lrztar -l -L 9 -o %o %i &"))
      (add-to-list 'dired-compress-files-alist
                   '("\\.tar\\.zpaq\\'" . "lrztar -z -L 9 -o %o %i &"))))

(setq dired-listing-switches "-Alh")

;; from here:
;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html#Dired
(require 'gnus-dired)

(eval-after-load 'gnus-dired
  '(defun gnus-dired-mail-buffers ()
     "Return a list of active mu4e message buffers."
     (let (buffers)
       (save-current-buffer
         (dolist (buffer (buffer-list t))
           (set-buffer buffer)
           (when (and (derived-mode-p 'message-mode)
                      (null message-sent-message-via))
             (push (buffer-name buffer) buffers))))
       (nreverse buffers))))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(use-package dired-du
  :ensure t
  :config
  (setq dired-du-size-format t))

(defun nd/helm-devices ()
  "Mount, unmount, and navigate to removable media using helm."
  (interactive)
  (let* ((mounted (mapcar
                   (lambda (d)
                     `(,(file-name-base d) . ,d))
                   (nd/get-mounted-directories)))
         (mountable (seq-filter
                     (lambda (d) (not (member (car d) (mapcar #'car mounted))))
                     (nd/get-mountable-devices))))
    (helm
     :sources
     (list
      (helm-build-sync-source "Mounted Devices"
        :candidates mounted
        :action
        '(("Open" . (lambda (s) (find-file s)))
          ("Unmount" . (lambda (s) (start-process "unmount" nil "udevil" "unmount" s)))))
      (helm-build-sync-source "Mountable Devices"
        :candidates mountable
        :action
        '(("Mount and Follow" . (lambda (s)
                                  (nd/mount-device s)
                                  (find-file (nd/get-mountpoint s))))
          ("Mount" . (lambda (s) (nd/mount-device s))))))
     :buffer "*helm device buffer*"
     :prompt "Device: ")))

(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent
      mu4e-maildir "/mnt/data/Mail"

      mu4e-attachment-dir "~/Downloads"
      
      mu4e-view-show-images t
      mu4e-headers-show-target nil
      
      mu4e-view-show-addresses t

      message-kill-buffer-on-exit t
      
      mu4e-change-filenames-when-moving t

      mu4e-confirm-quit nil

      mu4e-view-prefer-html t

      mu4e-compose-dont-reply-to-self t
      
      mu4e-get-mail-command "systemctl --user start mbsync"

      user-full-name "Dwarshuis, Nathan J")

(setq mu4e-headers-fields '((:human-date . 11)
                            (:flags . 5)
                            (:from . 22)
                            (:thread-subject))
      mu4e-headers-date-format "%F"
      mu4e-headers-time-format "%R"
      mu4e-use-fancy-chars nil)

;; necessary for the header macros below
(require 'nnheader)

(defun nd/message-insert-citation-header ()
  "Insert the header of the reply message."
  (let* ((h message-reply-headers)
         (sep "________________________________")
         (from (concat "From: " (mail-header-from h)))
         (date (concat "Sent: " (mail-header-date h)))
         (to (concat "To: " user-full-name))
         (subj (concat "Subject: " (message-strip-subject-re (mail-header-subject h)))))
    (insert (string-join `("" ,sep ,from ,date ,to ,subj "") "\n"))))
    
(setq message-citation-line-function 'nd/message-insert-citation-header)

(setq message-yank-prefix "")
(setq message-yank-cited-prefix "")
(setq message-yank-empty-prefix "")

(setq
 mu4e-compose-pre-hook
 (lambda ()
   (let* ((msg mu4e-compose-parent-message)
          (html (and msg (plist-get msg :body-html)))
          ;; oops, mu4e screwed up
          (mu4e-html2text-command
           (if (file-exists-p "/usr/bin/html2text")
               "html2text --ignore-emphasis --images-to-alt --body-width=0"
             'mu4e-shr2text)))
     (when (and html mu4e-view-prefer-html (member compose-type '(reply forward)))
       ;; hackity hack, since the normal mu4e-message-body-text function
       ;; does not render the desired html, do it here and force the
       ;; aforementioned function to only look at text by removing
       ;; the html
       (plist-put msg :body-txt (mu4e~html2text-shell msg mu4e-html2text-command))
       (plist-put msg :body-html nil)))))

(require 'smtpmail)
;; (require 'smtpmail-async)
;; (require 'secrets)
;; (setq secrets-enabled t)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)
(add-to-list 'auth-sources (expand-file-name "~/.emacs.d/.authinfo_mu4e.gpg"))
;; (add-to-list 'auth-sources "secrets:default")

(setq mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask-if-none
      mu4e-user-mail-address-list '("natedwarshuis@gmail.com" "ndwarshuis3@gatech.edu" "ndwarsh@emory.edu")
      
      mu4e-contexts
      `( ,(make-mu4e-context
           :name "personal"
           :match-func
           (lambda (msg)
             (when msg
               (let ((pfx (mu4e-message-field msg :maildir)))
                 (string-prefix-p "/gmail" pfx))))
           :vars '((mu4e-trash-folder . "/gmail/trash")
                   (mu4e-drafts-folder . "/gmail/drafts")
                   (mu4e-sent-folder . "/gmail/sent")
                   (mu4e-refile-folder . "/gmail/archive")
                   (mu4e-sent-messages-behavior . delete)
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-service . 587)
                   (smtpmail-smtp-user . "natedwarshuis@gmail.com")
                   (user-mail-address . "natedwarshuis@gmail.com")
                   (mu4e-maildir-shortcuts .
                                           (("/gmail/inbox" . ?i)
                                            ("/gmail/sent" . ?s)
                                            ("/gmail/trash" . ?t)
                                            ("/gmail/drafts" . ?d)
                                            ("/gmail/archive" . ?a)))))
         ,(make-mu4e-context
           :name "gatech"
           :match-func
           (lambda (msg)
             (when msg
               (let ((pfx (mu4e-message-field msg :maildir)))
                 (string-prefix-p "/gatech" pfx))))
           :vars '((mu4e-trash-folder . "/gatech/trash")
                   (mu4e-drafts-folder . "/gatech/drafts")
                   (mu4e-sent-folder . "/gatech/sent")
                   (mu4e-refile-folder . "/gatech/archive")
                   (mu4e-sent-messages-behavior . sent)
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-server . "smtp.office365.com")
                   (smtpmail-smtp-service . 587)
                   (smtpmail-smtp-user . "ndwarshuis3@gatech.edu")
                   (user-mail-address . "ndwarshuis3@gatech.edu")
                   (mu4e-maildir-shortcuts .
                                           (("/gatech/inbox" . ?i)
                                            ("/gatech/sent" . ?s)
                                            ("/gatech/trash" . ?t)
                                            ("/gatech/drafts" . ?d)
                                            ("/gatech/archive" . ?a)))))
         ,(make-mu4e-context
           :name "emory"
           :match-func
           (lambda (msg)
             (when msg
               (let ((pfx (mu4e-message-field msg :maildir)))
                 (string-prefix-p "/emory" pfx))))
           :vars '((mu4e-trash-folder . "/emory/trash")
                   (mu4e-drafts-folder . "/emory/drafts")
                   (mu4e-sent-folder . "/emory/sent")
                   (mu4e-refile-folder . "/emory/archive")
                   (mu4e-sent-messages-behavior . sent)
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-server . "smtp.office365.com")
                   (smtpmail-smtp-service . 587)
                   (smtpmail-smtp-user . "ndwarsh@emory.edu")
                   (user-mail-address . "ndwarsh@emory.edu")
                   (mu4e-maildir-shortcuts .
                                           (("/emory/inbox" . ?i)
                                            ("/emory/sent" . ?s)
                                            ("/emory/trash" . ?t)
                                            ("/emory/drafts" . ?d)
                                            ("/emory/archive" . ?a)))))))

(use-package org-mu4e
  :after (org mu4e)
  :config
  (setq
   ;; for using mu4e in org-capture templates
   org-mu4e-link-query-in-headers-mode nil
   ;; for composing rich-text emails using org mode
   org-mu4e-convert-to-html t))

(setq mu4e-compose-signature-auto-include nil

      mu4e-compose-signature
      (string-join
       '("Nathan Dwarshuis"
         ""
         "PhD Student - Biomedical Engineering - Krish Roy Lab"
         "Georgia Institute of Technology and Emory University"
         "ndwarshuis3@gatech.edu")
       "\n"))

(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
(add-hook 'mu4e-view-mode-hook 'turn-off-auto-fill)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

(add-hook 'mu4e-compose-mode-hook (lambda () (flyspell-mode 1)))

(load "auctex.el" nil t t)
(require 'tex-mik)

(setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
                                    "dvi2tty")
                                   ((output-dvi style-pstricks)
                                    "dvips and gv")
                                   (output-dvi "xdvi")
                                   (output-pdf "Okular")
                                   (output-html "xdg-open")))

;; remove ugly section size
(setq font-latex-fontify-sectioning 'color)

(add-hook 'LaTeX-mode-hook (lambda () (outline-minor-mode 1)))
(add-hook 'Tex-latex-mode-hook (lambda () (outline-minor-mode 1)))

(use-package outline-magic
  :ensure t
  :after outline)

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq reftex-default-bibliography (expand-file-name "~/BibTeX/master.bib")
        org-ref-bibliography-notes (expand-file-name "~/BibTeX/notes.org")
        org-ref-default-bibliography (expand-file-name "~/BibTeX/master.bib")))

(use-package helm-bibtex
  :ensure t
  :after helm
  :config
  (setq bibtex-completion-bibliography (expand-file-name "~/BibTeX/master.bib")
        bibtex-completion-library-path (expand-file-name "~/BibTeX/pdf")
        bibtex-completion-pdf-field "File"))

(use-package ebib
  :ensure t)

(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)

(defun nd/term-send-raw-escape ()
  "Send a raw escape character to the running terminal."
  (interactive)
  (term-send-raw-string "\e"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package evil
  :ensure t
  :init
  ;; this is required to make evil collection work
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(setq sentence-end-double-space nil)

(add-to-list 'evil-motion-state-modes 'ess-help-mode)
(add-to-list 'evil-insert-state-modes 'inferior-ess-mode)

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :delight
  :config
  (evil-commentary-mode))

(use-package evil-replace-with-register
  :ensure t
  :after evil
  :config
  (evil-replace-with-register-install))

(mapc (lambda (k) (nd/move-key global-map evil-emacs-state-map (eval k)))
      '((kbd "C-s")
        (kbd "C-p")
        (kbd "C-n")
        (kbd "C-f")
        (kbd "C-b")
        (kbd "C-a")
        (kbd "C-e")
        (kbd "C-<SPC>")
        
        (kbd "C-x C-;")
        (kbd "C-x C-l")
        (kbd "C-x C-u")
        (kbd "C-x C-z")
        (kbd "C-x C-c")

        (kbd "M-c")
        (kbd "M-d")
        (kbd "M-e")
        (kbd "M-r")
        (kbd "M-f")
        (kbd "M-h")
        (kbd "M-j")
        (kbd "C-M-j")
        (kbd "M-k")
        (kbd "M-l")
        (kbd "M-m")
        (kbd "M-q")
        (kbd "M-w")
        (kbd "M-t")
        (kbd "M-u")
        (kbd "M-i")
        (kbd "M-z")
        (kbd "M-v")
        (kbd "M-/")
        (kbd "M-;")
        (kbd "M-DEL")))

(use-package evil-org
  :ensure t
  :after (evil org)
  :delight
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; some of the defaults bug me...
  (evil-define-key 'motion org-agenda-mode-map
    "t" 'nd/toggle-project-toplevel-display
    "D" 'org-agenda-day-view
    "W" 'org-agenda-week-view
    "M" 'org-agenda-month-view
    "Y" 'org-agenda-year-view
    "ct" nil
    "sC" 'nd/org-agenda-filter-non-context
    "sE" 'nd/org-agenda-filter-non-effort
    "sD" 'nd/org-agenda-filter-delegate
    "sP" 'nd/org-agenda-filter-non-peripheral
    "e" 'org-agenda-set-effort
    "ce" nil))

(use-package evil-magit
  :ensure t
  :after (evil magit))

(evil-define-key '(normal visual) 'visual-line-mode
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "0" 'beginning-of-visual-line
  "$" 'end-of-visual-line)

(defun nd/comint-char-mode-evil-insert ()
  "If not at the last line, go to the end of the buffer and enter insert mode.  Else just enter insert mode."
  (interactive)
  (if (/= (line-number-at-pos (point)) (line-number-at-pos (point-max)))
        (goto-char (point-max))))
        
(defun nd/comint-send-input-evil-insert (&optional send-input-cmd)
  "Go into insert mode after calling SEND-INPUT-CMD which is usually
the function that send the command to the interactive process in the
REPL. If no SEND-INPUT-CMD then `comint-send-input' is used."
  (interactive)
  (if send-input-cmd (funcall send-input-cmd) (comint-send-input))
  (evil-insert 1))
        
(evil-define-key '(normal insert) comint-mode-map
  (kbd "C-k") 'comint-previous-input
  (kbd "C-j") 'comint-next-input)

(evil-define-key 'normal inferior-ess-mode-map
  (kbd "RET") (lambda () nd/comint-send-input-evil-insert
                'inferior-ess-send-input))

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (add-hook 'evil-insert-state-entry-hook
                      'nd/comint-char-mode-evil-insert nil t)))

(evil-define-key 'normal intero-repl-mode-map
  (kbd "RET") 'nd/comint-send-input-evil-insert)
  
(add-hook 'intero-repl-mode-hook
          (lambda ()
            (add-hook 'evil-insert-state-entry-hook
                      'nd/comint-char-mode-evil-insert nil t)))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-mode-list
        '(company dired ediff flycheck helm minibuffer mu4e term which-key))
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(defun nd/dired-move-to-parent-directory ()
  "Move buffer to parent directory (like 'cd ..')."
  (interactive)
  (find-alternate-file ".."))

(defun nd/dired-xdg-open ()
  "Open all non-text files in external app using xdg-open.
Only regular files are considered."
  (interactive)
  (let* ((file-list (seq-filter #'file-regular-p (dired-get-marked-files)))
         (do-it (if (<= (length file-list) 5)
                    t
                  (y-or-n-p "Open more then 5 files? "))))
    (when do-it
      (mapc
       (lambda (f) (let ((process-connection-type nil))
                (start-process "" nil "xdg-open" f)))
       file-list))))

(defun nd/dired-open-with ()
  "Open marked non-text files in external app via open-with dialog
according to mime types as listed in all available desktop files."
  (interactive)
  (let* ((mf (seq-filter #'file-regular-p (dired-get-marked-files)))
         (qmf (mapcar #'shell-quote-argument mf))
         (file-mime-list (mapcar (lambda (f) (list f (nd/get-mime-type f))) qmf)))

    (if (= (length file-mime-list) 0)
        (message "No files selected")
      
      (let* ((first-pair (car file-mime-list))
             (last-pairs (cdr file-mime-list))
             mime-alist file-list)
        (setq file-list (nth 0 first-pair)
              mime-alist (nd/get-apps-from-mime (nth 1 first-pair)))
        ;; if multiple files selected, add to the selection list
        (if last-pairs
            (progn
              (setq file-list (string-join (mapcar #'car file-mime-list) " "))
              (dolist (mime (mapcar (lambda (f) (nth 1 f)) last-pairs))
                (setq mime-alist (intersection mime-alist
                                               (nd/get-apps-from-mime mime)
                                               :test #'equal)))))
        (if (= (length mime-alist) 0)
            (let* ((ml (delete-dups (mapcan #'cdr file-mime-list)))
                   (mls (string-join ml ", ")))
              (if (= (length ml) 1)
                  (message (concat "No apps found for mime type: "  mls))
                (message (concat "No common apps found for mime types: " mls))))
          (helm
           :sources (helm-build-sync-source "Apps"
                      :candidates mime-alist
                      :action '(("Open" . (lambda (f) (nd/execute-desktop-command f file-list)))))
           :buffer "*helm open with*"))))))

(defun nd/dired-sort-by ()
  "Sort current dired buffer by a list of choices presented in helm menu.
Note this assumes there are no sorting switches on `dired-ls'"
  (interactive)
  (let ((sort-alist '(("Name" . "")
                      ("Date" . "-t")
                      ("Size" . "-S")
                      ("Extension" . "-X")
                      ("Dirs First" . "--group-directories-first"))))
    (helm
     :sources
     (helm-build-sync-source "Switches"
       :candidates sort-alist
       :action
       '(("Sort" . (lambda (s) (dired-sort-other (concat dired-listing-switches " " s))))))
     :buffer "*helm sort buffer*")))

(put 'dired-find-alternate-file 'disabled nil)

(evil-define-key 'normal dired-mode-map
  "a" 'dired-find-file
  "za" 'gnus-dired-attach
  "gs" 'nd/dired-sort-by
  "^" 'nd/dired-move-to-parent-directory
  "q" 'nd/kill-current-buffer
  (kbd "<return>") 'dired-find-alternate-file
  (kbd "C-<return>") 'nd/dired-xdg-open
  (kbd "M-<return>") 'nd/dired-open-with)

(evil-define-key '(normal insert) helm-map
  (kbd "<tab>") 'helm-execute-persistent-action
  (kbd "C-<tab>") 'helm-select-action)

(evil-define-key 'insert term-raw-map
  (kbd "<escape>") 'nd/term-send-raw-escape
  (kbd "C-<escape>") 'evil-normal-state)

(add-hook 'org-mode-hook
          (lambda ()
            ;; override default TODO timestamp creation to insert the creation date
            (local-set-key (kbd "M-S-<return>") 'nd/org-insert-todo-heading-inactive-timestamp)

            ;; use the hyper keys/vim arrows with the shifters instead of shift/arrows
            (local-set-key (kbd "H-k") 'org-shiftup)
            (local-set-key (kbd "H-l") 'org-shiftright)
            (local-set-key (kbd "H-j") 'org-shiftdown)
            (local-set-key (kbd "H-h") 'org-shiftleft)

            ;; this is just a useful function I made (actually I think I stole)
            (local-set-key (kbd "C-c C-x x") 'nd/mark-subtree-done)

            ;; override default org subtree cloning with something that clones and resets
            (local-set-key (kbd "C-c C-x c") 'nd/org-clone-subtree-with-time-shift)))
            
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'org-agenda-set-tags)
            (local-set-key (kbd "C-c C-x c") 'nd/org-agenda-clone-subtree-with-time-shift)
            (local-set-key (kbd "C-c C-x C-b") 'nd/org-agenda-toggle-checkbox)))

(define-key mu4e-headers-mode-map (kbd "C-c C-l") 'org-store-link)
(define-key mu4e-view-mode-map (kbd "C-c C-l") 'org-store-link)

(define-key dired-mode-map (kbd "C-x g") 'magit)

(define-key helm-command-prefix (kbd "b") 'helm-bibtex)
(define-key helm-command-prefix (kbd "S") 'helm-swoop)
(define-key helm-command-prefix (kbd "<f8>") 'helm-resume)

(define-key helm-command-prefix (kbd "f") 'helm-flyspell-correct)
(define-key helm-command-prefix (kbd "F") 'helm-multi-files)

(define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)

(global-set-key (kbd "<f1>") 'org-agenda)
(global-set-key (kbd "<f2>") 'org-capture)
(global-set-key (kbd "<f3>") 'cfw:open-org-calendar)
(global-set-key (kbd "<f4>") 'org-clock-goto)
(global-set-key (kbd "<f5>") 'ansi-term)
(global-set-key (kbd "<f8>") 'helm-command-prefix)
(global-set-key (kbd "C-<f5>") 'nd/open-urxvt)
(global-set-key (kbd "<f12>") 'mu4e)
(global-set-key (kbd "C-<f12>") 'global-hl-line-mode)
(global-set-key (kbd "S-<f12>") 'display-line-numbers-mode)

(global-set-key (kbd "C-<SPC>") 'company-complete)

(global-set-key (kbd "C-c e") 'nd/config-visit)
(global-set-key (kbd "C-c r") 'nd/config-reload)
(global-set-key (kbd "C-c s") 'sudo-edit)

(global-set-key (kbd "C-x 2") 'nd/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'nd/split-and-follow-vertically)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x k") 'nd/kill-current-buffer)
(global-set-key (kbd "C-x C-d") 'helm-bookmarks)
(global-set-key (kbd "C-x C-c C-d") 'nd/helm-devices)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(global-set-key (kbd "C-M-S-k") 'nd/close-all-buffers)
(global-set-key (kbd "C-M-S-o") 'nd/org-close-all-buffers)
(global-set-key (kbd "C-M-S-a") 'org-agenda-kill-all-agenda-buffers)

(global-set-key (kbd "M-b") 'nd/switch-to-previous-buffer)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "M-x") 'helm-M-x)
