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

(use-package org
  :delight
  ;; source of indent-mode required here
  (org-indent-mode nil org-indent)
  (visual-line-mode)
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t
        org-directory "~/Org"
        org-modules '(org-habit org-protocol))

  (require 'org-protocol))

(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-yank-adjusted-subtrees t)

(defun nd/org-save-all-org-buffers ()
  "Save org buffers without confirmation or message (unlike default)."
  (save-some-buffers t (lambda () (derived-mode-p 'org-mode)))
  (when (featurep 'org-id) (org-id-locations-save)))

(run-at-time "00:59" 3600 #'nd/org-save-all-org-buffers)

(setq org-log-into-drawer "LOGBOOK")

(setq org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time)

(setq org-log-repeat 'note)

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (let ((heading-height 1.15))
              (set-face-attribute 'org-level-1 nil :weight 'bold :height heading-height)
              (set-face-attribute 'org-level-2 nil :weight 'semi-bold :height heading-height)
              (set-face-attribute 'org-level-3 nil :weight 'normal :height heading-height)
              (set-face-attribute 'org-level-4 nil :weight 'normal :height heading-height)
              (set-face-attribute 'org-level-5 nil :weight 'normal :height heading-height))))

(setq org-src-window-setup 'current-window
      org-src-fontify-natively t
      org-edit-src-content-indentation 0)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-insert-heading-respect-content t)

(defun nd/mark-subtree-keyword (new-keyword &optional exclude)
  "Mark all tasks in a subtree with NEW-KEYWORD unless original
keyword is in the optional argument EXCLUDE."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (not (listp exclude))
        (error "exlude must be a list if provided"))
    (save-excursion
      (while (< (point) subtree-end)
        (let ((keyword (nd/is-todoitem-p)))
          (if (and keyword (not (member keyword exclude)))
              (org-todo new-keyword)))
        (outline-next-heading)))))

(defun nd/mark-subtree-done ()
  "Mark all tasks in subtree as DONE unless they are already CANC."
  (interactive)
  (nd/mark-subtree-keyword "DONE" '("CANC")))

(defun nd/org-clone-subtree-with-time-shift (n &optional shift)
  "Like `org-clone-subtree-with-time-shift' except it resets checkboxes
and reverts all todo keywords to TODO."
  (interactive "nNumber of clones to produce: ")
    
  (let ((shift (or (org-entry-get nil "TIME_SHIFT" 'selective)
                   (read-from-minibuffer
                    "Date shift per clone (e.g. +1w, empty to copy unchanged): "))))
    (condition-case err
        (progn
          (save-excursion
            ;; clone once and reset
            (org-clone-subtree-with-time-shift 1 shift)
            (org-forward-heading-same-level 1 t)
            (org-reset-checkbox-state-subtree)
            (nd/mark-subtree-keyword "TODO")
            (call-interactively 'nd/org-log-delete)
            (org-cycle)
            ;; clone reset tree again if we need more than one clone
            (if (> n 1)
                (let ((additional-trees (- n 1)))
                  (org-clone-subtree-with-time-shift additional-trees shift)
                  (dotimes (i additional-trees)
                    (org-forward-heading-same-level 1 t)
                    (org-cycle))))))
      (error (message "%s" (error-message-string err))))))

(defun nd/org-log-delete ()
  "Delete logbook drawer of subtree."
  (interactive)
  (save-excursion
    (goto-char (org-log-beginning))
    (when (save-excursion
            (save-match-data
              (beginning-of-line 0)
              (search-forward-regexp org-drawer-regexp)
              (goto-char (match-beginning 1))
              (looking-at "LOGBOOK")))
      (org-mark-element)
      (delete-region (region-beginning) (region-end))
      (org-remove-empty-drawer-at (point)))))

(defun nd/org-insert-todo-heading-inactive-timestamp ()
  "Insert a todo heading but also insert inactive timestamp set to now."
  (interactive)
  ;; a bit redundant and hacky, with the advantage of being effective
  (when (not (org-insert-item 'checkbox))
    (call-interactively 'org-insert-todo-heading)
    (insert "\n")
    (funcall-interactively 'org-time-stamp-inactive '(16))
    (forward-line -1)))

(defun nd/org-delete-subtree ()
  "Delete the entire subtree under the current heading without sending to kill ring."
  (interactive)
  (org-back-to-heading t)
  (delete-region (point) (+ 1 (save-excursion (org-end-of-subtree)))))

(defmacro nd/org-agenda-cmd-wrapper (get-head &rest body)
  "Wraps commands in BODY in necessary code to allow commands to be
called from the agenda buffer. Particularly, this wrapper will
navigate to the original header, execute BODY, then update the agenda
buffer."
  '(org-agenda-check-no-diary)
  `(let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			           (org-agenda-error)))
	     (buffer (marker-buffer hdmarker))
	     (pos (marker-position hdmarker))
	     (inhibit-read-only t)
	     newhead)
    (org-with-remote-undo buffer
	  (with-current-buffer buffer
	    (widen)
	    (goto-char pos)
	    (org-show-context 'agenda)
        ,@body
	    (when ,get-head (setq newhead (org-get-heading))))
	  (if ,get-head
          (org-agenda-change-all-lines newhead hdmarker)
        (org-agenda-redo))
	  (beginning-of-line 1))))
  
(defun nd/org-agenda-toggle-checkbox ()
  "Toggle checkboxes in org agenda view using `org-toggle-checkbox'."
  (interactive)
  (nd/org-agenda-cmd-wrapper
   t
   (call-interactively #'org-toggle-checkbox)))

(defun nd/org-agenda-clone-subtree-with-time-shift ()
  "Apply `nd/org-clone-subtree-with-time-shift' to an agenda entry.
It will clone the last entry in the selected subtree."
  (interactive)
  (nd/org-agenda-cmd-wrapper
   nil
   (org-end-of-subtree)
   (call-interactively #'nd/org-clone-subtree-with-time-shift)))

(defun nd/org-agenda-delete-subtree ()
  "Apply `nd/org-delete-subtree' to an agenda entry."
  (interactive)
  (nd/org-agenda-cmd-wrapper
   nil
   (call-interactively #'nd/org-delete-subtree)))

(setq org-columns-default-format
      "%25ITEM %4TODO %TAGS %5Effort{:} %DELEGATE(DEL)")

(set-face-attribute 'org-column nil :background "#1e2023")
;; org-columns-summary-types

(use-package calfw
  :ensure t
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package calfw-org
  :ensure t
  :after calfw
  :config
  (setq cfw:org-agenda-schedule-args
        '(:deadline :timestamp)))

(defun nd/org-todo-position (buffer alist)
  (let ((win (car (cl-delete-if-not
                   (lambda (window)
                     (with-current-buffer (window-buffer window)
                       (memq major-mode
                             '(org-mode org-agenda-mode))))
                   (window-list)))))
    (when win
      (let ((new (split-window win -4 'below)))
        (set-window-buffer new buffer)
        new))))

(defun nd/org-todo-window-advice (orig-fn)
  "Advice to fix window placement in `org-fast-todo-selection'."
  (let  ((override '("\\*Org todo\\*" nd/org-todo-position)))
    (add-to-list 'display-buffer-alist override)
    (nd/with-advice
        ((#'org-switch-to-buffer-other-window :override #'pop-to-buffer))
      (unwind-protect (funcall orig-fn)
        (setq display-buffer-alist
              (delete override display-buffer-alist))))))

(advice-add #'org-fast-todo-selection :around #'nd/org-todo-window-advice)

(defun nd/org-tag-window-advice (orig-fn current inherited table &optional todo-table)
  "Advice to fix window placement in `org-fast-tags-selection'."
  (nd/with-advice
      ((#'delete-other-windows :override #'ignore)
       ;; pretty sure I just got lucky here...
       (#'split-window-vertically :override #'(lambda (&optional size)
                                                (split-window-below (or size -9)))))
    (unwind-protect (funcall orig-fn current inherited table todo-table))))

(advice-add #'org-fast-tag-selection :around #'nd/org-tag-window-advice)

(defun nd/org-capture-position (buffer alist)
  (let ((new (split-window (get-buffer-window) -14 'below)))
    (set-window-buffer new buffer)
    new))

(defun nd/org-capture-window-advice (orig-fn table title &optional prompt specials)
  "Advice to fix window placement in `org-capture-select-template'."
  (let  ((override '("\\*Org Select\\*" nd/org-capture-position)))
    (add-to-list 'display-buffer-alist override)
    (nd/with-advice
        ((#'org-switch-to-buffer-other-window :override #'pop-to-buffer))
      (unwind-protect (funcall orig-fn table title prompt specials)
        (setq display-buffer-alist
              (delete override display-buffer-alist))))))

(advice-add #'org-mks :around #'nd/org-capture-window-advice)

(setq org-html-doctype "html5")

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; (defvar nd/org-export-publishing-directory
;;   (expand-file-name "~/Downloads/org-exports")
;;   "The target directory to for all org exports.")

;; (defun nd/org-export-output-file-name (orig-fun extension &optional subtreep pub-dir)
;;   "Change the target export directory for org exports."
;;   (unless pub-dir
;;     (setq pub-dir nd/org-export-publishing-directory)
;;     (unless (file-directory-p pub-dir)
;;       (make-directory pub-dir)))
;;   (apply orig-fun extension subtreep pub-dir nil))

;; (advice-add 'org-export-output-file-name :around #'nd/org-export-output-file-name)

(add-to-list 'load-path "~/.emacs.d/untracked/org-gantt/")
(require 'org-gantt)

(add-to-list 'org-structure-template-alist
             '("og" "#+BEGIN: org-gantt-chart\n?\n#+END"))

(setq org-todo-keywords
      '((sequence
         ;; default undone state
         "TODO(t/!)"

         ;; undone but available to do now (projects only)
         "NEXT(n/!)" "|"

         ;; done and complete
         "DONE(d/!)")

        (sequence
         ;; undone and waiting on some external dependency
         "WAIT(w@/!)"
         
         ;; undone but signifies tasks on which I don't wish to focus at the moment
         "HOLD(h@/!)" "|"

         ;; done but not complete
         "CANC(c@/!)")))

(setq   org-todo-keyword-faces
      '(("TODO" :foreground "light coral" :weight bold)
        ("NEXT" :foreground "khaki" :weight bold)
        ("DONE" :foreground "light green" :weight bold)
        ("WAIT" :foreground "orange" :weight bold)
        ("HOLD" :foreground "violet" :weight bold)
        ("CANC" :foreground "deep sky blue" :weight bold)))

(setq org-tag-alist
      ;; (@) gtd location context
      '((:startgroup)
        ("@errand" . ?e)
        ("@home" . ?h)
        ("@work" . ?w)
        ("@travel" . ?r)
        (:endgroup)
        
        ;; (#) gtd resource context 
        ("#laptop" . ?l)
        ("#tcult" . ?t)
        ("#phone" . ?p)
        
        ;; (%) misc tags 
        ;; denotes reference information
        ("%note" . ?n)
        
        ;; incubator
        ("%inc" . ?i)
        
        ;; denotes tasks that need further subdivision to turn into true project
        ("%subdiv" . ?s)
        
        ;; catchall to mark important headings, usually for meetings
        ("%flag" . ?f)
        
        ;; (_) life categories, used for gtd priority context 
        (:startgroup)
        ("_env" . ?E)
        ("_fin" . ?F)
        ("_int" . ?I)
        ("_met" . ?M)
        ("_phy" . ?H)
        ("_pro" . ?P)
        ("_rec" . ?R)
        ("_soc" . ?S)
        (:endgroup)))

(defun nd/add-tag-face (fg-name prefix)
  "Adds list of cons cells to org-tag-faces with foreground set to fg-name.
  Start and end specify the positions in org-tag-alist which define the tags
  to which the faces are applied"
  (dolist (tag (nd/filter-list-prefix prefix (mapcar #'car org-tag-alist)))
    (push `(,tag . (:foreground ,fg-name)) org-tag-faces)))

(setq org-tag-faces '())

(nd/add-tag-face "PaleGreen" "@")
(nd/add-tag-face "SkyBlue" "#")
(nd/add-tag-face "PaleGoldenrod" "%")
(nd/add-tag-face "violet" "_")

(mapc (lambda (i) (add-to-list 'org-default-properties i))
      ;; defines a repeater group
      '("PARENT_TYPE"
        ;; defines the time shift for repeater groups

        "TIME_SHIFT"
        ;; assigns another person/entity to a task (experimental)

        "DELEGATE"

        ;; defines a goal (not currently used)
        "GOAL"))

(setq org-global-properties
      '(("PARENT_TYPE_ALL" . "periodical iterator")
        ("Effort_ALL" . "0:05 0:15 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00"))

      org-use-property-inheritance
      '("PARENT_TYPE" "TIME_SHIFT"))

(defun nd/org-timestamp-future (days)
  "Inserts an active org timestamp DAYS after the current time."
  (format-time-string (org-time-stamp-format nil)
                      (time-add (current-time) (days-to-time 1))))

(let ((capfile "~/Org/capture.org"))
  (setq org-capture-templates
        ;; regular TODO task
        `(("t" "todo" entry (file ,capfile)
           "* TODO %?\n%U\ndeliverable: \n")

          ;; for useful reference information that may be grouped with tasks
          ("n" "note" entry (file ,capfile)
           "* %?  :\\%note:\n%U\n")

          ;; for non-actionable events that happen at a certain time
          ("a" "appointment" entry (file ,capfile)
           "* %?\n%U\n%^t\n")

          ;; like appointment but multiple days
          ("s" "appointment-span" entry (file ,capfile)
           "* %?\n%U\n%^t--%^t\n")

          ;; task with a deadline
          ("d" "deadline" entry (file ,capfile)
           "* TODO %?\nDEADLINE: %^t\ndeliverable:\n%U\n")

          ;; for converting mu4e emails to tasks, defaults to next-day deadline
          ("e" "email" entry (file ,capfile)
           "* TODO Respond to %:fromname; Re: %:subject  :#laptop:\nDEADLINE: %(nd/org-timestamp-future 1)\n%U\n%a\n")

          ;; for interruptions that produce useful reference material
          ("m" "meeting" entry (file ,capfile)
           "* meeting with%?  :\\%note:\n%U\n")

          ;; for capturing web pages with web browser
          ("p" "org-protocol" entry (file ,capfile)
           "* %^{Title} :\\%note:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
           :immediate-finish t)

          ;; or capturing links with web browser
          ("L" "org-protocol link" entry (file ,capfile)
           "* %^{Title} :\\%note:\n[[%:link][%:description]]\n%U"
           :immediate-finish t))))

(add-hook 'org-capture-mode-hook (lambda () (evil-append 1)))

(setq org-refile-targets '((nil :maxlevel . 9)
                           ("~/Org/reference/idea.org" :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-indirect-buffer-display 'current-window)

(setq org-refile-target-verify-function
      (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))

;; TODO this no work, although does work if var is global
;; redfining the targets works for now
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (when (equal (buffer-name) "*Org Agenda(A)*")
              (setq-local org-refile-targets
                          '(("~/Org/journal/goals.org" :maxlevel . 9))))))
;;                           (lambda () (when (org-entry-get nil "GOAL") t))))))
;; (setq org-refile-targets '((nil :maxlevel . 9)
;;                            ("~/Org/reference/idea.org" :maxlevel . 9)
;;                            ("~/Org/journal/goals.org" :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9))

(setq org-clock-history-length 23
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-report-include-clocking-task t)

(defun nd/are-conflicting-p (ts-a ts-b)
  "Return t if timestamps TS-A and TS-B conflict."
  (let* ((earlier-a (car ts-a))
         (earlier-b (car ts-b))
         (later-b (+ earlier-b (nth 1 ts-b))))
    (and (>= earlier-a earlier-b) (<= earlier-a later-b))))

(defun nd/detect-conflict (ts ts-list conlist)
  "Recursively determine if timestamp TS conflicts with anything in TS-LIST.
If detected, conflict pair is added to CONLIST."
  (let ((next-ts (car ts-list))
        (rem-ts (cdr ts-list)))
    (if (nd/are-conflicting-p ts next-ts)
        (progn
          (setq conlist (cons (list ts next-ts) conlist))
          (if rem-ts (nd/detect-conflict ts rem-ts conlist) conlist))
      conlist)))
  
(defun nd/build-conlist (ts-list conlist)
  "Recursively build a list of timestamp conflicts from TS-LIST.

TS-LIST is comprised of entries in the form (staring-ts timerange marker) 
where timerange is 0 for singular timestamps and a positive number for
anything with to times or a timestamp range.
Detected conflicts are stored in CONLIST as pairs of conflicting ts
entries from the TS-LIST."
  (let ((cur-ts (car ts-list))
        (rem-ts (cdr ts-list)))
    (if rem-ts
        (nd/build-conlist rem-ts (nd/detect-conflict cur-ts rem-ts conlist))
      conlist)))

(defconst nd/org-tsm-regexp
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]+0-9>\r\n -]+? \\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}?\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
  "Regular expression for timestamps with two times.")

(defun nd/get-timestamps ()
  "Get the org-marker and timestamp(s) (multiple if range) or current heading."
  ;; TODO, what if I care about more than just TIMESTAMPs
  (let* ((ts (org-entry-get nil "TIMESTAMP"))
         (marker (point-marker))
         (ts-range 0)
         (ts-entry))
    (when ts
      (cond
       ;; match timestamps that have two times
       ((string-match nd/org-tsm-regexp ts)
        (let* ((ts1 (concat (match-string 1 ts) (match-string 2 ts)))
               (ts2 (concat (match-string 1 ts) (match-string 3 ts)))
               (ft1 (org-2ft ts1))
               (ft2 (org-2ft ts2)))
          (setq ts-entry ft1)
          (setq ts-range (- ft2 ft1))))
       
       ;; match timestamps that have a range (eq two timestamps)
       ((string-match org-tr-regexp ts)
        (let* ((ts1 (match-string 1 ts))
               (ts2 (match-string 2 ts))
               (ft1 (org-2ft ts1))
               (ft2 (org-2ft ts2)))
          (setq ts-entry ft1)
          (setq ts-range (- ft2 ft1))))
       
       ;; match timestamps with only one time
       (t (setq ts-entry (org-2ft ts))))
      (list ts-entry ts-range marker ts))))

(defun nd/build-conflict-list ()
  "Scan all org files and make a list of all timestamps that conflict."
  (let ((files (org-agenda-files))
        max-reached ts-list cur-index conflicts)
    ;; get all timestamps from org buffers
    (dolist (f files ts-list)
      (with-current-buffer
        (find-file-noselect f)
        (goto-char (point-min))
        (when (not (outline-on-heading-p)) (outline-next-heading))
        (setq max-reached nil)
        (while (not max-reached)
          (let ((new-ts (nd/get-timestamps)))
            (if new-ts (setq ts-list (cons new-ts ts-list))))
          (unless (outline-next-heading) (setq max-reached t)))))

    ;; sort the timestamp list
    ;; TODO, need to make range-aware
    (setq ts-list (sort ts-list (lambda (a b) (< (car a) (car b)))))

    ;; build a list of conflicts
    (nd/build-conlist ts-list conflicts)))

(defun nd/get-conflict-header-text (conflict-marker)
  "Return string with text properties representing the org header for
MARKER for use in the conflict agenda view."
  (let* ((props (list
                 'face nil
		         'done-face 'org-agenda-done
		         'org-not-done-regexp org-not-done-regexp
		         'org-todo-regexp org-todo-regexp
		         'org-complex-heading-regexp org-complex-heading-regexp
		         'mouse-face 'highlight))
		         ;; 'help-echo
		         ;; (format "mouse-2 or RET jump to org file %s"
			     ;;         (abbreviate-file-name buffer-file-name))))
	     marker priority category level tags todo-state
	     ts-date ts-date-type ts-date-pair
	     txt beg end inherited-tags todo-state-end-pos)

    (with-current-buffer (marker-buffer conflict-marker) 
      (save-excursion
	    (goto-char conflict-marker)

	    (setq marker (org-agenda-new-marker (point))
	          category (org-get-category)
	          ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
	          ts-date (car ts-date-pair)
	          ts-date-type (cdr ts-date-pair)
	          txt (org-get-heading t)
	          inherited-tags
	          (or (eq org-agenda-show-inherited-tags 'always)
		          (and (listp org-agenda-show-inherited-tags)
		               (memq 'todo org-agenda-show-inherited-tags))
		          (and (eq org-agenda-show-inherited-tags t)
		               (or (eq org-agenda-use-tag-inheritance t)
			               (memq 'todo org-agenda-use-tag-inheritance))))
	          tags (org-get-tags-at nil (not inherited-tags))
	          level (make-string (org-reduced-level (org-outline-level)) ? )
	          txt (org-agenda-format-item "" txt level category tags t)
	          priority (1+ (org-get-priority txt)))
        
	    (org-add-props txt props
	      'org-marker marker 'org-hd-marker marker
	      'priority priority
	      'level level
	      'ts-date ts-date
	      'type "timestamp")))))

(defun nd/org-conflicts (&optional arg)
  (interactive "P")

  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))

  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))

  (let* ((today (org-today))
	     (date (calendar-gregorian-from-absolute today))
	     (completion-ignore-case t)
         (org-agenda-prefix-format '((agenda . "  %-12:c %-5:e ")))
	     rtn rtnall files file pos)

    (catch 'exit
      (when org-agenda-sticky (setq org-agenda-buffer-name "*Org Conflicts*"))

      (org-agenda-prepare)
      ;; (org-compile-prefix-format 'todo)
      (org-compile-prefix-format 'agenda)
      ;; (org-set-sorting-strategy 'todo)

      (setq org-agenda-redo-command '(nd/org-conflicts))

	  (insert "Conflicting Headings: \n")
	  (add-text-properties (point-min) (1- (point))
			               (list 'face 'org-agenda-structure
				                 'short-heading "Conflicts"))
	  (org-agenda-mark-header-line (point-min))

      (setq rtnall (nd/build-conflict-list))
      (when rtnall
        (mapc
         (lambda (c)
           (insert (concat "Between " (mapconcat (lambda (ts) (nth 3 ts)) c " and ") "\n"))
           (insert (concat (mapconcat (lambda (ts) (nd/get-conflict-header-text (nth 2 ts))) c "\n") "\n"))
           (insert "\n"))
         rtnall))

      ;; clean up and finalize
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties
       (point-min) (point-max)
	   `(org-agenda-type agenda
					     org-last-args ,arg
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

(setq org-agenda-files '("~/Org"
                        "~/Org/projects"
                        "~/Org/reference/peripheral.org"))

(setq org-agenda-sticky t)

(add-hook 'org-finalize-agenda-hook
          (lambda () (setq org-agenda-tags-column (- 4 (window-width)))
            (org-agenda-align-tags)))

(setq org-agenda-prefix-format
      '((agenda . "  %-12:c %-5:e %?-12t% s")
        (todo . "  %-12:c")
        (tags . "  %-12:c %-5:e ")
        (search . "  %-12:c")))

(setq org-agenda-dim-blocked-tasks nil
      org-agenda-compact-blocks t
      org-agenda-window-setup 'current-window
      org-agenda-start-on-weekday 0
      org-agenda-span 'day
      org-agenda-current-time-string "### -- NOW -- ###")

(setq org-habit-graph-column 50)

(defun nd/org-agenda-filter-non-context ()
  "Filter all tasks with context tags."
  (interactive)
  (let* ((tags-list (mapcar #'car org-tag-alist))
         (context-tags (append
                        (nd/filter-list-prefix "@" tags-list)
                        (nd/filter-list-prefix "#" tags-list))))
    (setq org-agenda-tag-filter
          (mapcar (lambda (tag) (concat "-" tag)) context-tags))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag)))

(defun nd/org-agenda-filter-non-peripheral ()
  "Filter all tasks that don't have peripheral tags."
  (interactive)
  (let* ((peripheral-tags '("PERIPHERAL")))
    (setq org-agenda-tag-filter
          (mapcar (lambda (tag) (concat "-" tag)) peripheral-tags))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag)))
    
(defun nd/org-agenda-filter-non-effort ()
  "Filter agenda by non-effort tasks."
  (interactive)
  (setq org-agenda-hasprop-filter '("-Effort"))
  (org-agenda-filter-apply org-agenda-hasprop-filter 'hasprop))

(defun nd/org-agenda-filter-delegate ()
  "Filter agenda by tasks with an external delegate."
  (interactive)
  (setq org-agenda-hasprop-filter '("+DELEGATE"))
  (org-agenda-filter-apply org-agenda-hasprop-filter 'hasprop))

;; initialize new filters
(defvar org-agenda-hasprop-filter nil)

(defun nd/org-agenda-filter-make-matcher-prop
    (filter type &rest args)
  "Return matching matcher form for FILTER and TYPE where TYPE is not
in the regular `org-agenda-filter-make-matcher' function. This is
intended to be uses as :before-until advice and will return nil if
the type is not valid (which is currently 'prop')"
  (let (f f1)
    ;; has property
    (cond
     ((eq type 'hasprop)
      (dolist (x filter)
        (push (nd/org-agenda-filter-make-matcher-hasprop-exp x) f))))
    (if f (cons 'and (nreverse f)))))

(defun nd/org-agenda-filter-make-matcher-hasprop-exp (h)
 "Returns form to test the presence or absence of properties H.
H is a string like +prop or -prop"
 (let (op)
   (let* ((op (string-to-char h))
          (h (substring h 1))
          (f `(save-excursion
                (let ((m (org-get-at-bol 'org-hd-marker)))
                  (with-current-buffer
                      (marker-buffer m)
                    (goto-char m)
                    (org-entry-get nil ,h))))))
     (if (eq op ?-) (list 'not f) f))))

(defun nd/org-agenda-filter-show-all-hasprop nil
  (org-agenda-remove-filter 'hasprop))

(advice-add #'org-agenda-filter-make-matcher :before-until
            #'nd/org-agenda-filter-make-matcher-prop)

(advice-add #'org-agenda-filter-remove-all :before
            (lambda () (when org-agenda-hasprop-filter
                    (nd/org-agenda-filter-show-all-hasprop))))

(setq org-agenda-bulk-custom-functions
      '((?D nd/org-agenda-delete-subtree)))

(setq holiday-bahai-holidays nil
      holiday-hebrew-holidays nil
      holiday-oriental-holidays nil
      holiday-islamic-holidays nil)

(setq calendar-holidays (append holiday-general-holidays
                                holiday-christian-holidays))

(defconst nd/iter-future-time (* 7 24 60 60)
  "Iterators must have at least one task greater into the future to be active.")

(defconst nd/iter-statuscodes '(:uninit :empty :active)
  "Iterators can have these statuscodes.")
 
(defconst nd/peri-future-time nd/iter-future-time
  "Periodicals must have at least one heading greater into the future to be fresh.")

(defconst nd/peri-statuscodes '(:uninit :stale :fresh))

(defconst nd/project-invalid-todostates
  '("WAIT" "NEXT")
  "Projects cannot have these todostates.")
  
(defvar nd/agenda-limit-project-toplevel t
  "If true, filter projects by all levels or top level only.")

(defvar nd/agenda-hide-incubator-tags t
  "If true, don't show incubator headings.")
  
(defconst nd/org-agenda-todo-sort-order
  '("NEXT" "WAIT" "HOLD" "TODO")
  "Defines the order in which todo keywords should be sorted.")
  
(defconst nd/project-skip-todostates
  '("HOLD" "CANC")
  "These keywords override all contents within their subtrees.
Currently used to tell skip functions when they can hop over
entire subtrees to save time and ignore tasks")

(defun nd/get-date-property (timestamp-property)
  "Get TIMESTAMP-PROPERTY on current heading and convert to a number.
If it does not have a date, it will return nil."
  (let ((ts (org-entry-get nil timestamp-property)))
        (when ts (org-2ft ts))))

(defun nd/heading-compare-timestamp (timestamp-fun
                                     &optional ref-time future)
  "Returns the timestamp (from TIMESTAMP-FUM on the current heading) 
if timestamp is futher back in time compared to a REF-TIME (default to 
0 which is now, where negative is past and positive is future). If the 
FUTURE flag is t, returns timestamp if it is in the future compared 
to REF-TIME. Returns nil if no timestamp is found."
  (let* ((timestamp (funcall timestamp-fun))
        (ref-time (or ref-time 0)))
    (if (and timestamp
             (if future
                 (> (- timestamp (float-time)) ref-time)
               (<= (- timestamp (float-time)) ref-time)))
        timestamp)))

(defun nd/is-ia-timestamped-heading-p ()
  "Get active timestamp of current heading."
  (nd/get-date-property "TIMESTAMP_IA"))

(defun nd/is-timestamped-heading-p ()
  "Get active timestamp of current heading."
  (nd/get-date-property "TIMESTAMP"))

(defun nd/is-scheduled-heading-p ()
  "Get scheduled timestamp of current heading."
  (nd/get-date-property "SCHEDULED"))

(defun nd/is-deadlined-heading-p ()
  "Get deadline timestamp of current heading."
  (nd/get-date-property "DEADLINE"))

(defun nd/is-closed-heading-p ()
  "Get closed timestamp of current heading."
  (nd/get-date-property "CLOSED"))

(defun nd/is-stale-heading-p (&optional ts-prop)
  "Return timestamp for TS-PROP (TIMESTAMP by default) if current heading is stale."
  (nd/heading-compare-timestamp
   (lambda () (let ((ts (org-entry-get nil (or ts-prop "TIMESTAMP"))))
           (when (and ts (not (find ?+ ts))) (org-2ft ts))))))

(defun nd/is-fresh-heading-p ()
  "Return timestamp if current heading is fresh."
  (nd/heading-compare-timestamp 'nd/is-timestamped-heading-p nil t))

(defvar nd/archive-delay-days 30
  "The number of days to wait before tasks show up in the archive view.")

(defun nd/is-archivable-heading-p ()
  "Return timestamp if current heading is archivable."
  (nd/heading-compare-timestamp
   'nd/is-closed-heading-p
    (- (* 60 60 24 nd/archive-delay-days))))

(defun nd/is-todoitem-p ()
  "Return todo keyword if heading has one."
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

(defun nd/is-project-p ()
  "Return todo keyword if heading has todoitem children."
  (and (nd/heading-has-children 'nd/is-todoitem-p) (nd/is-todoitem-p)))

(defun nd/is-task-p ()
  "Return todo keyword if heading has todoitem children."
  (and (not (nd/heading-has-children 'nd/is-todoitem-p)) (nd/is-todoitem-p)))

(defun nd/is-project-task-p ()
  "Return todo keyword if heading has todoitem parents."
  (and (nd/heading-has-parent 'nd/is-todoitem-p) (nd/is-task-p)))

(defun nd/is-atomic-task-p ()
  "Return todo keyword if heading has no todoitem parents or children."
  (and (not (nd/heading-has-parent 'nd/is-todoitem-p)) (nd/is-task-p)))

(defun nd/is-periodical-heading-p ()
  "Return t if heading is a periodical."
  (equal "periodical" (org-entry-get nil "PARENT_TYPE" t)))

(defun nd/is-iterator-heading-p ()
  "Return t if heading is an iterator."
  (equal "iterator" (org-entry-get nil "PARENT_TYPE" t)))

(defun nd/heading-has-effort-p ()
  "Return t if heading has an effort."
  (org-entry-get nil "Effort"))

(defun nd/heading-has-context-p ()
  "Return t if heading has a context."
  (let ((tags (org-get-tags-at)))
    (or (> (length (nd/filter-list-prefix "#" tags)) 0)
        (> (length (nd/filter-list-prefix "@" tags)) 0))))

(defun nd/heading-has-tag-p (tag)
  "Return t if heading has tag TAG."
  (member tag (org-get-tags-at)))

(defun nd/heading-has-children (heading-test)
  "Return t if heading has a child for whom HEADING-TEST is t."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        has-children previous-point)
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not has-children)
                  (< previous-point (point) subtree-end))
        (when (funcall heading-test)
          (setq has-children t))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    has-children))

(defun nd/heading-has-parent (heading-test)
  "Return t if heading has parent for whom HEADING-TEST is t."
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

(defun nd/has-discontinuous-parent ()
  "Return t if heading has a non-todoitem parent which in turn has a todoitem parent."
  (let ((has-todoitem-parent)
        (has-non-todoitem-parent))
    (save-excursion
      (while (and (org-up-heading-safe)
                  (not has-todoitem-parent))
        (if (nd/is-todoitem-p)
            (setq has-todoitem-parent t)
          (setq has-non-todoitem-parent t))))
    (and has-todoitem-parent has-non-todoitem-parent)))

(defmacro nd/compare-statuscodes (op sc1 sc2 sc-list)
  "Compare position of statuscodes SC1 and SC2 in SC-LIST using operator OP."
  `(,op (position ,sc1 ,sc-list) (position ,sc2 ,sc-list)))

(defun nd/descend-into-project (allowed-statuscodes trans-tbl get-task-status)
  "Loop through (sub)project and return overall statuscode.

The returned statuscode is chosen from list ALLOWED-STATUSCODES where 
later entries in the list trump earlier ones. 

When a subproject is encountered, this function will obtain the 
statuscode of that project and use TRANS-TBL to translate the 
subproject statuscode to one in ALLOWED-STATUSCODES (if not found an 
error will be raised). TRANS-TBL is given as an alist of two-member 
cons cells where the first member is the subproject statuscode and the
 second is the index in ALLOWED-STATUSCODES to which the subproject 
statuscode will be translated.

When a task is encountered, function GET-TASK-STATUS will be applied to
obtain a statuscode-equivalent of the status of the tasks."
  ;; define "breaker-status" as the last of the allowed-statuscodes
  ;; when this is encountered the loop is broken because we are done
  ;; (the last entry trumps all others)
  (let ((project-status (first allowed-statuscodes))
        (breaker-status (car (last allowed-statuscodes)))
        (previous-point))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      ;; loop through subproject tasks until breaker-status found
      (while (and (not (eq project-status breaker-status))
                  (> (point) previous-point))
        (let ((keyword (nd/is-todoitem-p)))
          (if keyword
              (let ((new-status
                     ;; if project then descend recursively
                     (if (nd/heading-has-children 'nd/is-todoitem-p)
                         (let ((n (nd/get-project-status)))
                           ;; if project returns an allowed status
                           ;; then use that
                           (or (and (member n allowed-statuscodes) n)
                               ;; otherwise look up the value in the
                               ;; translation table and return error
                               ;; if not found
                               (nth (or (alist-get n trans-tbl)
                                        (error (concat "status not found: " n)))
                                    allowed-statuscodes)))
                       ;; if tasks then use get-task-status to obtain status
                       (nth (funcall get-task-status keyword)
                            allowed-statuscodes))))
                (if (nd/compare-statuscodes > new-status project-status allowed-statuscodes)
                    (setq project-status new-status)))))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    project-status))

(defun nd/get-project-status ()
  "Return project heading statuscode (assumes it is indeed a project)."
  (let ((keyword (nd/is-todoitem-p)))
    ;;
    ;; these first three are easy because they only require
    ;; testing the project headline and nothing underneath
    ;;
    (cond
     ;; it does not make sense for projects to be scheduled
     ((nd/is-scheduled-heading-p) :scheduled-project)

     ;; held projects do not care what is underneath them
     ((equal keyword "HOLD") :held)

     ;; projects with invalid todostates are nonsense
     ((member keyword nd/project-invalid-todostates)
      :invalid-todostate)
     
     ;;
     ;; these require descending into the project subtasks
     ;;

     ;; canceled projects can either be archivable or complete
     ;; any errors or undone tasks are irrelevant
     ((equal keyword "CANC")
      (nd/descend-into-project
       '(:archivable :complete)
       '((:stuck . 1)
         (:held . 1)
         (:waiting . 1)
         (:active . 1)
         (:scheduled-project . 1)
         (:invalid-todostate . 1)
         (:undone-complete . 1)
         (:done-incomplete . 1))
       (lambda (k)
         (if (and (member k org-done-keywords)
                  (nd/is-archivable-heading-p)) 0 1))))
     
     ;; done projects are like canceled projects but can also be incomplete
     ((equal keyword "DONE")
      (nd/descend-into-project
       '(:archivable :complete :done-incomplete)
       '((:stuck . 2)
         (:held . 2)
         (:waiting . 2)
         (:active . 2)
         (:scheduled-project . 2)
         (:invalid-todostate . 2)
         (:undone-complete . 2))
       (lambda (k)
         (if (member k org-done-keywords)
             (if (nd/is-archivable-heading-p) 0 1)
           2))))
     
     ;; project with TODO states could be basically any status
     ((equal keyword "TODO")
      (nd/descend-into-project
       '(:undone-complete :stuck :held :waiting :active)
       '((:complete . 0)
         (:archivable . 0)
         (:scheduled-project . 1)
         (:invalid-todostate . 1)
         (:done-incomplete . 1))
       (lambda (k)
         (cond ((equal k "TODO") (if (nd/is-scheduled-heading-p) 4 1))
               ((equal k "HOLD") 2)
               ((equal k "WAIT") 3)
               ((equal k "NEXT") 4)
               (t 0)))))
     
     (t (error (concat "invalid keyword detected: " keyword))))))

(defun nd/get-iterator-status ()
  "Get the status of an iterator where allowed statuscodes are in list
 `nd/get-iter-statuscodes.' where latter codes in the list trump 
earlier ones."
  (let ((iter-status (first nd/iter-statuscodes))
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not (eq iter-status :active))
                  (< (point) subtree-end))
        (let ((keyword (nd/is-atomic-task-p))
              (new-status))
          (if keyword
              (progn
                (setq new-status (if (nd/heading-compare-timestamp
                                      (lambda ()
                                        (or (nd/is-scheduled-heading-p)
                                            (nd/is-deadlined-heading-p)))
                                        nd/iter-future-time t)
                                     :active
                                   :empty))
                (if (nd/compare-statuscodes > new-status iter-status nd/iter-statuscodes)
                    (setq iter-status new-status)))))
        (outline-next-heading)))
    iter-status))
    
(defun nd/get-periodical-status ()
  "Get the status of a periodical where allowed statuscodes are in list
 `nd/get-peri-statuscodes.' where latter codes in the list trump 
earlier ones."
  (let ((peri-status :uninit)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not (eq peri-status :fresh))
                  (< (point) subtree-end))
        (if (and (nd/is-periodical-heading-p)
                 (not (nd/heading-has-children 'nd/is-periodical-heading-p)))
            (let ((new-status
                   (if (nd/heading-compare-timestamp
                        'nd/is-timestamped-heading-p
                        nd/iter-future-time t)
                       :fresh
                     :stale)))
              (if (nd/compare-statuscodes > new-status peri-status nd/peri-statuscodes)
                  (setq peri-status new-status))))
        (outline-next-heading)))
    peri-status))

(defun nd/skip-heading ()
  "Skip forward to next heading."
  (save-excursion (or (outline-next-heading) (point-max))))

(defun nd/skip-subtree ()
  "Skip forward to next subtree."
  (save-excursion (or (org-end-of-subtree t) (point-max))))


(defmacro nd/skip-heading-without (heading-fun test-fun)
  "Skip headings accoring to certain characteristics. 

HEADING-FUN is a function that tests the heading and returns the 
todoitem keyword on success. TEST-FUN is a function that further tests 
the identity of the heading and may or may not use the keyword output 
supplied by the HEADING-FUN. This function will not skip if 
HEADING-FUN and TEST-FUN return true"
  `(save-restriction
     (widen)
     (let ((keyword (,heading-fun)))
       ;; (message keyword)
       (if (not (and keyword ,test-fun))
           (nd/skip-heading)))))

(defun nd/skip-headings-with-tags (pos-tags-list &optional neg-tags-list)
  "Skip headings that have tags in POS-TAGS-LIST and not in NEG-TAGS-LIST."
  (save-restriction
    (widen)
    (let ((heading-tags (org-get-tags-at)))
      (if (and (or (not pos-tags-list)
                   (intersection pos-tags-list heading-tags :test 'equal))
               (not (intersection neg-tags-list heading-tags :test 'equal)))
          (nd/skip-heading)))))

(defun nd/skip-non-stale-headings ()
  "Skip headings that do not have stale timestamps and are not part of projects."
  (save-restriction
    (widen)
    (let ((keyword (nd/is-todoitem-p)))
      (if (not
           (and (nd/is-stale-heading-p)
                (not (member keyword org-done-keywords))
                (not (nd/heading-has-children 'nd/is-todoitem-p))
                (not (nd/heading-has-parent 'nd/is-todoitem-p))))
          (nd/skip-heading)))))

(defun nd/skip-non-ia-timestamped-tasks ()
  "Skip tasks that do not have an inactive timestamp."
  (save-excursion
    (widen)
    (if (not (and (nd/is-task-p)
                  (not (nd/is-ia-timestamped-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-atomic-tasks ()
  "Skip headings that are not atomic tasks."
  (save-excursion
    (widen)
    (if (not (nd/is-atomic-task-p))
        (nd/skip-heading))))

(defun nd/skip-non-closed-atomic-tasks ()
  "Skip headings that are not complete (but not archivable) atomic tasks."
  (nd/skip-heading-without
   nd/is-atomic-task-p
   (and (member keyword org-done-keywords)
        (not (nd/is-archivable-heading-p)))))

(defun nd/skip-non-archivable-atomic-tasks ()
  "Skip headings that are not archivable atomic tasks."
  (nd/skip-heading-without
   nd/is-atomic-task-p
   (nd/is-archivable-heading-p)))

(defun nd/skip-non-iterator-parent-headings ()
  "Skip headings that are not toplevel iterator headings."
  (save-restriction
    (widen)
    (if (not (and (nd/is-iterator-heading-p)
                  (not (nd/heading-has-parent 'nd/is-iterator-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-iterator-unscheduled ()
  "Skip all headings that are not unscheduled iterator children."
  (nd/skip-heading-without
   nd/is-atomic-task-p
   (not (or (nd/is-scheduled-heading-p)
            (nd/is-deadlined-heading-p)))))
            
(defun nd/skip-non-periodical-parent-headings ()
  "Skip headings that are not toplevel periodical headings."
  (save-restriction
    (widen)
    (if (not (and (nd/is-periodical-heading-p)
                  (not (nd/heading-has-parent 'nd/is-periodical-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-periodical-untimestamped ()
  "Skip all headings that are not periodical children without a timestamp."
  (save-restriction
    (widen)
    (if (not (and (nd/is-periodical-heading-p)
                  (not (nd/is-timestamped-heading-p))
                  (not (nd/heading-has-children 'nd/is-periodical-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-project-tasks ()
  "Skip headings that are not project tasks."
  (save-restriction
    (widen)
    (let ((keyword (nd/is-todoitem-p)))
      (if keyword
          (if (nd/heading-has-children 'nd/is-todoitem-p)
              (if (member keyword nd/project-skip-todostates)
                  (nd/skip-subtree)
                (nd/skip-heading))
            (if (not (nd/heading-has-parent 'nd/is-todoitem-p))
                (nd/skip-heading)))
        (nd/skip-heading)))))

(defun nd/skip-non-discontinuous-project-tasks ()
  "Skip headings that are not discontinuous within projects."
  (nd/skip-heading-without
   nd/is-todoitem-p
   (nd/has-discontinuous-parent)))

(defun nd/skip-non-done-unclosed-todoitems ()
  "Skip headings that are not completed without a closed timestamp."
  (nd/skip-heading-without
   nd/is-todoitem-p
   (and (member keyword org-done-keywords)
        (not (nd/is-closed-heading-p)))))

(defun nd/skip-non-undone-closed-todoitems ()
  "Skip headings that are not incomplete with a closed timestamp."
  (nd/skip-heading-without
   nd/is-todoitem-p
   (and (not (member keyword org-done-keywords))
        (nd/is-closed-heading-p))))

(defun nd/skip-non-projects (&optional ignore-toplevel)
  "Skip headings that are not projects (toplevel-only if IGNORE-TOPLEVEL is t)."
  (save-restriction
    (widen)
    (let ((keyword (nd/is-project-p)))
      (if keyword
          (if (and nd/agenda-limit-project-toplevel
                   (not ignore-toplevel)
                   (nd/heading-has-parent 'nd/is-todoitem-p))
              (nd/skip-subtree))
        (nd/skip-heading)))))

(defun nd/org-agenda-filter-status (filter status-fun a-line
                                           &optional filter-only)
  "Filter for `org-agenda-before-sorting-filter-function' intended for
agenda project views (eg makes the assumption that all entries are
from projects in the original org buffer) wherein this function will
filter project headings based on their statuscodes.

It works by going to the original org buffer and determining the 
project status using STATUS-FUN, after which it will check if 
status is in FILTER (a list of statuscodes). If true, the flag string 
in the prefix is replaced with the status, and the status is set as a 
text property for further sorting.

If option FILTER-ONLY is t, function only return the unmodified a-line 
or nil to act as a filter (eg does not touch text properties)."
  (let* ((m (get-text-property 1 'org-marker a-line))
         (s (with-current-buffer (marker-buffer m)
              (goto-char m)
              (funcall status-fun))))
    (if (member s filter)
        (if filter-only
            a-line
          (org-add-props (replace-regexp-in-string
                          "xxxx" (symbol-name s) a-line)
              nil 'project-status s)))))

(defun nd/org-agenda-sort-prop (prop order a b)
  "Sort a block agenda view by text property PROP given a list ORDER
of said text properties in the desired order and lines A and B as 
inputs. To be used with `org-agenda-cmp-user-defined'."
  (let* ((ta (get-text-property 1 prop a))
         (tb (get-text-property 1 prop b))
         (pa (position ta order :test (if (stringp ta) #'equal)))
         (pb (position tb order :test (if (stringp tb) #'equal))))
    (cond ((or (null pa) (null pb)) nil)
          ((< pa pb) +1)
          ((> pa pb) -1))))

(defun nd/agenda-base-heading-cmd (match header skip-fun)
  "Make a tags agenda view that matches tags in string MATCH with
header given as string HEADER and with skip function SKIP-FUN."
  `(tags
    ,match
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-sorting-strategy '(category-keep)))))

(defun nd/agenda-base-task-cmd (match header skip-fun &optional sort)
  "Make a tags-todo agenda view that matches tags in string MATCH with
header given as string HEADER and with skip function SKIP-FUN. Also
takes a sorting structure SORT which is passed to 
`org-agenda-sorting-strategy'"
  (or sort (setq sort ''(category-keep)))
  `(tags-todo
    ,match
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-todo-ignore-with-date t)
     (org-agenda-sorting-strategy ,sort))))

(defun nd/agenda-base-project-cmd (match header skip-fun kw-list status-fun
                                         &optional todo status-px)
  "Make a tags-todo agenda view that matches tags in string MATCH with
header given as string HEADER and with skip function SKIP-FUN. KW-LIST
is a list of keywords to be used in filtering and sorting (the order
in the list defines the sort order). STATUS-FUN is a function used to
get the statuscode of the current line in the agenda. Optional arg
TODO determines if this is a tags-todo (t) or tags (nil) block, and
STATUS-PX as t enables the statuscode to be formatted into the prefix
string."
  `(,(if 'tags-todo 'tags)
    ,match
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-before-sorting-filter-function
      (lambda (l) (nd/org-agenda-filter-status ,kw-list ,status-fun l)))
     (org-agenda-cmp-user-defined
      (lambda (a b) (nd/org-agenda-sort-prop 'project-status ,kw-list a b)))
     (org-agenda-prefix-format '((tags . ,(if status-px
                                             "  %-12:c %(format \"xxxx: \")"
                                           "  %-12:c       "))))
     (org-agenda-sorting-strategy '(user-defined-down category-keep)))))

(defun nd/toggle-project-toplevel-display ()
  "Toggle all project headings and toplevel only headings in project blocks."
  (interactive)
  (setq nd/agenda-limit-project-toplevel (not nd/agenda-limit-project-toplevel))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "Showing %s project view in agenda"
           (if nd/agenda-limit-project-toplevel "toplevel" "complete")))

(defun nd/org-tags-view-advice (orig-fn &optional todo-only match)
  "Advice to include done states in `org-tags-view' for tags-todo agenda types."
  (nd/with-advice
      ((#'org-make-tags-matcher
        :around (lambda (f m)
                  (let ((org--matcher-tags-todo-only nil))
                    (funcall f m)))))
    (funcall orig-fn todo-only match)))

(advice-add #'org-tags-view :around #'nd/org-tags-view-advice)

(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-cmp-user-defined
      '(lambda (a b)
         (let ((pa (- (length (member
                               (get-text-property 1 'todo-state a)
                               nd/org-agenda-todo-sort-order))))
               (pb (- (length (member
                               (get-text-property 1 'todo-state b)
                               nd/org-agenda-todo-sort-order)))))
           (cond ((or (null pa) (null pb)) nil)
                 ((> pa pb) +1)
                 ((< pa pb) -1)))))

(let* ((actionable "-NA-REFILE-%inc")
       (periodical "PARENT_TYPE=\"periodical\"")
       (iterator "PARENT_TYPE=\"iterator\"")
       (habit "STYLE=\"habit\"")
       (task-match (concat actionable "-" periodical "-" habit "/!"))
       (act-no-rep-match (concat actionable "-" periodical "-" iterator "-" habit "/!"))
       (peri-match (concat actionable "+" periodical "-" iterator "-" habit))
       (iter-match (concat actionable "-" periodical "+" iterator "-" habit "/!")))

  (setq
   org-agenda-custom-commands
   `(("a"
      "Calendar View"
      ((agenda "" ((org-agenda-skip-function '(nd/skip-headings-with-tags '("%inc" "REFILE")))
                        (org-agenda-include-diary t)))))

     ("t"
      "Task View"
      (,(nd/agenda-base-task-cmd act-no-rep-match
                                 "Project Tasks"
                                 ''nd/skip-non-project-tasks
                                 ''(user-defined-up category-keep))
       ,(nd/agenda-base-task-cmd act-no-rep-match "Atomic Tasks" ''nd/skip-non-atomic-tasks)))

     ("p"
      "Project View"
      (,(nd/agenda-base-project-cmd
         act-no-rep-match
         '(concat (and nd/agenda-limit-project-toplevel "Toplevel ") "Projects")
         ''nd/skip-non-projects
         ''(:scheduled-project :invalid-todostate :undone-complete :done-incomplete
                               :stuck :waiting :held :active)
         ''nd/get-project-status t t)))
          
     ("i"
      "Incubator View"
      ((agenda "" ((org-agenda-skip-function '(nd/skip-headings-with-tags nil '("%inc")))
                   (org-agenda-span 7)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:deadline :timestamp :scheduled))))
       ,(nd/agenda-base-heading-cmd "-NA-REFILE+%inc"
                                   "Stale Incubated Timestamps"
                                   ''nd/skip-non-stale-headings)
       ,(nd/agenda-base-task-cmd "-NA-REFILE+%inc/!"
                                 "Incubated Tasks"
                                 ''nd/skip-non-atomic-tasks)
       ,(nd/agenda-base-project-cmd
         "-NA-REFILE+%inc/!"
         '(concat (and nd/agenda-limit-project-toplevel "Toplevel ") "Incubated Projects")
         ''nd/skip-non-projects
         ''(:scheduled-project :invalid-todostate :undone-complete :done-incomplete
                               :stuck :waiting :held :active)
         ''nd/get-project-status
         t t)))
          
     ("P"
      "Periodical View"
      (,(nd/agenda-base-project-cmd
         (concat actionable "-" iterator "+" periodical "-" habit)
         "Periodical Status"
         ''nd/skip-non-periodical-parent-headings
         'nd/peri-statuscodes ''nd/get-periodical-status nil t)
       ,(nd/agenda-base-heading-cmd "-NA-REFILE+PARENT_TYPE=\"periodical\""
                                   "Untimestamped"
                                   ''nd/skip-non-periodical-untimestamped)))

     ("I"
      "Iterator View"
      (,(nd/agenda-base-project-cmd
         "-NA-REFILE+PARENT_TYPE=\"iterator\""
         "Iterator Status"
         ''nd/skip-non-iterator-parent-headings
         'nd/iter-statuscodes ''nd/get-iterator-status nil t)
       ,(nd/agenda-base-task-cmd "-NA-REFILE+PARENT_TYPE=\"iterator\"/!"
                                 "Unscheduled or Undeaded"
                                 ''nd/skip-non-iterator-unscheduled)))

     ("r" "Refile" ((tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile"))
                          (org-tags-match-list-sublevels nil))))
     
     ("f" "Flagged" ((tags "%flag" ((org-agenda-overriding-header "Flagged Tasks")))))
     
     ("e"
      "Critical Errors"
      (,(nd/agenda-base-task-cmd task-match
                                 "Discontinous Project"
                                 ''nd/skip-non-discontinuous-project-tasks)
       ,(nd/agenda-base-heading-cmd task-match
                                    "Undone Closed"
                                    ''nd/skip-non-undone-closed-todoitems)
       ,(nd/agenda-base-heading-cmd (concat actionable "-" periodical)
                                    "Done Unclosed"
                                    ''nd/skip-non-done-unclosed-todoitems)
       ,(nd/agenda-base-task-cmd (concat task-match)
                                 "Missing Creation Timestamp"
                                 ''nd/skip-non-ia-timestamped-tasks)))
       

     ("A"
      "Archivable Tasks and Projects"
      ((tags-todo ,(concat actionable "-" periodical "-" habit "/DONE|CANC")
                  ((org-agenda-overriding-header "Archivable Atomic Tasks and Iterators")
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-agenda-skip-function 'nd/skip-non-archivable-atomic-tasks)))
       ,(nd/agenda-base-heading-cmd (concat actionable "-" habit)
                                   "Stale Tasks and Periodicals"
                                   ''nd/skip-non-stale-headings)
       ,(nd/agenda-base-project-cmd
         (concat actionable "-" periodical "-" iterator "-" habit)
         '(concat (and nd/agenda-limit-project-toplevel "Toplevel ") "Archivable Projects")
         ''nd/skip-non-projects ''(:archivable) ''nd/get-project-status))))))

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
