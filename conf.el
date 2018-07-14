(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-default 'truncate-lines t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq pop-up-windows nil) ; no popups (eg ediff)

;; (global-linum-mode t)
(line-number-mode 1)
(column-number-mode 1)

(setq-default tab-width 4)

(setq scroll-conservatively 100)

(add-hook 'ess-mode-hook #'prettify-symbols-mode)
(add-hook 'inferior-ess-mode-hook #'prettify-symbols-mode)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

(defalias 'yes-or-no-p 'y-or-n-p) ; eliminate yes or no prompt on killing procs

;; for spacemacs theme
(setq spacemacs-theme-custom-colors '((lnum . "#64707c")))

(defvar my:theme 'spacemacs-dark)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)
(if (daemonp)
    (add-hook 'after-make-frame-functions(lambda (frame)
                                           (select-frame frame)
                                           (if (window-system frame)
                                               (unless my:theme-window-loaded
                                                 (if my:theme-terminal-loaded
                                                     (enable-theme my:theme)
                                                   (load-theme my:theme t))
                                                 (setq my:theme-window-loaded t))
                                             (unless my:theme-terminal-loaded
                                               (if my:theme-window-loaded
                                                   (enable-theme my:theme)
                                                 (load-theme my:theme t))
                                               (setq my:theme-terminal-loaded t)))))
  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t))))

(use-package spaceline
  :ensure t
  :config
    (require 'spaceline-config)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme)
    (setq spaceline-buffer-size-p nil))

;;  (use-package dashboard
;;    :ensure t
;;    :config
;;    (dashboard-setup-startup-hook)
;;     (setq dashboard-banner-logo-title "Emacs"))
     ;; (setq dashboard-items '((recents . 10))))

(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "<f1>") 'org-agenda)
(global-set-key (kbd "<f2>") 'org-capture)
(global-set-key (kbd "<f12>") 'global-hl-line-mode)
(global-set-key (kbd "S-<f12>") 'display-line-numbers-mode)
(global-set-key (kbd "C-<f12>") 'mu4e)

(use-package evil
  :ensure t
  :init
  ;; this is required to make evil collection work
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init '(which-key helm minibuffer mu4e)))

(use-package delight
  :ensure t)

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
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  ("C-c h" . 'helm-command-prefix)
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
		helm-scroll-amount 8))

(helm-autoresize-mode 1)
(require 'helm-config)

;; (use-package ido
;;   :ensure t
;;   :bind
;;   ("C-x C-b" . 'ido-switch-buffer)
;;   ("C-x b" . 'ibuffer)
;;   :config
;;   (ido-mode 1)
;;   (setq ido-everywhere t)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-max-directory-size 100000)
;;   (setq ido-default-file-method 'selected-window)
;;   (setq ido-default-buffer-method 'selected-window)
;;   (use-package ido-vertical-mode
;;     :ensure t
;;     :init
;;     (ido-vertical-mode 1)
;;     (setq ido-vertical-define-keys 'C-n-and-C-p-only)))


  ;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; (use-package smex
;;   :ensure t
;;   :init
;;   (smex-initialize)
;;   :bind
;;   ("M-x" . 'smex)
;;   ("M-X" . 'smex-major-mode-commands))

(use-package rainbow-delimiters
  :ensure t
  :delight
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'inferior-ess-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'ess-mode-hook #'rainbow-delimiters-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-background t)
  (set-face-attribute 'aw-leading-char-face nil :foreground "#292b2e"
					  :background "#bc6ec5":height 1.0 :box nil))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char)
  :config (setq avy-background t))

(use-package sudo-edit
  :ensure t
  :bind ("C-c s" . sudo-edit))

(use-package typit
  :init
  :ensure t)

(use-package calfw
  :init
  :ensure t)

(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))

(use-package fill-column-indicator
  :ensure t
  :init
  :config
  (setq fci-rule-use-dashes t)
  (add-hook 'prog-mode-hook #'fci-mode))

(use-package rainbow-mode
  :ensure t)

;; lovingly stolen from aaron harris
(defmacro nd/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun nd/filter-list-prefix (prefix str-list)
  "Return a subset of tags-list whose first character matches prefix.'
  tags-list defaults to org-tag-alist if not given"
  (seq-filter (lambda (i)
                (and (stringp i)
                     (string-prefix-p prefix i)))
              str-list))

(defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun config-visit ()
(interactive)
(find-file "~/.emacs.d/conf.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
"Reloads ~/.emacs.d/conf.org at runtime"
(interactive)
(org-babel-load-file (expand-file-name "~/.emacs.d/conf.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(global-set-key (kbd "C-S-w") 'fc/delete-whole-line)
(defun fc/delete-whole-line ()
"Delete the whole line without flooding the kill ring"
(interactive)
(delete-region (progn (forward-line 0) (point))
                (progn (forward-line 1) (point))))

(global-set-key (kbd "M-d") 'fc/delete-word-forward)
(defun fc/delete-word-forward (arg)
"Delete word forward without flooding the kill ring"
(interactive "p")
(delete-region (point) (progn (forward-word arg) (point))))

(global-set-key (kbd "<M-backspace>") 'fc/delete-word-backward)
(defun fc/delete-word-backward (arg)
"Delete word backward without flooding the kill ring"
(interactive "p")
(delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-c C-d") 'fc/duplicate-current-line-or-region)
(defun fc/duplicate-current-line-or-region (arg)
    "Duplicates the current line or region ARG times."
    (interactive "p")
    (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
        (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))))

(setq inferior-R-args "--quiet --no-save")
(load "ess-site")
(setq ess-history-file "session.Rhistory")
(setq ess-history-directory
	  (substitute-in-file-name "${XDG_CONFIG_HOME}/r/"))

(use-package haskell-mode
  :ensure t
  :config
  ;; enable dynamic linking by default
  (setq haskell-compile-command "ghc -dynamic -Wall -ferror-spans -fforce-recomp -c %s")
  (setq haskell-interactive-popup-errors nil))

(setq org-startup-indented t)
(delight 'org-indent-mode)
(setq org-directory "~/Org")
(run-at-time "00:59" 3600 'org-save-all-org-buffers)
(setq org-modules '(org-habit org-protocol))
(require 'org-protocol)

(use-package evil-org
  :ensure t
  :after evil
  :after org
  :delight
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-log-into-drawer "LOGBOOK")
(setq org-log-done t)

(use-package org-bullets
  :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(defun nd/org-ui-heading-same-font-height ()
  (let ((heading-height 1.15))
    (set-face-attribute 'org-level-1 nil :weight 'bold :height heading-height)
    (set-face-attribute 'org-level-2 nil :weight 'semi-bold :height heading-height)
    (set-face-attribute 'org-level-3 nil :weight 'normal :height heading-height)
    (set-face-attribute 'org-level-4 nil :weight 'normal :height heading-height)
    (set-face-attribute 'org-level-5 nil :weight 'normal :height heading-height)))

(add-hook 'org-mode-hook 'nd/org-ui-heading-same-font-height)

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

(setq org-src-window-setup 'current-window)
(setq org-src-fontify-natively t)
(setq org-edit-src-content-indentation 0)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x x") 'nd/mark-subtree-done)
            (local-set-key (kbd "C-c C-x c") 'nd/org-clone-subtree-with-time-shift)))

(evil-define-key 'motion org-agenda-mode-map
  "t" 'nd/toggle-project-toplevel-display
  "D" 'org-agenda-day-view
  "W" 'org-agenda-week-view
  "M" 'org-agenda-month-view
  "Y" 'org-agenda-year-view
  "ct" nil
  "sC" 'nd/org-agenda-filter-non-context
  "e" 'org-agenda-set-effort
  "ce" nil)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'org-agenda-set-tags)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "light coral" :weight bold)
        ("NEXT" :foreground "khaki" :weight bold)
        ("DONE" :foreground "light green" :weight bold)
        ("WAIT" :foreground "orange" :weight bold)
        ("HOLD" :foreground "violet" :weight bold)
        ("CANC" :foreground "deep sky blue" :weight bold)))

(defun nd/add-tag-face (fg-name prefix)
  "Adds list of cons cells to org-tag-faces with foreground set to fg-name.
  Start and end specify the positions in org-tag-alist which define the tags
  to which the faces are applied"
  (dolist (tag (nd/filter-list-prefix prefix (mapcar #'car org-tag-alist)))
    (push `(,tag . (:foreground ,fg-name)) org-tag-faces)))

;; for some reason, most special chars don't really
;; work in org-tag-alist, only #, @, %, and _
(setq org-tag-alist
      '((:startgroup)
        ("@errand" . ?e)
        ("@home" . ?h)
        ("@work" . ?w)
        ("@travel" . ?r)
        (:endgroup)
        
        ("#laptop" . ?l)
        ("#tcult" . ?t)
        ("#phone" . ?p)
        
        ("%note" . ?n)
        ("%inc" . ?i)
        ("%subdiv" . ?s)
        
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

(setq org-tag-faces '())

(nd/add-tag-face "PaleGreen" "@")
(nd/add-tag-face "SkyBlue" "#")
(nd/add-tag-face "PaleGoldenrod" "%")
(nd/add-tag-face "violet" "_")

(add-to-list 'org-default-properties "PARENT_TYPE")
(add-to-list 'org-default-properties "OWNER")
(add-to-list 'org-default-properties "GOAL")
(add-to-list 'org-default-properties "TIME_SHIFT")

(setq org-global-properties
      '(("PARENT_TYPE_ALL" . "periodical iterator")
        ("Effort_ALL" . "0:05 0:15 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00")))

;; TODO this may not be needed
(setq org-use-property-inheritance '("PARENT_TYPE" "TIME_SHIFT"))

(let ((capfile "~/Org/capture.org"))
  (setq org-capture-templates
        `(("t" "todo" entry (file ,capfile)
		   "* TODO %?\ndeliverable: \n%U\n")

          ("n" "note" entry (file ,capfile)
		   "* %? :\\%note:\n%U\n")

          ("a" "appointment" entry (file ,capfile)
		   "* %?\n%U\n%^t\n")

          ("m" "multi-day" entry (file ,capfile)
		   "* TODO %?\n%U\n%^t--%^t\n")

          ("d" "deadline" entry (file ,capfile)
		   "* TODO %?\nDEADLINE: %^t\ndeliverable:\n%U\n")

		  ("p" "org-protocol" entry (file ,capfile)
           "* %^{Title} :\\%note:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
		   :immediate-finish t)

		  ("L" "org-protocol link" entry (file ,capfile)
           "* %^{Title} :\\%note:\n[[%:link][%:description]]\n%U"
		   :immediate-finish t))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 ("~/Org/reference/idea.org" :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
;; (setq org-completion-use-ido t)

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-indirect-buffer-display 'current-window)

(defun nd/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'nd/verify-refile-target)

(setq org-habit-graph-column 50)



(setq org-agenda-files '("~/Org"
                      "~/Org/projects"
                      "~/Org/reference"))
;; (setq org-agenda-files '("~/Org/reference/agendatest.org"))
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-agenda-window-setup 'current-window)

(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

(defun nd/get-date-property (date-property)
  "Helper function to get the date property and convert to a number.
If it does not have a date, it will return nil."
  (let ((timestamp (org-entry-get nil date-property)))
    (if timestamp (float-time (date-to-time timestamp)))))

(defun nd/heading-compare-timestamp (timestamp-fun
									 &optional ref-time future)
  "helper function that returns the timestamp (returned by 
timestamp-fun on the current header) if timestamp is futher back in 
time compared to a ref-time (default to 0 which is now, where negative
is past an positive is future). If the future flag is set, returns 
timestamp if it is in the future compared to ref-time. Returns nil if 
no timestamp is found."
  (let* ((timestamp (funcall timestamp-fun))
        (ref-time (or ref-time 0)))
    (if (and timestamp
             (if future
                 (> (- timestamp (float-time)) ref-time)
               (<= (- timestamp (float-time)) ref-time)))
        timestamp)))

(defun nd/is-timestamped-heading-p ()
  (nd/get-date-property "TIMESTAMP"))

(defun nd/is-scheduled-heading-p ()
  (nd/get-date-property "SCHEDULED"))

(defun nd/is-deadlined-heading-p ()
  (nd/get-date-property "DEADLINE"))

(defun nd/is-closed-heading-p ()
  (nd/get-date-property "CLOSED"))

(defun nd/is-stale-heading-p ()
  (nd/heading-compare-timestamp
   (lambda () (let ((ts (org-entry-get nil "TIMESTAMP")))
		   (if (and ts (not (find ?+ ts)))
			   (float-time (date-to-time ts)))))))

(defun nd/is-fresh-heading-p ()
  (nd/heading-compare-timestamp 'nd/is-timestamped-heading-p nil t))

(defvar nd/archive-delay-days 30
  "the number of days to wait before tasks show up in the archive view")

(defun nd/is-archivable-heading-p ()
  (nd/heading-compare-timestamp
   'nd/is-closed-heading-p
    (- (* 60 60 24 nd/archive-delay-days))))

(defun nd/is-todoitem-p ()
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

(defun nd/is-project-p ()
  (and (nd/heading-has-children 'nd/is-todoitem-p) (nd/is-todoitem-p)))

(defun nd/is-task-p ()
  (and (not (nd/heading-has-children 'nd/is-todoitem-p)) (nd/is-todoitem-p)))

(defun nd/is-project-task-p ()
  (and (nd/heading-has-parent 'nd/is-todoitem-p) (nd/is-task-p)))

(defun nd/is-atomic-task-p ()
  (and (not (nd/heading-has-parent 'nd/is-todoitem-p)) (nd/is-task-p)))

(defun nd/is-periodical-heading-p ()
  (equal "periodical" (org-entry-get nil "PARENT_TYPE" t)))

(defun nd/is-iterator-heading-p ()
  (equal "iterator" (org-entry-get nil "PARENT_TYPE" t)))

(defun nd/heading-has-effort-p ()
  (org-entry-get nil "Effort"))

(defun nd/heading-has-context-p ()
  (let ((tags (org-get-tags-at)))
    (or (> (length (nd/filter-list-prefix "#" tags)) 0)
        (> (length (nd/filter-list-prefix "@" tags)) 0))))

(defun nd/heading-has-tag-p (tag)
  (member tag (org-get-tags-at)))

(defun nd/heading-has-children (heading-test)
  "returns t if heading has subheadings that return t when assessed with 
heading-test function"
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
  "returns parent keyword if heading is in the immediate subtree of a heading 
that evaluated to t with heading-test function"
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

(defun nd/has-discontinuous-parent ()
  "returns t if heading has a parent which is not a
todoitem which in turn has a parent which is a todoitem"
  (let ((has-todoitem-parent)
        (has-non-todoitem-parent))
    (save-excursion
      (while (and (org-up-heading-safe)
                  (not has-todoitem-parent))
        (if (nd/is-todoitem-p)
            (setq has-todoitem-parent t)
          (setq has-non-todoitem-parent t))))
    (and has-todoitem-parent has-non-todoitem-parent)))

(defconst nd/project-invalid-todostates
  '("WAIT" "NEXT")
  "projects cannot have these todostates") 

(defmacro nd/compare-statuscodes (op sc1 sc2 sc-list)
  `(,op (position ,sc1 ,sc-list) (position ,sc2 ,sc-list)))

(defun nd/decend-into-project (allowed-statuscodes trans-tbl get-task-status)
  (let ((project-status (first allowed-statuscodes))
		(breaker-status (car (last allowed-statuscodes)))
		(previous-point))
	;; (message "hi")
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
					   ;; if not project then use user-defined function
					   ;; to obtain status of task
					   (nth (funcall get-task-status keyword)
							allowed-statuscodes))))
				;; (message (format "%s" (concat "new status: " (symbol-name new-status))))
				;; (message (format "%s" (concat "project status: " (symbol-name project-status))))
				;; (message (format "%s" keyword))
				(if (nd/compare-statuscodes > new-status project-status allowed-statuscodes)
					(setq project-status new-status)))))
		(setq previous-point (point))
		(org-forward-heading-same-level 1 t)))
	project-status))

(defun nd/get-project-status ()
  (let ((keyword (nd/is-todoitem-p)))
	;; these first three are easy because they only require
	;; testing the project headline and nothing underneath
	(cond
	 ((nd/is-scheduled-heading-p) :scheduled-project)
	 ((equal keyword "HOLD") :held)
	 ((member keyword nd/project-invalid-todostates)
	  :invalid-todostate)
	 
	 ;; these require descending into the project subtasks
	 ((equal keyword "CANC")
	  (nd/decend-into-project
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
	 
	 ((equal keyword "DONE")
	  (nd/decend-into-project
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
	 
	 ((equal keyword "TODO")
	  (nd/decend-into-project
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

(defconst nd/iter-future-time (* 7 24 60 60))

(defconst nd/iter-statuscodes '(:uninit :empty :active))

(defun nd/get-iterator-status ()
  (let ((iter-status :uninit)
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

(defconst nd/peri-future-time nd/iter-future-time)

(defconst nd/peri-statuscodes '(:uninit :stale :fresh))

(defun nd/get-periodical-status ()
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
  (save-excursion (or (outline-next-heading) (point-max))))

(defun nd/skip-subtree ()
  (save-excursion (or (org-end-of-subtree t) (point-max))))

(defconst nd/project-skip-todostates
  '("HOLD" "CANC")
  "These keywords override all contents within their subtrees.
Currently used to tell skip functions when they can hop over
entire subtrees to save time and ignore tasks")

(defmacro nd/skip-heading-with (heading-fun test-fun)
  "Skips headings accoring to certain characteristics. heading-fun
is a function that tests the heading and returns the todoitem keyword
on success. Test-fun is a function that further tests the identity of
the heading and may or may not use the keyword output supplied by
the heading-fun. This function will not skip if heading-fun and 
test-fun return true"
  `(save-restriction
     (widen)
     (let ((keyword (,heading-fun)))
       (message keyword)
       (if (not (and keyword ,test-fun))
           (nd/skip-heading)))))

(defun nd/skip-headings-with-tags (pos-tags-list &optional neg-tags-list)
  "Skips headings that have tags in pos-tags-list and also skips
tags that do not have tags in neg-tags-list"
  (save-restriction
    (widen)
    (let ((header-tags (org-get-tags-at)))
      (if (and (or (not pos-tags-list)
                   (intersection pos-tags-list header-tags :test 'equal))
               (not (intersection neg-tags-list header-tags :test 'equal)))
          (nd/skip-heading)))))

(defun nd/skip-non-stale-headings ()
  (save-restriction
    (widen)
    (let ((keyword (nd/is-todoitem-p)))
      (if (not
           (and (nd/is-stale-heading-p)
                (not (member keyword org-done-keywords))
                (not (nd/heading-has-children 'nd/is-todoitem-p))
                (not (nd/heading-has-parent 'nd/is-todoitem-p))))
          (nd/skip-heading)))))

;; NOTE: this assumes that tags-todo will
;; filter out all done state tasks
(defun nd/skip-non-atomic-tasks ()
  (save-excursion
    (widen)
    (if (not (nd/is-atomic-task-p))
        (nd/skip-heading))))

(defun nd/skip-non-closed-atomic-tasks ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (and (member keyword org-done-keywords)
        (not (nd/is-archivable-heading-p)))))

(defun nd/skip-non-archivable-atomic-tasks ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (and (member keyword org-done-keywords)
        (nd/is-archivable-heading-p))))

(defun nd/skip-non-periodical-parent-headers ()
  (save-restriction
    (widen)
    (if (not (and (nd/is-periodical-heading-p)
                  (not (nd/heading-has-parent 'nd/is-periodical-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-periodical-untimestamped ()
  (save-restriction
    (widen)
    (if (not (and (nd/is-periodical-heading-p)
				  (not (nd/is-timestamped-heading-p))
                  (not (nd/heading-has-children 'nd/is-periodical-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-iterator-parent-headers ()
  (save-restriction
    (widen)
    (if (not (and (nd/is-iterator-heading-p)
                  (not (nd/heading-has-parent 'nd/is-iterator-heading-p))))
        (nd/skip-heading))))

(defun nd/skip-non-iterator-unscheduled ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (not (or (nd/is-scheduled-heading-p)
			(nd/is-deadlined-heading-p)))))

(defun nd/skip-non-project-tasks ()
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
  (nd/skip-heading-with
   nd/is-todoitem-p
   (nd/has-discontinuous-parent)))

(defun nd/skip-non-done-unclosed-todoitems ()
  (nd/skip-heading-with
   nd/is-todoitem-p
   (and (member keyword org-done-keywords)
        (not (nd/is-closed-heading-p)))))

(defun nd/skip-non-undone-closed-todoitems ()
  (nd/skip-heading-with
   nd/is-todoitem-p
   (and (not (member keyword org-done-keywords))
        (nd/is-closed-heading-p))))

(defun nd/skip-atomic-tasks-with-context ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (not (nd/heading-has-context-p))))

(defun nd/skip-project-tasks-with-context ()
  (nd/skip-heading-with
   nd/is-project-task-p
   (not (nd/heading-has-context-p))))

(defun nd/skip-projects-with-context ()
  (nd/skip-heading-with
   nd/is-project-p
   (not (nd/heading-has-context-p))))

(defun nd/skip-tasks-with-effort ()
  (nd/skip-heading-with
   nd/is-task-p
   (not (nd/heading-has-effort-p))))

(defun nd/skip-non-projects (&optional ignore-toplevel)
  (save-restriction
    (widen)
    (let ((keyword (nd/is-project-p)))
      (if keyword
          (if (and nd/agenda-limit-project-toplevel
				   (not ignore-toplevel)
                   (nd/heading-has-parent 'nd/is-todoitem-p))
              (nd/skip-subtree))
        (nd/skip-heading)))))

(defvar nd/agenda-limit-project-toplevel t
  "used to filter projects by all levels or top-level only")

(defvar nd/agenda-hide-incubator-tags t
  "used to filter incubator headings")

(defun nd/toggle-project-toplevel-display ()
  (interactive)
  (setq nd/agenda-limit-project-toplevel (not nd/agenda-limit-project-toplevel))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "Showing %s project view in agenda"
           (if nd/agenda-limit-project-toplevel "toplevel" "complete")))

(defun nd/toggle-agenda-var (var msg)
  (interactive)
  (set var (not (eval var)))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message msg))

(defun nd/org-agenda-filter-non-context ()
  "A quick and dirty agenda filter that removes all
tasks with context tags"
  (interactive)
  (let* ((tags-list (mapcar #'car org-tag-alist))
         (context-tags (append
                        (nd/filter-list-prefix "@" tags-list)
                        (nd/filter-list-prefix "#" tags-list))))
    (setq org-agenda-tag-filter
          (mapcar (lambda(tag) (concat "-" tag)) context-tags))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag)))

(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-prefix-format
      '((agenda . "  %-12:c %-5:e %?-12t% s")
        (timeline . "  % s")
        (todo . "  %-12:c")
        (tags . "  %-12:c %-5:e ")
        (search . "  %-12:c")))

(defconst nd/org-agenda-todo-sort-order '("NEXT" "WAIT" "HOLD" "TODO"))

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

(defun nd/org-agenda-filter-status (filter status-fun a-line)
  "Filter for org-agenda-before-sorting-filter-function intended for
agenda project views (eg makes the assumption that all entries are
from projects in the original org buffer)

Will go to the original org buffer and determine the project status
after which it will check if status is in filter. If true, the flag
string in the prefix is replaced with the status and the status is
set as a text property for further sorting"
  (let* ((m (get-text-property 1 'org-marker a-line))
		 (s (with-current-buffer (marker-buffer m)
			  (goto-char m)
			  (funcall status-fun))))
	(if (member s filter)
		(org-add-props (replace-regexp-in-string
					   "xxxx" (symbol-name s) a-line)
					  nil 'project-status s))))

(defun nd/org-agenda-sort-prop (prop order a b)
  (let* ((ta (get-text-property 1 prop a))
		 (tb (get-text-property 1 prop b))
		 (pa (position ta order :test (if (stringp ta) #'equal)))
         (pb (position tb order :test (if (stringp tb) #'equal))))
    (cond ((or (null pa) (null pb)) nil)
          ((< pa pb) +1)
          ((> pa pb) -1))))

(defun nd/agenda-base-header-cmd (match header skip-fun)
  `(tags
    ,match
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-sorting-strategy '(category-keep)))))

(defun nd/agenda-base-task-cmd (match header skip-fun &optional sort)
  (or sort (setq sort ''(category-keep)))
  `(tags-todo
    ,match
    ((org-agenda-overriding-header ,header)
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-todo-ignore-with-date t)
     (org-agenda-sorting-strategy ,sort))))

(let* ((actionable "-NA-REFILE-%inc")
	   (periodical "PARENT_TYPE=\"periodical\"")
	   (iterator "PARENT_TYPE=\"iterator\"")
	   (habit "STYLE=\"habit\"")
	   (task-match (concat actionable "-" periodical "-" habit "/!"))
       (act-no-rep-match (concat actionable "-" periodical "-" iterator "-" habit "/!"))
       (peri-match (concat actionable "+" periodical "-" iterator "-" habit))
       (iter-match (concat actionable "-" periodical "+" iterator "-" habit "/!")))

  (setq org-agenda-custom-commands
        `(("t"
           "Task View"
           ((agenda "" ((org-agenda-skip-function '(nd/skip-headings-with-tags '("%inc" "REFILE")))
						(org-agenda-include-diary t)))
            ,(nd/agenda-base-task-cmd act-no-rep-match
                                          "Project Tasks"
                                          ''nd/skip-non-project-tasks
                                          ''(user-defined-up category-keep))
            ,(nd/agenda-base-task-cmd act-no-rep-match
                                          "Atomic Tasks"
                                          ''nd/skip-non-atomic-tasks)))

          ("p"
           "Project View"
		   ((tags-todo
		  	 ,act-no-rep-match
		  	 ((org-agenda-overriding-header
		  	   (concat (and
		  				nd/agenda-limit-project-toplevel "Toplevel ")
		  			   "Projects"))
		  	  (org-agenda-skip-function '(nd/skip-non-projects))
		  	  (org-agenda-before-sorting-filter-function
			   (lambda (l) (nd/org-agenda-filter-status
					   '(:scheduled-project :invalid-todostate :undone-complete
											:done-incomplete :stuck :waiting
											:held :active)
					   'nd/get-project-status l)))
			  (org-agenda-cmp-user-defined
			   (lambda (a b) (nd/org-agenda-sort-prop
						 'project-status
						 '(:scheduled-project :invalid-todostate :undone-complete
											  :done-incomplete :stuck :waiting
											  :held :active)
						 a b)))
		  	  (org-agenda-prefix-format '((tags . "  %-12:c %(format \"xxxx: \")")))
		  	  (org-agenda-sorting-strategy '(user-defined-down category-keep))))))
		  
          ("P"
           "Periodical View"
		   ((tags
			 ,(concat actionable "-" iterator "+" periodical "-" habit)
		  	 ((org-agenda-overriding-header "Periodical Status")
		  	  (org-agenda-skip-function '(nd/skip-non-periodical-parent-headers))
		  	  (org-agenda-before-sorting-filter-function
			   (lambda (l) (nd/org-agenda-filter-status
					   nd/peri-statuscodes 'nd/get-periodical-status l)))
			  (org-agenda-cmp-user-defined
			   (lambda (a b) (nd/org-agenda-sort-prop
						 'project-status nd/peri-statuscodes a b)))
		  	  (org-agenda-prefix-format '((tags . "  %-12:c %(format \"xxxx: \")")))
		  	  (org-agenda-sorting-strategy '(user-defined-down category-keep))))
            ,(nd/agenda-base-header-cmd "-NA-REFILE+PARENT_TYPE=\"periodical\""
										"Untimestamped"
										''nd/skip-non-periodical-untimestamped)))

          ("i"
           "Iterator View"
           ((tags
			 "-NA-REFILE+PARENT_TYPE=\"iterator\""
		  	 ((org-agenda-overriding-header "Iterator Status")
		  	  (org-agenda-skip-function '(nd/skip-non-iterator-parent-headers))
		  	  (org-agenda-before-sorting-filter-function
			   (lambda (l) (nd/org-agenda-filter-status nd/iter-statuscodes 'nd/get-iterator-status l)))
			  (org-agenda-cmp-user-defined
			   (lambda (a b) (nd/org-agenda-sort-prop 'project-status nd/iter-statuscodes a b)))
		  	  (org-agenda-prefix-format '((tags . "  %-12:c %(format \"xxxx: \")")))
		  	  (org-agenda-sorting-strategy '(user-defined-down category-keep))))
            ,(nd/agenda-base-task-cmd "-NA-REFILE+PARENT_TYPE=\"iterator\"/!"
									  "Unscheduled or Undeaded"
									  ''nd/skip-non-iterator-unscheduled)))
		  
          ("I"
           "Incubator View"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-time-grid nil)
                        (org-agenda-entry-types '(:deadline :timestamp))))
            ,(nd/agenda-base-task-cmd "-NA-REFILE+%inc/!"
									  "Incubated Tasks"
									  ''nd/skip-non-atomic-tasks)
		   (tags-todo
			"-NA-REFILE+%inc/!"
		  	 ((org-agenda-overriding-header
		  	   (concat (and
		  				nd/agenda-limit-project-toplevel "Toplevel ")
		  			   "Incubated Projects"))
		  	  (org-agenda-skip-function '(nd/skip-non-projects))
		  	  (org-agenda-before-sorting-filter-function
			   (lambda (l) (nd/org-agenda-filter-status
					   '(:scheduled-project :invalid-todostate :undone-complete
											:done-incomplete :stuck :waiting
											:held :active)
					   'nd/get-project-status l)))
			  (org-agenda-cmp-user-defined
			   (lambda (a b) (nd/org-agenda-sort-prop
						 'project-status
						 '(:scheduled-project :invalid-todostate :undone-complete
											  :done-incomplete :stuck :waiting
											  :active :held)
						 a b)))
		  	  (org-agenda-prefix-format '((tags . "  %-12:c %(format \"xxxx: \")")))
		  	  (org-agenda-sorting-strategy '(user-defined-down category-keep))))))

          ("r"
           "Refile and Critical Errors"
           ((tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile"))
                  (org-tags-match-list-sublevels nil))
            ,(nd/agenda-base-task-cmd task-match
									  "Discontinous Project"
									  ''nd/skip-non-discontinuous-project-tasks)
			,(nd/agenda-base-header-cmd task-match
										"Undone Closed"
										''nd/skip-non-undone-closed-todoitems)
            ,(nd/agenda-base-header-cmd (concat actionable "-" periodical)
										"Done Unclosed"
										''nd/skip-non-done-unclosed-todoitems)))

          ("A"
           "Archivable Tasks and Projects"
           (,(nd/agenda-base-header-cmd (concat actionable "-" periodical "-" habit)
										"Archivable Atomic Tasks and Iterators"
										''nd/skip-non-archivable-atomic-tasks)
            ,(nd/agenda-base-header-cmd (concat actionable "-" habit)
										"Stale Tasks and Periodicals"
										''nd/skip-non-stale-headings)
			(tags-todo
			 ,(concat actionable "-" periodical "-" iterator "-" habit)
		  	 ((org-agenda-overriding-header
		  	   (concat (and	nd/agenda-limit-project-toplevel "Toplevel ")
		  			   "Archivable Projects"))
		  	  (org-agenda-skip-function '(nd/skip-non-projects))
		  	  (org-agenda-before-sorting-filter-function
			   (lambda (l) (nd/org-agenda-filter-status '(:archivable) 'nd/get-project-status l)))
			  (org-agenda-cmp-user-defined
			   (lambda (a b) (nd/org-agenda-sort-prop 'project-status '(:archivable) a b)))
		  	  (org-agenda-prefix-format '((tags . "  %-12:c %(format \"xxxx: \")")))
		  	  (org-agenda-sorting-strategy '(user-defined-down category-keep)))))))))

(setq org-agenda-start-on-weekday 0)
(setq org-agenda-span 'day)
(setq org-agenda-current-time-string "### -- NOW -- ###")
(setq org-agenda-time-grid '((daily today remove-match)
							 (0800 1000 1200 1200 1400 1600)
                             "......" "-----------------"))

(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(defun nd/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'nd/org-auto-exclude-function)

(setq org-columns-default-format
      "%25ITEM %4TODO %TAGS %5Effort{:} %OWNER(OWN)")

(set-face-attribute 'org-column nil :background "#1e2023")
;; org-columns-summary-types

(defun nd/mark-subtree-keyword (new-keyword &optional exclude)
  "marks all tasks in a subtree with keyword unless original keyword
is in the optional argument exclude"
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
  "marks all tasks in subtree as DONE unless they are already canc"
  (interactive)
  (nd/mark-subtree-keyword "DONE" '("CANC")))

(defun nd/org-clone-subtree-with-time-shift (n &optional shift)
  "Like `org-clone-subtree-with-time-shift' except it resets checkboxes
and reverts all todo keywords to TODO"
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

(use-package calfw-org
  :init
  :ensure t
  :config (setq cfw:fchar-junction ?╋
                cfw:fchar-vertical-line ?┃
                cfw:fchar-horizontal-line ?━
                cfw:fchar-left-junction ?┣
                cfw:fchar-right-junction ?┫
                cfw:fchar-top-junction ?┯
                cfw:fchar-top-left-corner ?┏
                cfw:fchar-top-right-corner ?┓))

(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent
	  mu4e-maildir "/mnt/data/Mail"
	  mu4e-drafts-folder "/gmail/[Gmail].Drafts"
	  mu4e-sent-folder   "/gmail/[Gmail].Sent Mail"
	  mu4e-trash-folder  "/gmail/[Gmail].Trash")

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
	  user-mail-address "natedwarshuis@gmail.com"
	  user-full-name "Nate Dwarshuis"

      mu4e-sent-messages-behavior 'delete
      
	  starttls-use-gnutls t
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials '(("smtp.gmail.com" 587
								   "natedwarshuis@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587
	  smtpmail-smtp-server "smtp.gmail.com")

(defvar nd-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list nd-term-shell)))
(ad-activate 'ansi-term)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
