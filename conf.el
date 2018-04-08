
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

(when window-system (global-prettify-symbols-mode t))

(when window-system (global-hl-line-mode t))

(defalias 'yes-or-no-p 'y-or-n-p) ; eliminate yes or no prompt on killing procs

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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10))))

(global-set-key (kbd "C-h a") 'apropos)

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

(use-package ido
  :ensure t
  :bind
  ("C-x C-b" . 'ido-switch-buffer)
  ("C-x b" . 'ibuffer)
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-max-directory-size 100000)
  (setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)
  (use-package ido-vertical-mode
    :ensure t
    :init
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))


  ;; (setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :bind
  ("M-x" . 'smex)
  ("M-X" . 'smex-major-mode-commands))

(use-package rainbow-delimiters
  :ensure t
  :delight
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-background nil))

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

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-org
    :ensure t
    :after org
    :delight
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))

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

(setq org-log-done t)
(setq org-src-window-setup 'current-window)
(setq org-startup-indented t)
(delight 'org-indent-mode)
(setq org-directory "~/Org")

;;(add-hook 'org-capture-mode-hook 'evil-append)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; consider adding f1-12 shortcuts for org things that must be a) fast and b) work in any mode

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light coral" :weight bold)
              ("NEXT" :foreground "khaki" :weight bold)
              ("DONE" :foreground "light green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "violet" :weight bold)
              ("CANCELLED" :foreground "deep sky blue" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq  org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@work" . ?o)
                            ("@home" . ?h)
                            ("@travel" . ?f)
                            (:endgroup)
                            ("LAPTOP" . ?L)
                            ("WAITING" . ?W)
                            ("HOLD" . ?H)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?O)
                            ("NOTE" . ?N)
                            ("CANCELLED" . ?C)
                            ("FLAGGED" . ??))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Org/capture.org") "* TODO %?\n%U\n")
              ("n" "note" entry (file "~/Org/capture.org") "* %? :NOTE:\n%U\n" )
              ("a" "appointment" entry (file "~/Org/capture.org") "* TODO %?\n%U\n%^t\n" )
              ("m" "multi-day" entry (file "~/Org/capture.org") "* TODO %?\n%U\n%^t--%^t\n" )
              ("d" "deadline" entry (file "~/Org/capture.org") "* TODO %?\nDEADLINE: %^t\n%U\n" )

              ("j" "journal" entry (file+datetree "~/Org/diary.org") "* %?\n%U\n")
              ("p" "org-protocol" entry (file+headline ,(concat org-directory "~/Org/capture.org") "Inbox")
               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
              ("L" "org-protocol" entry (file+headline ,(concat org-directory "~/Org/capture.org") "Inbox")
               "* %? [[%:link][%:description]] \nCaptured On: %U")            
              ("h" "habit" entry (file "~/Org/capture.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 ("~/Org/reference/idea.org" :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-indirect-buffer-display 'current-window)

(defun nd/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'nd/verify-refile-target)

(setq org-agenda-files (quote ("~/Org"
                               "~/Org/large_projects"
                               "~/Org/reference")))
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)

(evil-define-key 'motion org-agenda-mode-map "T" 'nd/toggle-project-toplevel-display)

(setq org-agenda-span 'day)
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

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

(defun nd/is-todoitem-p ()
  "return todo keyword if present in headline (which defines the heading as a todoitem)
this is used to both test if a heading is a todoitem and retrieving the keyword"
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

(defun nd/is-project-p ()
  "return todo keyword if heading is todoitem and has children"
  (and (nd/heading-has-children) (nd/is-todoitem-p)))

(defun nd/is-task-p ()
  "return todo keyword if heading is todoitem with no children"
  (and (not (nd/heading-has-children)) (nd/is-todoitem-p)))

(defun nd/is-atomic-task-p ()
  "return todo keyword if heading is task with no parents"
  (and (not (nd/heading-has-parent)) (nd/is-task-p)))
  
(defun nd/is-project-task-p ()
  "return todo keyword if heading is task with no parents"
  (and (nd/heading-has-parent) (nd/is-task-p)))

(defun nd/is-scheduled-heading-p ()
  "return timestamp if headline is scheduled"
  (org-entry-get nil "SCHEDULED"))

(defun nd/is-active-task-p ()
  "return keyword if task is either NEXT or scheduled"
  (let ((keyword (nd/is-task-p)))
    (if (or (equal keyword "NEXT") (nd/is-scheduled-heading-p))
        keyword)))

(defun nd/is-blocked-task-p ()
  "return keyword if task is WAITING"
  (equal (nd/is-task-p) "WAITING"))

(defconst nd/project-invalid-todostates
  '("WAITING" "NEXT")
  "projects cannot have these todostates") 

(defun nd/heading-has-children ()
  "returns t if heading has todoitems in its immediate subtree"
  ;; TODO make this more efficient (and accurate) by only testing
  ;; the level immediately below (if it exists)
  (let ((has-children)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (outline-next-heading)
      (while (and (not has-children)
                  (< (point) subtree-end))
        (when (nd/is-todoitem-p)
          (setq has-children t))
;;        (org-forward-heading-same-level 1 t)))
        (outline-next-heading)))
    has-children))

(defun nd/heading-has-parent ()
  "returns parent keyword if heading is in the immediate subtree of a todoitem"
  (save-excursion (and (org-up-heading-safe) (nd/is-todoitem-p))))

(defun nd/test-first-order-project ()
  "tests the state of a project assuming first order.
if not first order, this function will iterate to the next project
and descend into it by calling itelf recursively.
function is not meant to be called independently."
  (let ((found-active)
        (previous-point))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not found-active)
                  (> (point) previous-point))
        (when (or (and (nd/is-project-p)
                       (nd/test-first-order-project))
                  (nd/is-active-task-p))
          (setq found-active t))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    found-active))

;; project level testing
;; TODO: is there a better way to handle statuscodes like this??? (array like thingy)
(defun nd/descend-into-project ()
  "returns statuscode according to state of project:
0: complete
10: stuck
20: held
30: waiting
40: active
50: invalid???

This function works on an assumed order of precendence:
- we start by assuming all projects as complete (eg only DONE and CANCELLED)
- if project has any TODO (regardless of DONE or CANCELLED) it is stuck
- if project has any HOLD (regardless of DONE, CANCELLED, or TODO) it is held
- in the same manner WAITING means waiting project
- in the same manner, NEXT means active. NEXT overrides all

Using this scheme, we simply compare the magnitude of the statuscodes"
  (let ((project-state 0)
        (previous-point))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (< project-state 40)
                  (> (point) previous-point))
        (let ((keyword (nd/is-todoitem-p))
              (has-children (nd/heading-has-children)))
          (if keyword
              (let ((cur-state
                     (if has-children
                         (cond ((equal keyword "HOLD") 20)
                               ((equal keyword "TODO") (nd/descend-into-project))
                               ;; NOTE: all projects are assumed to only have TODO, HOLD, CANCELLED, or DONE, hence the three possible statuscodes
                               (t 0))
                       (cond ((equal keyword "HOLD") 20)
                             ((equal keyword "WAITING") 30)
                             ((equal keyword "NEXT") 40)
                             ((and (equal keyword "TODO") (nd/is-scheduled-heading-p)) 40)
                             ((equal keyword "TODO") 10)
                             (t 0)))))
                (if (> cur-state project-state)
                    (setq project-state cur-state)))))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    project-state))

(defun nd/is-project-status-p (statuscode)
  (let ((keyword (nd/is-project-p)))
    (if keyword
        (cond ((member keyword nd/project-invalid-todostates) nil)
              ((and (equal keyword "HOLD") (= statuscode 20)) keyword)
              ((and (equal keyword "HOLD") (/= statuscode 20)) nil)
              ((= statuscode (nd/descend-into-project)) keyword)))))

;; NOTE: use save-restriction and widen if we ever actually use narrowing
;; tasks
(defun nd/skip-non-atomic-tasks ()
  (if (not (nd/is-atomic-task-p))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun nd/skip-non-next-project-tasks ()
  (if (not (equal (nd/is-project-task-p) "NEXT"))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun nd/skip-non-waiting-project-tasks ()
  (if (not (equal (nd/is-project-task-p) "WAITING"))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defun nd/skip-non-held-project-tasks ()
  (if (not (equal (nd/is-project-task-p) "HOLD"))
      (save-excursion (or (outline-next-heading) (point-max)))))
  
;; projects
(defun nd/skip-projects-without-statuscode (statuscode)
  (if (not (nd/is-project-status-p statuscode))
      (save-excursion (or (outline-next-heading) (point-max)))))

;; top-level projects
(defun nd/skip-subprojects-without-statuscode (statuscode)
  (if (or (nd/heading-has-parent) (not (nd/is-project-status-p statuscode)))
      (save-excursion (or (outline-next-heading) (point-max)))))

(defvar nd/agenda-limit-project-toplevel t
  "used to filter projects by all levels or top-level only")

(defun nd/toggle-project-toplevel-display ()
  (interactive)
  (setq nd/agenda-limit-project-toplevel (not nd/agenda-limit-project-toplevel))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "Showing %s project view in agenda" (if nd/agenda-limit-project-toplevel "toplevel" "complete")))

(defmacro nd/agenda-base-task-command (keyword skip-fun)
  "shorter syntax to define task agenda commands"
  `(tags-todo
    "-NA-REFILE/!"
    ((org-agenda-overriding-header (concat ,keyword " Tasks"))
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-todo-ignore-with-date 'all)
     (org-agenda-sorting-strategy '(category-keep)))))

(defmacro nd/agenda-base-project-command (keyword statuscode)
  "shorter syntax to define project agenda commands"
  `(tags-todo
    "-NA-REFILE-ATOMIC/!"
    ((org-agenda-overriding-header (concat
                                    (and nd/agenda-limit-project-toplevel "Toplevel ")
                                    ,keyword
                                    " Projects"))
     (org-agenda-skip-function (if nd/agenda-limit-project-toplevel
                                   '(nd/skip-subprojects-without-statuscode ,statuscode)
                                 '(nd/skip-projects-without-statuscode ,statuscode)))
     (org-agenda-sorting-strategy '(category-keep)))))

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-custom-commands
      `(("t" "Task view"
         ((agenda "" nil)
          ,(macroexpand '(nd/agenda-base-task-command "Next Project" 'nd/skip-non-next-project-tasks))
          ,(macroexpand '(nd/agenda-base-task-command "Waiting Project" 'nd/skip-non-waiting-project-tasks))
          ,(macroexpand '(nd/agenda-base-task-command "Atomic" 'nd/skip-non-atomic-tasks))
          ,(macroexpand '(nd/agenda-base-task-command "Held Project" 'nd/skip-non-held-project-tasks))))
        ("o" "Project Overview"
          (,(macroexpand '(nd/agenda-base-project-command "Stuck" 10))
           ,(macroexpand '(nd/agenda-base-project-command "Waiting" 20))
           ,(macroexpand '(nd/agenda-base-project-command "Active" 40))
           ,(macroexpand '(nd/agenda-base-project-command "Held" 30))))
        ("r" "Refile and errors"
         ((tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile"))
                (org-tags-match-list-sublevels nil))))))

(use-package org-bullets
  :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

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

(defvar nd-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list nd-term-shell)))
(ad-activate 'ansi-term)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
