
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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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

(setq org-agenda-span 'day)

(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-NA-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if nd/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'nd/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-with-date 'all)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-NA-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Atomic Tasks"
                                                                  (if nd/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'nd/skip-non-atomic-tasks)
                            (org-agenda-todo-ignore-with-date 'all)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-NA-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if nd/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'nd/skip-non-project-tasks)
                            (org-agenda-todo-ignore-with-date 'all)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-NA-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if nd/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'nd/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-with-date 'all)))
                (tags-todo "-NA-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'nd/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-NA-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'nd/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags "-NA-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'nd/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(defun nd/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'nd/org-auto-exclude-function)

;; functions to define headlings relative to project structure (or lack thereof)
(defun nd/is-todoitem-p ()
  "return t if headline has valid todo keyword"
  (member (nth 2 (org-heading-components)) org-todo-keywords-1))

(defun nd/todoitem-has-children ()
  "returns t if heading is a todoitem and has todoitems in its subtree"
  (if (nd/is-todoitem-p)
      (let ((has-children)
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (save-excursion
          (outline-next-heading)
          (while (and (not has-children)
                      (< (point) subtree-end))
            (when (nd/is-todoitem-p)
              (setq has-children t)
            (outline-next-heading))))
        has-children)))

(defun nd/todoitem-has-parent ()
  "returns t if heading is a todoitem that is in the subtree of another todoitem"
  (if (nd/is-todoitem-p)
      (let ((has-parent))
        (save-excursion
          (while (and (not has-parent) (org-up-heading-safe))
            (when (nd/is-todoitem-p)
              (setq has-parent t))))
        has-parent)))

(defun nd/is-project-p ()
  (nd/todoitem-has-children))

(defun nd/is-task-p ()
  (and (nd/is-todoitem-p) (not nd/todoitem-has-children)))

(defun nd/is-atomic-task-p ()
  (and (nd/is-task-p) (not (nd/todoitem-has-parent))))
  
;; functions to test tasks (which are "atomic")
;; (defun nd/is-scheduled-p ()
;;   "task with scheduled property"
;;   ((org-entry-get nil "SCHEDULED")))

;; org-forward-heading-same-level

;; task skip functions
(defun nd/skip-non-atomic-tasks ()
  (save-restriction
    (widen)
    (if (not (and ((nd/is-atomic-p) (not (nd/is-subtask-p)))))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defvar nd/hide-scheduled-and-waiting-next-tasks t)

(defun nd/toggle-next-task-display ()
  (interactive)
  (setq nd/hide-scheduled-and-waiting-next-tasks (not nd/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if nd/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun nd/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (nd/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;; project test functions
;; is state
;;   if project
;;     if order = 1
;;       return (state is true)
;;     else order > 1
;;       call is state (recursive)
;;   else if task
;;     return (state is true)
;; note: this needs to iterate through lines
(defun nd/is-active-project-p ()
  "return true if project has at least one
NEXT/scheduled task or active subproject"
  ;; if not a project then don't bother
  (if (nd/is-project-p)
      (let (((subtree-end (save-excursion (org-end-of-subtree t))))
            (is-active))
        (save-excursion
          (while (and (not is-active)
                      (< (point) subtree-end))
            (outline-heading-next)
            (cond ((nd/is-active-task-p) (setq is-active t))
                  ((nd/is-active-project-p) (setq is-active))))))))

;; (defun nd/skip-non-stuck-projects ()
  ;; goto next headline
  ;; if project
  ;;   if project order 1
  ;;     if it has NEXT, WAITING, HOLD, or a scheduled task
  ;;       then skip (return end of subtree)
  ;;     else stuck project, return nil
  ;;   else (order > 1)
  ;;     descend into project (recursion)
  ;; skip (either an atomic task or non-todo, return next heading)
;;  )

(defun nd/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (nd/list-sublevels-for-projects-indented)
  (if (save-excursion (nd/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((nd/is-project-p)
            nil)
           ((and (nd/is-subtask-p) (not (nd/is-atomic-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun nd/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((nd/is-atomic-p)
        nil)
       (t
        next-headline)))))

(defun nd/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((nd/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

(defun nd/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ;; ((org-is-habit-p)
       ;;  next-headline)
       ((and nd/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((nd/is-project-p)
        next-headline)
       ((and (nd/is-atomic-p) (not (nd/is-subtask-p)))
        next-headline)
       (t
        nil)))))

(defun nd/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((nd/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (not limit-to-project)
             (nd/is-subtask-p))
        subtree-end)
       ((and limit-to-project
             (nd/is-subtask-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun nd/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((nd/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((nd/is-subtask-p)
        subtree-end)
       (t
        nil)))))

(defun nd/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((nd/is-project-p)
        next-headline)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       ((and (nd/is-subtask-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (nd/is-subtask-p))
        subtree-end)
       (t
        nil)))))

(defun nd/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((nd/is-project-p)
        subtree-end)
       ;; ((org-is-habit-p)
       ;;  subtree-end)
       (t
        nil)))))

;; (defun nd/skip-non-subprojects ()
;;   "Skip trees that are not projects"
;;   (let ((next-headline (save-excursion (outline-next-heading))))
;;     (if (nd/is-subproject-p)
;;         nil
;;       next-headline)))

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
