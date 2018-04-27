
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

(global-set-key (kbd "<f1>") 'org-agenda)
(global-set-key (kbd "<f2>") 'org-capture)
(global-set-key (kbd "<f3>") 'org-iswitchb)

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

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light coral" :weight bold)
              ("NEXT" :foreground "khaki" :weight bold)
              ("DONE" :foreground "light green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "violet" :weight bold)
              ("CANCELLED" :foreground "deep sky blue" :weight bold))))

(setq  org-tag-alist '((:startgroup)
                       ("@errand" . ?e)
                       ("@work" . ?w)
                       ("@home" . ?h)
                       ("@travel" . ?f)
                       (:endgroup)
                       ("#laptop" . ?L)
                       ("#hood" . ?H)
                       ("WORK" . ?W)
                       ("PERSONAL" . ?P)
                       ("NOTE" . ?N)
                       ("FLAGGED" . ??)))

;; TODO I'm sure there is a better way to do this in lisp
(setq org-tag-faces
      '(("@errand" . (:foreground "PaleGreen"))
        ("@work" . (:foreground "PaleGreen"))
        ("@home" . (:foreground "PaleGreen"))
        ("@travel" . (:foreground "PaleGreen"))
        ("#laptop" . (:foreground "SkyBlue"))
        ("#hood" . (:foreground "SkyBlue"))))

;; this is basically the same as putting the properties at the top of all org files
(add-to-list 'org-default-properties "Project_Type")
(setq org-global-properties '(("Project_Type_ALL" . "series")))
;; TODO this may not be needed
(setq org-use-property-inheritance '("Project_Type"))

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Org/capture.org") "* TODO %?\ndeliverable: \n%U\n")
        ("n" "note" entry (file "~/Org/capture.org") "* %? :NOTE:\n%U\n" )
        ("a" "appointment" entry (file "~/Org/capture.org") "* TODO %?\n%U\n%^t\n" )
        ("m" "multi-day" entry (file "~/Org/capture.org") "* TODO %?\n%U\n%^t--%^t\n" )
        ("d" "deadline" entry (file "~/Org/capture.org") "* TODO %?\nDEADLINE: %^t\ndeliverable:\n%U\n" )
        
        ("j" "journal" entry (file+datetree "~/Org/diary.org") "* %?\n%U\n")
        ("p" "org-protocol" entry (file+headline ,(concat org-directory "~/Org/capture.org") "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "org-protocol" entry (file+headline ,(concat org-directory "~/Org/capture.org") "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")            
        ("h" "habit" entry (file "~/Org/capture.org")
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 ("~/Org/reference/idea.org" :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)

(setq org-refile-allow-creating-parent-nodes 'confirm)

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

(defun nd/is-series-header-p ()
  "return t if headline has property Project_Type=series"
  (equal "series" (org-entry-get nil "Project_Type")))

(defun nd/is-closed-heading-p ()
  "return timestamp if headline is closed"
  (let ((timestamp (org-entry-get nil "CLOSED")))
    (if timestamp (float-time (date-to-time timestamp)))))

(defvar nd/archive-delay-days 30
  "the number of days to wait before tasks show up in the archive view")

(defun nd/is-archivable-heading-p ()
  "return timestamp if todoitem is closed and older than specified time"
  (let ((timestamp (nd/is-closed-heading-p)))
    ;; NOTE we do not ensure that the todo state is in done keywords
    ;; this is to allow easier error correction in slip functions
    (if (and timestamp (> (- (float-time) timestamp) (* 60 60 24 nd/archive-delay-days)))
        timestamp)))

(defun nd/is-archivable-atomic-task-p ()
  "return keyword if heading is an archivable task"
  (and (nd/is-archivable-heading-p) (nd/is-atomic-task-p)))
  
(defun nd/is-archivable-project-p ()
  "return keyword if heading is an archivable task"
  (and (nd/is-archivable-heading-p) (nd/is-project-p)))

(defun nd/is-active-task-p ()
  "return keyword if task is either NEXT or scheduled"
  (let ((keyword (nd/is-task-p)))
    (if (or (equal keyword "NEXT") (nd/is-scheduled-heading-p))
        keyword)))

(defun nd/is-blocked-task-p ()
  "return keyword if task is WAITING"
  (equal (nd/is-task-p) "WAITING"))

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

(defun nd/is-discontinuous-project-task-p ()
  "detects todoitems that are children of non-todoitems
that in turn are children of todoitems (discontinous project)"
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
  '("WAITING" "NEXT")
  "projects cannot have these todostates") 

;; project level testing
(defconst nd/project-statuscodes
  '(:archivable
    :complete
    :stuck
    :held
    :waiting
    :active
    :done-incomplete
    :undone-complete
    :undone-closed
    :done-unclosed
    :invalid-todostate
    :scheduled-project
    :discontinous)
  "list of statuscodes to be used in assessing projects
Note they are listed in order of priority (eg items further
down the list override higher items")

(defmacro nd/compare-statuscodes (operator statuscode-1 statuscode-2)
  "syntactic suger to compare statuscodes by position"
  `(,operator (position ,statuscode-1 nd/project-statuscodes)
     (position ,statuscode-2 nd/project-statuscodes)))
  
(defun nd/status< (statuscode-1 statuscode-2)
  "returns t is statuscode-1 is lesser priority than statuscode-2"
  (nd/compare-statuscodes < statuscode-1 statuscode-2))

(defun nd/status> (statuscode-1 statuscode-2)
  "returns t is statuscode-1 is greater priority than statuscode-2"
  (nd/compare-statuscodes > statuscode-1 statuscode-2))

(defun nd/status= (statuscode-1 statuscode-2)
  "returns t is statuscode-1 is equal priority than statuscode-2"
  (nd/compare-statuscodes = statuscode-1 statuscode-2))

(defun nd/descend-into-project ()
  "returns statuscode of project and recursively descends into subprojects"
  (let ((project-state :archivable)
        (previous-point))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      ;; loop breaks if active or higher priority
      ;; note that all invalid statuscodes are higher
      ;; thus this function will only return the first
      ;; encountered error
      (while (and (nd/status< project-state :active)
                  (> (point) previous-point))
        (let ((keyword (nd/is-todoitem-p)))
          (if keyword
              (let ((cur-state
                     (if (nd/heading-has-children)
                         (cond ((member keyword nd/project-invalid-todostates) :invalid-todostate)
                               ((nd/is-scheduled-heading-p) :scheduled-project)
                               ((equal keyword "CANCELLED") (if (nd/is-archivable-heading-p)
                                                                :archivable
                                                              :complete))
                               ((equal keyword "HOLD") :held)
                               (t (let ((child-statuscode (nd/descend-into-project)))
                                    (cond ((equal keyword "TODO")
                                           (if (nd/status> child-statuscode :complete)
                                               child-statuscode
                                             :undone-complete))
                                          (t (case child-statuscode
                                               (:complete :complete)
                                               (:archivable (if (nd/is-archivable-heading-p)
                                                                :archivable
                                                              :complete))
                                               (t (if (= child-statuscode :complete)
                                                      :complete
                                                    :done-imcomplete))))))))
                       (cond ((equal keyword "HOLD") :held)
                             ((equal keyword "WAITING") :waiting)
                             ((equal keyword "NEXT") :active)
                             ((and (equal keyword "TODO") (nd/is-scheduled-heading-p)) :active)
                             ((equal keyword "TODO") :stuck)
                             ((nd/is-archivable-heading-p) :archivable)
                             (t :complete)))))
                (if (nd/status> cur-state project-state)
                    (setq project-state cur-state)))))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    project-state))

(defun nd/is-project-status-p (statuscode)
  (let ((keyword (nd/is-project-p)))
    (if keyword
        (cond ((member keyword nd/project-invalid-todostates)
               (if (nd/status= statuscode :invalid-todostate) keyword))
              ((equal keyword "HOLD") (if (nd/status= statuscode :held) keyword))
              ((equal keyword "CANCELLED") (if (nd/is-archivable-heading-p)
                                               (if (nd/status= statuscode :archivable) keyword)
                                             (if (nd/status= statuscode :complete) keyword)))
              (t (let ((child-statuscode (nd/descend-into-project)))
                   (cond ((equal keyword "DONE")
                          (if (nd/is-archivable-heading-p)
                              ;; TODO make my statuscode condition checker handle multiples
                              (if (and (nd/status= statuscode :archivable)
                                       (nd/status= child-statuscode :archivable))
                                  keyword)
                            (if (and (nd/status= statuscode :complete)
                                     (nd/status= child-statuscode :complete))
                                keyword
                              (if (and (nd/status> child-statuscode :complete)
                                       (nd/status= statuscode :done-incomplete))
                                  keyword))))
                         ((equal keyword "TODO")
                          (if (nd/status> child-statuscode :complete)
                              (if (nd/status= statuscode child-statuscode) keyword)
                            (if (nd/status= statuscode :undone-complete) keyword)))
                         (t (if (nd/status= statuscode child-statuscode) keyword)))))))))

;; TODO we could clean this up with macros
(defun nd/skip-non-atomic-tasks ()
  (save-restriction
    (widen)
    (if (not (nd/is-atomic-task-p))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defun nd/skip-non-next-project-tasks ()
  (save-restriction
    (widen)
    ;; TODO skip over invalid and held
    (if (not (equal (nd/is-project-task-p) "NEXT"))
        (save-excursion (or (outline-next-heading) (point-max))))))
  
(defun nd/skip-non-waiting-project-tasks ()
  (save-restriction
    (widen)
    ;; TODO skip over invalid and held
    (if (not (equal (nd/is-project-task-p) "WAITING"))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defun nd/skip-non-held-project-tasks ()
  (save-restriction
    (widen)
    ;; TODO skip over invalid and held
    (if (not (equal (nd/is-project-task-p) "HOLD"))
        (save-excursion (or (outline-next-heading) (point-max))))))
  
;; slip functions
(defun nd/skip-non-discontinuous-project-tasks ()
  (save-restriction
    (widen)
    (if (not (nd/is-discontinuous-project-task-p))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defun nd/skip-non-done-open-todoitems ()
  (save-restriction
    (widen)
    (if (not (and (member (nd/is-todoitem-p) org-done-keywords) (not (nd/is-closed-heading-p))))
        (save-excursion (or (outline-next-heading) (point-max))))))
  
(defun nd/skip-non-undone-closed-todoitems ()
  (save-restriction
    (widen)
    (if (not (and (not (member (nd/is-todoitem-p)) org-done-keywords) (nd/is-closed-heading-p)))
        (save-excursion (or (outline-next-heading) (point-max))))))

;; projects
;; TODO skip entire subtree if we don't need to evaluate anything inside
;; otherwise (for example) a held project will still have it's subtasks show up
(defun nd/skip-projects-without-statuscode (statuscode)
  (save-restriction
    (widen)
    (if (not (nd/is-project-status-p statuscode))
        (save-excursion (or (outline-next-heading) (point-max))))))

;; top-level projects
(defun nd/skip-subprojects-without-statuscode (statuscode)
  (save-restriction
    (widen)
    (if (or (nd/heading-has-parent) (not (nd/is-project-status-p statuscode)))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defun nd/skip-series-projects-without-statuscode (statuscode)
  (save-restriction
    (widen)
    (if (not (and (nd/is-series-heading-p) (nd/is-project-status-p statuscode)))
        (save-excursion (or (outline-next-heading) (point-max))))))
;; series projects
;; defined as project with property Project_type=series
;; must have:
;; - one level of subtasks
;; - all subtasks either TODO/scheduled, NEXT, DONE, CANCELLED
;; - at least one TODO/scheduled or NEXT (active) ..else empty
;; invalid if:
;; - project header is invalid project header (typical rules apply)

;; archiving
(defun nd/skip-non-archivable-atomic-tasks ()
  (save-restriction
    (widen)
    (if (not (nd/is-archivable-atomic-task-p))
        (save-excursion (or (outline-next-heading) (point-max))))))

(defvar nd/agenda-limit-project-toplevel t
  "used to filter projects by all levels or top-level only")

(defun nd/toggle-project-toplevel-display ()
  (interactive)
  (setq nd/agenda-limit-project-toplevel (not nd/agenda-limit-project-toplevel))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "Showing %s project view in agenda" (if nd/agenda-limit-project-toplevel "toplevel" "complete")))

(defun nd/agenda-base-task-command (keyword skip-fun)
  "shorter syntax to define task agenda commands"
  `(tags-todo
    "-NA-REFILE/!"
    ((org-agenda-overriding-header (concat ,keyword " Tasks"))
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-todo-ignore-with-date 'all)
     (org-agenda-sorting-strategy '(category-keep)))))

(defun nd/agenda-base-project-command (match keyword statuscode)
  "shorter syntax to define project agenda commands"
  `(tags
    ,match
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
      `(("t"
         "Task View"
         ((agenda "" nil)
          ,(nd/agenda-base-task-command "Next Project" ''nd/skip-non-next-project-tasks)
          ,(nd/agenda-base-task-command "Waiting Project" ''nd/skip-non-waiting-project-tasks)
          ,(nd/agenda-base-task-command "Atomic" ''nd/skip-non-atomic-tasks)
          ,(nd/agenda-base-task-command "Held Project" ''nd/skip-non-held-project-tasks)))
        ("o"
         "Project Overview"
         (,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/!" "Stuck" :stuck)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/!" "Waiting" :waiting)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/!" "Active" :active)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/!" "Held" :held)))
        ("r"
         "Refile and errors"
         ((tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile"))
                (org-tags-match-list-sublevels nil))
          ,(nd/agenda-base-task-command "Discontinous Project" ''nd/skip-non-discontinuous-project-tasks)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/!" "Unmarked Completed" :complete)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC-Project_Type=\"series\"/" "Invalid" :invalid-todostate)
          ;; ,(nd/agenda-base-task-command "Done But Not Closed" ''nd/skip-non-done-open-todoitems)
          ;; ,(nd/agenda-base-task-command "Closed But Not Done" ''nd/skip-non-open-closed-todoitems)
          ))
        ("s"
         "Series projects"
         (,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC+Project_Type=\"series\"/!" "Active Series" :active)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC+Project_Type=\"series\"/!" "Empty Series" :complete)))
        ("A"
         "Archivable Tasks and Projects"
         ((tags "-NA-REFILE/"
                ((org-agenda-overriding-header "Atomic Tasks to Archive")
                 (org-agenda-skip-function 'nd/skip-non-archivable-atomic-tasks)
                 (org-tags-match-list-sublevels nil)))
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC+Project_Type=\"series\"/!" "Archivable Series" :archivable)
          ,(nd/agenda-base-project-command "-NA-REFILE-ATOMIC/" "Archivable" :archivable)))))

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
