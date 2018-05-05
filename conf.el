
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

;;(add-hook 'org-capture-mode-hook 'evil-append)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x x") 'nd/mark-subtree-done)
            (local-set-key (kbd "C-c C-x c") 'nd/org-clone-subtree-with-time-shift-reset)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "light coral" :weight bold)
              ("NEXT" :foreground "khaki" :weight bold)
              ("DONE" :foreground "light green" :weight bold)
              ("WAIT" :foreground "orange" :weight bold)
              ("HOLD" :foreground "violet" :weight bold)
              ("CANC" :foreground "deep sky blue" :weight bold))))

(setq  org-tag-alist '((:startgroup)
                       ("@errand" . ?e)
                       ("@work" . ?w)
                       ("@home" . ?h)
                       ("@travel" . ?t)
                       (:endgroup)

                       ("#laptop" . ?L)
                       ("#tcult" . ?T)

                       ("%note" . ?n)
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

;; not the most elegant but this will work
(setq org-tag-faces '())

(defun nd/add-tag-face (fg-name start end)
  "Adds list of cons cells to org-tag-faces with foreground set to fg-name.
Start and end specify the positions in org-tag-alist which define the tags
to which the faces are applied"
  (dolist (tag (mapcar #'car (subseq org-tag-alist start end)))
    (push `(,tag . (:foreground ,fg-name)) org-tag-faces)))

(nd/add-tag-face "PaleGreen" 1 5)
(nd/add-tag-face "SkyBlue" 6 8)
(nd/add-tag-face "PaleGoldenrod" 8 10)
(nd/add-tag-face "violet" 11 19)

(add-to-list 'org-default-properties "PARENT_TYPE")
(add-to-list 'org-default-properties "OWNER")
(setq org-global-properties
      '(("PARENT_TYPE_ALL" . "periodical iterator")
        ("Effort_ALL" . "00 10 30 60 90")))

;; TODO this may not be needed
(setq org-use-property-inheritance '("PARENT_TYPE"))

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Org/capture.org") "* TODO %?\ndeliverable: \n%U\n")
        ("n" "note" entry (file "~/Org/capture.org") "* %? :\\%note:\n%U\n" )
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

(setq org-agenda-files '("~/Org"
                      "~/Org/projects"
                      "~/Org/reference"))
;; (setq org-agenda-files '("~/Org/reference/agendatest.org"))
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)

(defun nd/get-date-property (date-property)
  "Helper function to get the date property and convert to a number.
If it does not have a date, it will return nil."
  (let ((timestamp (org-entry-get nil date-property)))
    (if timestamp (float-time (date-to-time timestamp)))))

(defun nd/is-timestamped-heading-p ()
  (nd/get-date-property "TIMESTAMP"))

(defun nd/is-scheduled-heading-p ()
  (nd/get-date-property "SCHEDULED"))

(defun nd/is-deadlined-heading-p ()
  (nd/get-date-property "DEADLINE"))

(defun nd/is-closed-heading-p ()
  (nd/get-date-property "CLOSED"))

(defun nd/is-stale-heading-p ()
  (let ((timestamp (nd/is-timestamped-heading-p)))
    (if (and timestamp (> (- (float-time) timestamp) 0))
        timestamp)))

(defvar nd/archive-delay-days 30
  "the number of days to wait before tasks show up in the archive view")

(defun nd/is-archivable-heading-p ()
  (let ((timestamp (nd/is-closed-heading-p)))
    (if (and timestamp (> (- (float-time) timestamp) (* 60 60 24 nd/archive-delay-days)))
        timestamp)))

(defun nd/is-todoitem-p ()
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

(defun nd/is-project-p ()
  (and (nd/heading-has-children) (nd/is-todoitem-p)))

(defun nd/is-task-p ()
  (and (not (nd/heading-has-children)) (nd/is-todoitem-p)))

(defun nd/is-atomic-task-p ()
  (and (not (nd/heading-has-parent)) (nd/is-task-p)))

(defun nd/is-series-heading-p ()
  "return t if headline has property Project_Type=series"
  (equal "series" (org-entry-get nil "Project_Type" t)))

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

(defconst nd/project-statuscodes
  '(:archivable
    :complete
    :stuck
    :held
    :waiting
    :active
    :done-incomplete
    :undone-complete
    :invalid-todostate
    :scheduled-project)
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
                               ((equal keyword "CANC") (if (nd/is-archivable-heading-p)
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
                                               (t (if (nd/status= child-statuscode :complete)
                                                      :complete
                                                    :done-incomplete))))))))
                       (cond ((equal keyword "HOLD") :held)
                             ((equal keyword "WAIT") :waiting)
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

(defmacro nd/is-project-keyword-status-p (test-keyword operator statuscode)
  "tests if a project has toplevel heading of top-keyword and
child status equal to status code and returns keyword if
both are true"
  `(and
    (equal ,keyword ,test-keyword)
    (nd/compare-statuscodes ,operator (nd/descend-into-project) ,statuscode)))

(defun nd/is-project-status-p (statuscode)
  "Returns t if project matches statuscode given. 
Note that this assumes the headline being tested is a valid project"
  (case statuscode
    ;; projects closed more than 30 days ago
    ;; note CANC overrides all subtasks/projects
    (:archivable
     (if (nd/is-archivable-heading-p)
         (or (equal keyword "CANC") 
             (nd/is-project-keyword-status-p "DONE" = :archivable))))
    
    ;; projects closed less than 30 days ago
    ;; note CANC overrides all subtasks/projects
    (:complete
     (if (not (nd/is-archivable-heading-p))
         (or (equal keyword "CANC")
             (nd/is-project-keyword-status-p "DONE" = :complete))))
    
    ;; projects with no waiting, held, or active components
    (:stuck
     (nd/is-project-keyword-status-p "TODO" = :stuck))
    
    ;; held projects
    ;; note toplevel HOLD overrides all subtasks/projects
    (:held
     (or (equal keyword "HOLD")
         (nd/is-project-keyword-status-p "TODO" = :held)))
    
    ;; projects with at least one waiting component
    (:waiting
     (nd/is-project-keyword-status-p "TODO" = :waiting))
    
    ;; projects with at least one active component
    (:active
     (nd/is-project-keyword-status-p "TODO" = :active))
    
    ;; projects marked DONE but still have undone subtasks
    (:done-incomplete
     (nd/is-project-keyword-status-p "DONE" > :complete))
    
    ;; projects marked TODO but all subtasks are done
    (:undone-complete
     (nd/is-project-keyword-status-p "TODO" < :stuck))
    
    ;; projects with invalid todo keywords
    (:invalid-todostate
     (member keyword nd/project-invalid-todostates))
    
    ;; projects with scheduled heading (only subtasks should be scheduled)
    (:scheduled-project
     (nd/is-scheduled-heading-p))

    ;; error if not known
    (t (if (not (member statuscode nd/project-statuscodes))
           (error "unknown statuscode")))))

;; helper functions
(defun nd/skip-item ()
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
           (nd/skip-item)))))

;; stale headings 
;; For archiving headings with old timestamps
;; Note that these are not always todo items
;; I only care about those that are not part
;; of projects (projects will get taken care
;; of when the entire project is finished)
;; and those that are not DONE/CANC (as
;; those appear in the regular archive
;; section)
(defun nd/skip-non-stale-headings ()
  (save-restriction
    (widen)
    (let ((keyword (nd/is-todoitem-p)))
      (if (not
           (and (nd/is-stale-heading-p)
                (not (member keyword org-done-keywords))
                (not (nd/heading-has-children))
                (not (nd/heading-has-parent))))
          (nd/skip-item)))))
  
;; atomic tasks
;; by definition these have no parents, so
;; we don't need to worry about skipping over projects
;; any todo state is valid and we only sort by done/canc
(defun nd/skip-non-unclosed-atomic-tasks ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (and (not (nd/is-timestamped-heading-p))
        (not (nd/is-scheduled-heading-p))
        (not (nd/is-deadlined-heading-p))
        (not (member keyword org-done-keywords)))))

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

;; project tasks
;; since these are part of projects I need to assess
;; if the parent project is skippable, in which case
;; I jump to the next subtree
;; Note that I only care about the keyword in these
;; cases because I don't archive these, I archive
;; their parent projects. The keywords I care about
;; are NEXT, WAIT, and HOLD because these are
;; definitive project tasks that require/inhibit
;; futher action
(defun nd/skip-non-keyword-project-tasks (skip-keyword)
  (save-restriction
    (widen)
    (let ((keyword (nd/is-todoitem-p)))
      (if keyword
          (if (nd/heading-has-children)
              (if (member keyword nd/project-skip-todostates)
                  (nd/skip-subtree)
                (nd/skip-item))
            (if (not (and (nd/heading-has-parent)
                          (not (nd/is-timestamped-heading-p))
                          (not (nd/is-scheduled-heading-p))
                          (not (nd/is-deadlined-heading-p))
                          (equal keyword skip-keyword)))
                (nd/skip-item)))
        (nd/skip-item)))))
  
;; task-level errors
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

(defun nd/skip-non-series-atomic-tasks ()
  (nd/skip-heading-with
   nd/is-atomic-task-p
   (nd/is-series-heading-p)))

;; projects
(defun nd/skip-projects-without-statuscode (statuscode)
  (save-restriction
    (widen)
    (let ((keyword (nd/is-project-p)))
      (if keyword
          (if (and nd/agenda-limit-project-toplevel
                   (nd/heading-has-parent))
              (nd/skip-subtree)
            (if (not (nd/is-project-status-p statuscode))
                (nd/skip-item)))
        (nd/skip-item)))))

(defvar nd/agenda-limit-project-toplevel t
  "used to filter projects by all levels or top-level only")

(defun nd/toggle-project-toplevel-display ()
  (interactive)
  (setq nd/agenda-limit-project-toplevel (not nd/agenda-limit-project-toplevel))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "Showing %s project view in agenda"
           (if nd/agenda-limit-project-toplevel "toplevel" "complete")))

(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-prefix-format
      '((agenda . " %-12:c%-5:e%?-12t% s")
        (timeline . " % s")
        (todo . " %-12:c")
        (tags . " %-12:c%-5:e")
        (search . " %-12:c")))

(defun nd/agenda-base-task-command (match keyword skip-fun)
  "shorter syntax to define task agenda commands"
  `(tags
    ,match
    ((org-agenda-overriding-header (concat ,keyword " Tasks"))
     (org-agenda-skip-function ,skip-fun)
     (org-agenda-sorting-strategy '(category-keep)))))

(defun nd/agenda-base-project-command (match keyword statuscode)
  "shorter syntax to define project agenda commands"
  `(tags
    ,match
    ((org-agenda-overriding-header
      (concat (and nd/agenda-limit-project-toplevel "Toplevel ") ,keyword " Projects"))
     (org-agenda-skip-function '(nd/skip-projects-without-statuscode ,statuscode))
     (org-agenda-sorting-strategy '(category-keep)))))

(let ((task-view-match "-NA-REFILE")
      (project-view-match "-NA-REFILE-PARENT_TYPE=\"iterator\"/")
      (series-view-match "-NA-REFILE+PARENT_TYPE=\"iterator\"/"))
  (setq org-agenda-custom-commands
        `(("t"
           "Task View"
           ((agenda "" nil)
            ,(nd/agenda-base-task-command task-view-match "Next Project" ''(nd/skip-non-keyword-project-tasks "NEXT"))
            ,(nd/agenda-base-task-command task-view-match "Waiting Project" ''(nd/skip-non-keyword-project-tasks "WAIT"))
            ,(nd/agenda-base-task-command task-view-match "Atomic" ''nd/skip-non-unclosed-atomic-tasks)
            ,(nd/agenda-base-task-command task-view-match "Held Project" ''(nd/skip-non-keyword-project-tasks "HOLD"))))
          ("p"
           "Project View"
           (,(nd/agenda-base-project-command project-view-match "Stuck" :stuck)
            ,(nd/agenda-base-project-command project-view-match "Waiting" :waiting)
            ,(nd/agenda-base-project-command project-view-match "Active" :active)
            ,(nd/agenda-base-project-command project-view-match "Held" :held)))
          ("s"
           "Series View"
           (,(nd/agenda-base-project-command series-view-match "Stuck Series" :stuck)
            ,(nd/agenda-base-project-command series-view-match "Empty Series" :undone-complete)
            ,(nd/agenda-base-project-command series-view-match "Active Series" :active)
            ,(nd/agenda-base-project-command series-view-match "Waiting Series" :waiting)
            ,(nd/agenda-base-project-command series-view-match "Held Series" :held)
            ,(nd/agenda-base-task-command series-view-match "Uninitialized Series" ''nd/skip-non-series-atomic-tasks)))
          ("r"
           "Refile and Critical Errors"
           ((tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile"))
                  (org-tags-match-list-sublevels nil))
            ,(nd/agenda-base-task-command task-view-match "Discontinous Project" ''nd/skip-non-discontinuous-project-tasks)
            ,(nd/agenda-base-project-command project-view-match "Invalid Todostate" :invalid-todostate)))
          ("e"
           "Non-critical Errors"
           (,(nd/agenda-base-task-command task-view-match "Undone Closed" ''nd/skip-non-undone-closed-todoitems)
            ,(nd/agenda-base-task-command task-view-match "Done Unclosed" ''nd/skip-non-done-unclosed-todoitems)
            ,(nd/agenda-base-project-command project-view-match "Undone Completed" :undone-complete)
            ,(nd/agenda-base-project-command project-view-match "Done Incompleted" :done-incomplete)))
          ("A"
           "Archivable Tasks and Projects"
           (,(nd/agenda-base-task-command task-view-match "Archivable Atomic" ''nd/skip-non-archivable-atomic-tasks)
            ,(nd/agenda-base-task-command task-view-match "Stale" ''nd/skip-non-stale-headings)
            ,(nd/agenda-base-project-command series-view-match "Archivable Series" :archivable)
            ,(nd/agenda-base-project-command project-view-match "Archivable" :archivable))))))

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

(setq org-columns-default-format
      "%25ITEM %4TODO %TAGS %3Effort{+} %OWNER(OWN)")

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

(defun nd/org-clone-subtree-with-time-shift-reset (n &optional shift)
  "Like `org-clone-subtree-with-time-shift' except it resets checkboxes
and reverts all todo keywords to TODO"
  (interactive "nNumber of clones to produce: ")
  (let ((shift (read-from-minibuffer
                "Date shift per clone (e.g. +1w, empty to copy unchanged): ")))
    (condition-case err
        (progn
          (org-clone-subtree-with-time-shift n shift)
          (save-excursion
            (dotimes (i n)
             (org-forward-heading-same-level 1 t)
             (org-reset-checkbox-state-subtree)
             (nd/mark-subtree-keyword "TODO")
             (org-cycle))))
      (error (message "%s" (error-message-string err))))))

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
