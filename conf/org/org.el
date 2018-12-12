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
