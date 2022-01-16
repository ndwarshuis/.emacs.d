;;; org-x-dag.el --- Org-in-a-DAG -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan Dwarshuis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Welcome to Dagestan, you will be smeshed...

;;; Code:

;; TODO this depends on other stuff in org-x like the file and id operations
(require 'org)
(require 'org-ml)
(require 'dash)
(require 'dag)
(require 'ht)

;;; GLOBAL STATE

;; variables to store state

(defvar org-x-dag nil
  "The org-x DAG.

Each node in this DAG represents a headline with the following
characteristics:
- contained in a file as given by `org-x-dag-get-files'
- has a keyword
- either has an immediate parent with a keyword or has no parents
  with keywords

Each node is represented by a key, which is either a string
representing the headlines's ID property or a cons cell
like (FILE POS) representing the staring position in file/buffer
of the headline (aka a \"pseudo-marker\").")

(defvar org-x-dag-sync-state nil
  "An alist representing the sync state of the DAG.

The car of each cell is the file path, and the cdr is the md5 of
that file as it currently sits on disk.")

;; functions to construct nodes within state

(defun org-x-dag-build-key (file point level todo tags id)
  (list :file file
        :point point
        :level level
        :todo todo
        :tags tags
        :id id))
  ;; (if id (list :id file point id) (list :pm file point)))

(defun org-x-dag-key-get-file (key)
  "Return file for KEY."
  (plist-get key :file))
  ;; (nth 1 key))

(defun org-x-dag-key-get-point (key)
  "Return point for KEY."
  (plist-get key :point))
  ;; (nth 2 key))

;;; DAG SYNCHRONIZATION/CONSTRUCTION

(defun org-x-dag-get-files ()
  "Return a list of all files to be used in the DAG."
  ;; (list "/mnt/data/Org/projects/router.org"
  ;;       "/mnt/data/Org/projects/omnimacs.org"
  ;;       ))
  `(,(org-x-get-lifetime-goal-file)
    ,(org-x-get-endpoint-goal-file)
    ,@(org-x-get-action-files)))

(defun org-x-dag-get-md5 (path)
  "Get the md5 checksum of PATH."
  (with-temp-buffer
    (let ((rc (call-process "md5sum" nil (current-buffer) nil path)))
      (if (/= 0 rc) (error "Could not get md5 of %s" path)
        (->> (buffer-string)
             (s-match "^\\([0-9a-z]+\\)")
             (cadr))))))

(defun org-x-dag-md5-matches-p (path md5)
  "Return t if the md5 of PATH on disk `equal's MD5."
  (equal (org-x-dag-get-md5 path) md5))

(defun org-x-dag-file-is-dirty (file md5)
  "Return t if FILE with MD5 has been recently changed."
  (or (org-x-with-file file (buffer-modified-p))
      (not (org-x-dag-md5-matches-p file md5))))

(defun org-x-dag-set-sync-state ()
  "Set the sync state to reflect the current files on disk."
  (->> (org-x-dag-get-files)
       (--map (cons it (org-x-dag-get-md5 it)))
       (setq org-x-dag-sync-state)))

(defun org-x-dag-get-sync-state ()
  "Return the sync state.

The returned value will be a list like (TO-REMOVE TO-INSERT
TO-UPDATE) which will contain the file paths the should be
removed from, added to, or edited within the DAG respectively."
  (cl-flet
      ((lookup-md5
        (path)
        (alist-get path org-x-dag-sync-state nil nil #'equal)))
    (-let* ((existing-files (org-x-dag-get-files))
            (state-files (-map #'car org-x-dag-sync-state))
            (to-remove (-difference state-files existing-files))
            (to-insert (-difference existing-files state-files))
            (to-update
             (->> (-intersection existing-files state-files)
                  (--filter (org-x-dag-file-is-dirty it (lookup-md5 it))))))
      ;; (print (list to-remove to-insert to-update))
      (list to-remove to-insert to-update))))

;; TODO this assumes the `org-id-locations' is synced
(defun org-x-dag-get-buffer-nodes (file kws)
  "Return a list of nodes from FILE.

A node will only be returned if the headline to which it points
has a valid (meaning in KWS) keyword and either its parent has a
valid keyword or none of its parents have valid keywords."
  (let ((more t)
        cur-path this-point this-key this-level this-todo has-todo this-parent
        tags acc)
    ;; TODO add org-mode sanity check
    (goto-char (point-min))
    ;; move forward until on a headline
    (while (and (not (= ?* (following-char))) (= 0 (forward-line 1))))
    ;; Build alist; Keep track of how 'deep' we are in a given org-tree using a
    ;; stack. The stack will have members like (LEVEL KEY) where LEVEL is the
    ;; level of the headline and KEY is the node key if it has a keyword. Only
    ;; add a node to the accumulator if it has a keyword, and only include its
    ;; parent headline if the parent also has a keyword (add the link targets
    ;; regardless).
    (while more
      (when (= ?* (following-char))
        (setq this-point (point)
              this-key nil)
        ;; Get tags (must be done from the first column)
        (setq this-tags (org--get-local-tags))
        ;; Get the level
        (while (= ?* (following-char)) (forward-char 1))
        (setq this-level (current-column))
        ;; Check if the headline has a keyword
        (forward-char 1)
        (while (not (memq (following-char) '(?  ?\n))) (forward-char 1))
        (setq this-todo (-> (+ 1 this-point this-level)
                            (buffer-substring (+ this-point (current-column))))
              has-todo (member this-todo kws))
        ;; Adjust the stack so that the top headline is the parent of the
        ;; current headline
        (while (and cur-path (<= this-level (nth 0 (car cur-path))))
          (!cdr cur-path))
        (setq this-parent (car cur-path))
        ;; Add the current headline to accumulator if it has a keyword, but only
        ;; if its parent has a keyword or none of its parents have keywords
        (when (and has-todo (or (nth 1 this-parent)
                                 (--none-p (nth 1 it) cur-path)))
          ;; If parent is not a todo and we want tag inheritance, store all tags
          ;; above this headline (sans file-tags which we can get later easily)
          (setq tags (if (and (not (nth 1 this-parent)) org-use-tag-inheritance)
                         (->> cur-path
                              (--mapcat (nth 2 it))
                              (append this-tags))
                       this-tags)
                this-key (org-x-dag-build-key file
                                              this-point
                                              this-level
                                              (substring-no-properties this-todo)
                                              tags
                                              (org-entry-get nil "ID")))
          ;; TODO also get a list of link parent targets and add them to the
          ;; parent list
          (!cons (cons this-key (-some-> (nth 1 this-parent) (list))) acc))
        ;; Add current headline to stack
        ;; (when (and (s-contains-p "general" file) (not (nth 1 this-parent)))
        ;;   (print (--map (nth 2 it) cur-path)))
          ;; (print (list cur-path this-tags)))
        (!cons (list this-level this-key this-tags) cur-path))
      (setq more (= 0 (forward-line 1))))
    (nreverse acc)))

(defun org-x-dag-get-file-nodes (file)
  "Return all nodes in FILE in one pass."
  (org-x-with-file file
    (org-x-dag-get-buffer-nodes file org-todo-keywords-1)))

;; (defun org-x-dag-key-is-pseudo-marker (key)
;;   "Return t if KEY is a pseudo marker."
;;   (eq (car key) :pm))
;;   ;; (= 2 (length key)))
;;   ;; (and (consp key) (stringp (car key)) (numberp (cdr key))))

;; (defun org-x-dag-key-is-id (key)
;;   "Return t if KEY is an ID."
;;   ;; (= 3 (length key)))
;;   (eq (car key) :id))

(defun org-x-dag-files-contains-key-p (key files)
  "Return t if KEY represents a node contained in FILES."
  (-if-let (other-file (org-x-dag-key-get-file key))
      (--any-p (equal other-file it) files)
    (error "Invalid key: %s" key)))
  ;; (cl-flet
  ;;     ((contains-key
  ;;       (files other-file)
  ;;       (--any-p (equal other-file it) files)))
  ;;   (cond
  ;;    ((org-x-dag-key-is-id key)
  ;;     (-some->> (ht-get org-id-locations key)
  ;;       (contains-key files)))
  ;;    ((org-x-dag-key-is-pseudo-marker key)
  ;;     (contains-key files (car key)))
  ;;    (t
  ;;     (error "Invalid key: %s" key)))))

(defun org-x-dag-get-nodes-in-files (dag files)
  (let ((x (->> (dag-get-nodes-and-edges-where org-x-dag
                  (org-x-dag-files-contains-key-p it files))
                (-map #'car)))
        (y (dag-get-floating-nodes-where org-x-dag
             (org-x-dag-files-contains-key-p it files))))
    ;; (print (list x y))
    ;; (print x)
    ;; (print (list (length x) (length y) (length (-intersection x y))))
    (append x y)))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG."
  (let* ((files-to-insert (append to-update to-insert))
         (nodes-to-insert (-mapcat #'org-x-dag-get-file-nodes files-to-insert)))
    (if org-x-dag
        (let* ((files-to-remove (append to-update to-remove))
               (keys-to-remove (->> (org-x-dag-get-nodes-in-files
                                     org-x-dag files-to-remove))))
          (when (or keys-to-remove nodes-to-insert)
            (setq org-x-dag (dag-edit-nodes keys-to-remove
                                            nodes-to-insert
                                            org-x-dag))))
      (setq org-x-dag (dag-alist-to-dag nodes-to-insert)))))

(defun org-x-dag-sync (&optional force)
  "Sync the DAG with files from `org-x-dag-get-files'.

If FORCE is non-nil, sync no matter what."
  (when force
    (setq org-x-dag-sync-state nil
          org-x-dag nil))
  (-let (((to-remove to-insert to-update) (org-x-dag-get-sync-state)))
    (org-x-dag-update to-remove to-insert to-update)
    (org-x-dag-set-sync-state)
    nil))

;;; DAG -> HEADLINE RETRIEVAL

(defun org-x-dag-relation-has-parent-headlines-p (key relation)
  ""
  (let ((this-file (org-x-dag-key-get-file key)))
    (->> (dag-relation-get-parents relation)
         (--any-p (equal this-file (org-x-dag-key-get-file it))))))

(defun org-x-dag-relation-has-child-headlines-p (key relation)
  ""
  (let ((this-file (org-x-dag-key-get-file key)))
    (->> (dag-relation-get-children relation)
         (--any-p (equal this-file (org-x-dag-key-get-file it))))))

(defun org-x-dag-get-standalone-task-nodes (dag)
  "Return the standalone task nodes of DAG."
  (let* ((action-files (org-x-get-action-files))
         (from-adjlist
          (dag-get-nodes-and-edges-where dag
            (and (org-x-dag-files-contains-key-p it action-files)
                 (not (org-x-dag-relation-has-parent-headlines-p it it-rel))
                 (not (org-x-dag-relation-has-child-headlines-p it it-rel)))))
         (from-floating
          (dag-get-floating-nodes-where dag
            (org-x-dag-files-contains-key-p it action-files))))
    (append (-map #'car from-adjlist) from-floating)))

(defun org-x-dag-get-toplevel-project-nodes (dag)
  "Return the toplevel project nodes of DAG."
  (let ((action-files (org-x-get-action-files)))
    (dag-get-nodes-and-edges-where dag
      (and (org-x-dag-files-contains-key-p it action-files)
           (not (org-x-dag-relation-has-parent-headlines-p it it-rel))
           (org-x-dag-relation-has-child-headlines-p it it-rel)))))

;;; DAG -> HEADLINE RETRIEVAL (CHILD/PARENT)

(defun org-x-dag-filter-children (dag key fun)
  (declare (indent 2))
  (-filter fun (dag-get-children key dag)))

(defun org-x-dag-separate-children (dag key fun)
  (declare (indent 2))
  (-separate fun (dag-get-children key dag)))

(defun org-x-dag-node-get-headline-children (dag key)
  (let ((this-file (org-x-dag-key-get-file key)))
    (org-x-dag-filter-children dag key
      (lambda (it) (equal this-file (org-x-dag-key-get-file it))))))

;; TODO somewhere in here I need to filter based on headline like CANC
(defun org-x-dag-project-node-get-task-nodes (dag key)
  (declare (indent 2))
  ;; NOTE if this is a standalone task it will return itself
  (-if-let (cs (org-x-dag-node-get-headline-children dag key))
      ;; TODO don't hardcode this
      (->> (--remove (member (plist-get it :todo) (list org-x-kw-canc org-x-kw-hold)) cs)
           (--mapcat (org-x-dag-project-node-get-task-nodes dag it)))
    (list key)))

(defun org-x-dag-get-project-task-nodes (fun dag)
  "Return project task nodes of DAG."
  (-let (((&plist :adjlist) dag))
    (->> (org-x-dag-get-toplevel-project-nodes dag)
         (-map #'car)
         (-remove fun)
         (--mapcat (org-x-dag-project-node-get-task-nodes dag it)))))

(defun org-x-dag-project-node-get-subproject-nodes (dag key)
  (-when-let (cs (org-x-dag-node-get-headline-children dag key))
    (cons key (--mapcat (org-x-dag-project-node-get-subproject-nodes dag it) cs))))

(defun org-x-dag-get-subproject-task-nodes (dag)
  "Return subproject nodes of DAG."
  ;; ignore floating nodes since these by definition can't be part of projects
  (-let (((&plist :adjlist) dag))
    (->> (org-x-dag-get-toplevel-project-nodes dag)
         (-map #'car)
         (--mapcat (org-x-dag-project-node-get-subproject-nodes dag it)))))

;; (defmacro org-x-dag-with-key (key &rest body)
;;   (declare (indent 1))
;;   `(cond
;;     ((org-x-dag-key-is-pseudo-marker ,key)
;;      (org-x-with-file (car ,key)
;;        (goto-char (cdr ,key))
;;        ,@body))
;;     ((org-x-dag-key-is-id ,key)
;;      (org-x-with-id-target ,key
;;        ,@body))))

;; NODE FORMATTING

(defun org-x-dag-get-headline-with-props (pos type face)
  (goto-char pos)
  (let* ((head (org-get-heading))
         (level (-> (org-outline-level)
                    (org-reduced-level)
                    (1-)
                    (make-string ?.)))
         (category (org-get-category))
         (todo-state (org-get-todo-state))
         (inherited-tags
          (or (eq org-agenda-show-inherited-tags 'always)
              (and (listp org-agenda-show-inherited-tags)
                   (memq 'agenda org-agenda-show-inherited-tags))
              (and (eq org-agenda-show-inherited-tags t)
                   (or (eq org-agenda-use-tag-inheritance t)
                       (memq 'agenda
                             org-agenda-use-tag-inheritance)))))
         (tags (org-get-tags nil (not inherited-tags)))
         (item (org-agenda-format-item "" head level category tags nil nil nil))
         (marker (org-agenda-new-marker pos)))
    (org-add-props item nil
      'org-marker marker
      'org-hd-marker marker
      'org-not-done-regexp org-not-done-regexp
      'org-todo-regexp org-todo-regexp
      'org-complex-heading-regexp org-complex-heading-regexp
      'mouse-face 'highlight
      'help-echo (format "mouse-2 or RET jump to Org file %s"
                         (abbreviate-file-name buffer-file-name))
      'undone-face face
      ;; TODO in the case of scheduled headline this has other stuff in it
      'priority (org-get-priority item)
      'todo-state todo-state
      'face face
      'type type)))

(defun org-x-dag-nodes-to-headlines (nodes)
    (->> (-group-by #'org-x-dag-key-get-file nodes)
         (--map (-let (((path . nodes) it))
                  (org-x-with-file path
                    (->> (-map #'org-x-dag-key-get-point nodes)
                         (--map (progn (goto-char it)
                                       (substring-no-properties (org-get-heading))))))))
                    ;; (->> (-map #'org-x-dag-key-get-point nodes)
                    ;;      (-map #'org-x-dag-get-headline-with-props)))))
         (-flatten-n 1)))

(defun org-x-dag-collapse-tags (tags)
  "Return TAGS with duplicates removed.

In the case of mutually exclusive tags, only the first tag
encountered will be returned."
  (-let (((x non-x) (--separate (memq (elt it 0) org-x-exclusive-prefixes) tags)))
    (->> (--group-by (elt it 0) x)
         (--map (car (cdr it)) )
         (append (-uniq non-x)))))

(defun org-x-dag-add-default-props (item)
  (org-add-props item nil
    'org-not-done-regexp org-not-done-regexp
    'org-todo-regexp org-todo-regexp
    'org-complex-heading-regexp org-complex-heading-regexp
    'mouse-face 'default))

(defun org-x-dag-format-tag-node (category tags key)
  ;; ASSUME I don't use subtree-level categories
  (-let* (;; (category (org-get-category))
          (head (org-get-heading))
          (level (-> (plist-get key :level)
                     (make-string ?s)))
          ;; no idea what this does...
          (help-echo (format "mouse-2 or RET jump to Org file %S"
                             (abbreviate-file-name
                              (or (buffer-file-name (buffer-base-buffer))
                                  (buffer-name (buffer-base-buffer))))))
          (marker (org-agenda-new-marker))
          ;; no idea what this function actually does
          ((ts . ts-type) (org-agenda-entry-get-agenda-timestamp (point)))
          (item (org-agenda-format-item "" head level category tags))
          (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item)
        (org-add-props nil
            ;; face
            'face 'default
            'done-face 'org-agenda-done
            'undone-face 'default
            ;; marker
            'org-hd-marker marker
            'org-marker marker
            ;; headline stuff
            'todo-state (plist-get key :todo)
            'priority priority
            'ts-date ts
            ;; misc
            'type (concat "tagsmatch" ts-type)
            'help-echo help-echo))))

(defun org-x-dag-key-is-iterator (key)
  (org-x-with-file (org-x-dag-key-get-file key)
    (->> (org-entry-get (org-x-dag-key-get-point key) org-x-prop-parent-type)
         (equal org-x-prop-parent-type-iterator))))

;; (defmacro org-x-dag-do-file-nodes (path keys form)
;;   (declare (indent 2))
;;   `(let ((acc))
;;     (org-x-with-file ,path
;;       ;; ;; TODO tbh this could just be the file basename since that's all
;;       ;; ;; I ever use
;;       ;; (let ((it-category (org-get-category)))
;;       (--each keys
;;         (goto-char (org-x-dag-key-get-point it))
;;         ,form))
;;     (nreverse acc)))

(defun org-x-headline-has-timestamp (re want-time)
  (let ((end (save-excursion (outline-next-heading))))
    (-when-let (p (save-excursion (re-search-forward re end t)))
      (if want-time (org-2ft (match-string 1)) p))))

(defun org-x-headline-is-deadlined (want-time)
  (org-x-headline-has-timestamp org-deadline-time-regexp want-time))

(defun org-x-headline-is-scheduled (want-time)
  (org-x-headline-has-timestamp org-scheduled-time-regexp want-time))

(defun org-x-headline-is-closed (want-time)
  (org-x-headline-has-timestamp org-closed-time-regexp want-time))

(defconst org-x-headline-task-status-priorities
  '((:archivable . -1)
    (:complete . -1)
    (:expired . 0)
    (:done-unclosed . 0)
    (:undone-closed . 0)
    (:active . 1)
    (:inert . 2)))

(defun org-x-headline-get-task-status-0 (kw)
  (if (member kw org-x-done-keywords)
      (-if-let (c (org-x-headline-is-closed t))
          (if (< (- (float-time) c) (* 60 60 24 org-x-archive-delay))
              :archivable
            :complete)
        :done-unclosed)
    (cond
     ((org-x-headline-is-expired-p) :expired)
     ((org-x-headline-is-inert-p) :inert)
     ((org-x-headline-is-closed nil) :undone-closed)
     (t :active))))

;; TODO making this an imperative-style loop doesn't speed it up 'that-much'
(defun org-x-dag-scan-tasks ()
  (let* ((dag org-x-dag)
         (sats (->> (org-x-dag-get-standalone-task-nodes dag)
                    (--map (cons it :is-standalone))))
         (pts (->> (org-x-dag-get-project-task-nodes #'org-x-dag-key-is-iterator dag)
                   (--map (list it))))
         (grouped (->> (append sats pts)
                       (--group-by (org-x-dag-key-get-file (car it)))))
         acc path key-cells category key tags is-standalone)
    (--each grouped
      ;; (-let (((path . key-cells) it))
      (-setq (path . key-cells) it)
      ;; TODO this won't add the file to `org-agenda-new-buffers'
      (org-x-with-file path
        ;; TODO tbh this could just be the file basename since that's all
        ;; I ever use
        (setq category (org-get-category))
        ;; (let ((category (org-get-category)))
        (--each key-cells
          (-setq (key . is-standalone) it)
          (setq tags (->> (org-x-dag-get-inherited-tags org-file-tags dag key)
                          (append (plist-get key :tags))
                          (org-x-dag-collapse-tags)))
          ;; (-let* (((key . is-standalone) it)
                  ;; (tags (->> (org-x-dag-get-inherited-tags org-file-tags dag key)
                  ;;            (append (plist-get key :tags))
                  ;;            (org-x-dag-collapse-tags))))
            ;; filter out incubators
          (goto-char (plist-get key :point))
          (unless (or (member org-x-tag-incubated tags)
                      (org-x-headline-is-scheduled nil)
                      (org-x-headline-is-deadlined nil))
            (let* ((s (org-x-headline-get-task-status-0 (plist-get key :todo)))
                   (p (alist-get s org-x-headline-task-status-priorities)))
              (unless (= p -1)
                (setq acc (-> (org-x-dag-format-tag-node category tags key)
                              (org-add-props nil
                                  'x-is-standalone is-standalone
                                  'x-status s)
                              (cons acc)))))))))
    acc))

(defun org-x-dag-scan-tags ()
  (let* ((dag org-x-dag)
         (nodes (org-x-dag-get-toplevel-project-nodes dag)))
    (->> (--group-by (org-x-dag-key-get-file (car it)) nodes)
         (--mapcat
          (-let (((path . nodes) it))
            (org-x-with-file path
              (->> (-map #'car nodes)
                   (--mapcat
                    (progn
                      (goto-char (org-x-dag-key-get-point it))
                      (org-x-dag-format-tag-node dag (org-get-tags (point)) it))))))))))

(defun org-x-dag-get-inherited-tags (init dag key)
  (let* ((this-file (org-x-dag-key-get-file key)))
    (cl-labels
        ((ascend
          (k tags)
          (-if-let (parent (->> (dag-get-parents k dag)
                                (--first (equal (org-x-dag-key-get-file it)
                                                this-file))))
              (->> (plist-get parent :tags)
                   (append tags)
                   (ascend parent))
            tags)))
      (org-x-dag-collapse-tags (append (ascend key nil) init)))))

;;; AGENDA VIEWS

(defun org-x-dag-get-day-entries (_ date &rest args)
  "Like `org-agenda-get-day-entries' but better."
  ;; for now just return a list of standalone tasks
  (->> (org-x-dag-get-standalone-task-nodes org-x-dag)
       (org-x-dag-nodes-to-headlines)))

(defun org-x-dag-agenda-list ()
  (let ((org-agenda-files (org-x-get-action-files)))
    (nd/with-advice
        (('org-agenda-get-day-entries :override #'org-x-dag-get-day-entries))
      (org-agenda-list))))

(defun org-x-dag-tags-view (_match)
  (org-x-dag-sync t)
  (let ((org-agenda-files (org-x-get-action-files)))
    (nd/with-advice
        (('org-scan-tags :override (lambda (&rest _) (org-x-dag-scan-tags))))
      (org-tags-view '(4) "TODO"))))

(defun org-x-dag-show-tasks (_match)
  (org-x-dag-sync t)
  ;; hack to make the loop only run once
  (let ((org-agenda-files (list (car (org-x-get-action-files)))))
    (nd/with-advice
        (('org-scan-tags :override (lambda (&rest _) (org-x-dag-scan-tasks))))
      (org-tags-view '(4) "TODO"))))

(defun org-x-dag-show-nodes (get-nodes)
  (org-x-dag-sync)
  (let* ((org-tags-match-list-sublevels org-tags-match-list-sublevels)
         (completion-ignore-case t)
         rtnall files file pos matcher
         buffer)
    (catch 'exit
      (org-agenda-prepare (concat "DAG-TAG"))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (let ((org-agenda-redo-command `(org-x-dag-show-nodes ',get-nodes))
            (rtnall (funcall get-nodes)))
        (org-agenda--insert-overriding-header
          (with-temp-buffer
            (insert "Headlines with TAGS match: \n")
            (add-text-properties (point-min) (1- (point))
                                 (list 'face 'org-agenda-structure))
            (buffer-string)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties
         (point-min) (point-max)
         `(org-agenda-type tags
                           org-last-args (,get-nodes)
                           org-redo-cmd ,org-agenda-redo-command
                           org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

(provide 'org-x-dag)
;;; org-x-dag.el ends here
