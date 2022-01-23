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

(defun org-x-dag-create (d m f)
  (list :dag d :id->meta m :file->ids f))

(defun org-x-dag-empty ()
  (org-x-dag-create (dag-empty) (ht-create #'equal) (ht-create #'equal)))

(defvar org-x-dag (org-x-dag-empty)
                        
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

;; (defun org-x-dag-build-key (file point level todo tags toplevelp id)
(defun org-x-dag-build-key (file point level todo tags toplevelp)
  (list :file file
        :point point
        :level level
        :todo todo
        :tags tags
        :toplevelp toplevelp))
        ;; :id id))
  ;; (if id (list :id file point id) (list :pm file point)))

(defun org-x-dag-build-meta (file point level todo tags toplevelp)
  (list :file file
        :point point
        :level level
        :todo todo
        :tags tags
        :toplevelp toplevelp))

(defun org-x-dag-key-get-file (key)
  "Return file for KEY."
  (org-x-dag-id-lookup-prop key :file))
  ;; (plist-get key :file))
  ;; (nth 1 key))

(defun org-x-dag-key-get-point (key)
  "Return point for KEY."
  (org-x-dag-id-lookup-prop key :point))
  ;; (plist-get key :point))
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
  (org-x-with-file path (buffer-hash)))
  ;; (with-temp-buffer
  ;;   (let ((rc (call-process "md5sum" nil (current-buffer) nil path)))
  ;;     (if (/= 0 rc) (error "Could not get md5 of %s" path)
  ;;       (->> (buffer-string)
  ;;            (s-match "^\\([0-9a-z]+\\)")
  ;;            (cadr))))))

(defun org-x-dag-md5-matches-p (path md5)
  "Return t if the md5 of PATH on disk `equal's MD5."
  (equal (org-x-dag-get-md5 path) md5))

(defun org-x-dag-file-is-dirty (file md5)
  "Return t if FILE with MD5 has been recently changed."
  (org-x-with-file file
    (let ((new-md5 (buffer-hash)))
      (unless (equal new-md5 md5)
        new-md5))))

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
            ((&alist 'to-insert 'to-update 'no-change)
             (->> (--map (cons it (org-x-dag-get-md5 it)) existing-files)
                  (--group-by (-let* (((path . new-md5) it)
                                      (old-md5 (lookup-md5 path)))
                                (cond
                                 ((null old-md5) 'to-insert)
                                 ((equal old-md5 new-md5) 'no-change)
                                 (t 'to-update)))))))
      (list to-remove to-insert to-update no-change))))

(defun org-x-dag-get-local-property (prop)
  (car (org--property-local-values prop nil)))

(defun org-x-dag-get-link-property ()
  (-some->> (org-x-dag-get-local-property org-x-prop-goal)
    (s-split ";")
    (--map (->> (s-trim it)
                (s-match "^\\[\\[id:\\(.*\\)\\]\\[.*\\]\\]$")
                (cadr)))))

;; (defun org-x-dag-link-buffer-nodes (id-cache raw)
;;   (let (this this-key these-links these-parents this-link link-target broken acc)
;;     (while raw
;;       (setq this (car raw)
;;             this-key (plist-get this :key)
;;             these-links (plist-get this :links)
;;             these-parents (plist-get this :parents))
;;       (while these-links
;;         (setq this-link (car these-links))
;;         (if (setq link-target (ht-get id-cache this-link))
;;             (!cons link-target these-parents)
;;           (!cons (list :key this-key :link this-link) broken))
;;         (!cdr these-links))
;;       (!cons (cons this-key these-parents) acc)
;;       (!cdr raw))
;;     ;; TODO this warning message won't be all the helpful seeing as the keys
;;     ;; are actually semi-obscure plists with extra information and such
;;     (--each broken
;;       (-let (((&plist :key :link) it))
;;         (message "WARNING: broken link for key %S: %s" key link)))
;;     acc))

;; TODO this assumes the `org-id-locations' is synced
(defun org-x-dag-get-buffer-nodes (file kws)
  "Return a list of nodes from FILE.

A node will only be returned if the headline to which it points
has a valid (meaning in KWS) keyword and either its parent has a
valid keyword or none of its parents have valid keywords."
  (let ((more t)
        ;; (id-cache (ht-create #'equal))
        cur-path this-point this-key this-level this-todo has-todo this-parent
        tags toplevelp this-file-links acc acc-meta)
    ;; TODO add org-mode sanity check
    (goto-char (point-min))
    ;; If not on a headline, check for a property drawer with links in it
    (unless (= ?* (following-char))
      (setq this-file-links (org-x-dag-get-link-property)))
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
        (setq this-parent (car cur-path)
              toplevelp (not (nth 1 this-parent)))
        ;; Add the current headline to accumulator if it has a keyword, but only
        ;; if its parent has a keyword or none of its parents have keywords
        (when (and has-todo (or (not toplevelp) (--none-p (nth 1 it) cur-path))
                   (setq this-key (org-x-dag-get-local-property "ID")))
          ;; If parent is not a todo and we want tag inheritance, store all tags
          ;; above this headline (sans file-tags which we can get later easily)
          (setq tags (if (and toplevelp org-use-tag-inheritance)
                         (->> cur-path
                              (--mapcat (nth 2 it))
                              (append this-tags))
                       this-tags)
                this-meta (org-x-dag-build-meta file
                                                this-point
                                                this-level
                                                (substring-no-properties this-todo)
                                                tags
                                                toplevelp))
          (!cons (cons this-key this-meta) acc-meta)
          (!cons (cons this-key (-some-> (nth 1 this-parent) (list))) acc))
        ;; Add current headline to stack
        (!cons (list this-level this-key this-tags) cur-path))
      (setq more (= 0 (forward-line 1))))
    ;; TODO reverse these to make things make sense later?
    (list acc acc-meta)))

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
    (append x y)))

(defun org-x-dag-get-data-to-insert (files)
  ;; TODO could also make another data structure to link each id to a file
  ;; since this is a very common lookup operation
  (cl-flet
      ((append-results
        (acc file)
        (-let* (((acc-ids acc-meta acc-filemaps) acc)
                ((ids metas) (org-x-dag-get-file-nodes file))
                (filemap (cons file (-map #'car ids))))
          (list (append ids acc-ids)
                (append metas acc-meta)
                (cons filemap acc-filemaps)))))
    (-reduce-from #'append-results nil files)))

(defun org-x-dag-update-ht (to-remove to-insert ht)
  (--each to-remove
    (ht-remove ht it))
  (--each to-insert
    (ht-set ht (car it) (cdr it)))
  ht)

(defun org-x-dag-get-ids-in-files (files)
  (let ((filemap (plist-get org-x-dag :file->ids)))
    (--mapcat (ht-get filemap it) files)))

(defun org-x-dag->adjacency-list ()
  (-> (plist-get org-x-dag :dag) (dag-get-adjacency-list)))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG."
  (-let* (((&plist :dag :id->meta :file->ids) org-x-dag)
          (files2rem (append to-update to-remove))
          (files2ins (append to-update to-insert))
          (ids2rem (org-x-dag-get-ids-in-files files2rem))
          ((ids2ins meta2ins fms2ins) (org-x-dag-get-data-to-insert files2ins)))
    (org-x-dag-update-ht ids2rem meta2ins id->meta)
    (org-x-dag-update-ht files2rem fms2ins file->ids)
    (->> (if (dag-is-empty-p dag) (dag-alist-to-dag ids2ins)
           (dag-edit-nodes ids2rem ids2ins dag))
         (plist-put org-x-dag :dag))))

(defun org-x-dag-id-lookup-prop (id prop)
  (-> (plist-get org-x-dag :id->meta)
      (ht-get id)
      (plist-get prop)))

(defun org-x-dag-sync (&optional force)
  "Sync the DAG with files from `org-x-dag-get-files'.

If FORCE is non-nil, sync no matter what."
  (when force
    (setq org-x-dag-sync-state nil
          org-x-dag (org-x-dag-empty)))
  ;; TODO verify integrity somewhere in here
  (-let (((to-remove to-insert to-update no-change) (org-x-dag-get-sync-state)))
    (org-x-dag-update to-remove (-map #'car to-insert) (-map #'car to-update))
    (setq org-x-dag-sync-state (append to-insert to-update no-change))
    nil))

;;; DAG -> HEADLINE RETRIEVAL

;; ;; TODO this is silly since there can only be one parent, this function may
;; ;; be doing too much
;; (defun org-x-dag-relation-has-parent-headlines-p (key relation)
;;   ""
;;   (let ((this-file (org-x-dag-key-get-file key)))
;;     (->> (dag-relation-get-parents relation)
;;          (--any-p (equal this-file (org-x-dag-key-get-file it))))))

(defun org-x-dag-relation-has-child-headlines-p (key relation)
  ""
  (let ((this-file (org-x-dag-key-get-file key)))
    (->> (dag-relation-get-children relation)
         (--any-p (equal this-file (org-x-dag-key-get-file it))))))

;; (defun org-x-dag-key-has-child-headlines-p (key dag)
;;   (org-x-dag-relation-has-child-headlines-p key (dag-get-relationships key dag)))

(defun org-x-dag-partition-task-nodes (files)
  (->> (org-x-dag-get-ids-in-files files)
       (--separate (org-x-dag-id-lookup-prop it :toplevelp))))
  ;; (-let (((from-adjlist-proj from-adjlist-task)
  ;;         (->> (dag-get-nodes-and-edges-where dag
  ;;                (and (org-x-dag-files-contains-key-p it files)
  ;;                     (plist-get it :toplevelp)))
  ;;              (--separate (org-x-dag-relation-has-child-headlines-p (car it) (cdr it))))))
  ;;   (list (-map #'car from-adjlist-task) (-map #'car from-adjlist-proj))))

(defun org-x-dag-partition-all-task-nodes (files dag)
  (-let (((from-adjlist-proj from-adjlist-task)
          (org-x-dag-partition-task-nodes files dag))
         (from-floating
          (dag-get-floating-nodes-where dag
            (org-x-dag-files-contains-key-p it files))))
    (list (append from-adjlist-task from-floating) from-adjlist-proj)))

(defun org-x-dag-get-standalone-task-nodes (dag)
  "Return the standalone task nodes of DAG."
  (let* ((action-files (org-x-get-action-files))
         (from-adjlist
          (dag-get-nodes-and-edges-where dag
            (and (org-x-dag-files-contains-key-p it action-files)
                 (plist-get it :toplevelp)
                 (not (org-x-dag-relation-has-child-headlines-p it it-rel)))))
         (from-floating
          (dag-get-floating-nodes-where dag
            (org-x-dag-files-contains-key-p it action-files))))
    (append (-map #'car from-adjlist) from-floating)))

(defun org-x-dag-get-toplevel-project-nodes ()
  "Return the toplevel project nodes of DAG."
  (let ((action-files (org-x-get-action-files)))
    (dag-get-nodes-and-edges-where (plist-get org-x-dag :dag)
      (and (org-x-dag-files-contains-key-p it action-files)
           (org-x-dag-id-lookup-prop it :toplevelp)
           ;; (plist-get it :toplevelp)
           (org-x-dag-relation-has-child-headlines-p it it-rel)))))

;;; DAG -> HEADLINE RETRIEVAL (CHILD/PARENT)

(defun org-x-dag-filter-children (dag key fun)
  (declare (indent 2))
  (-filter fun (dag-get-children key (plist-get org-x-dag :dag))))

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
      (->> (--remove (member (org-x-dag-id-lookup-prop it :todo) (list org-x-kw-canc org-x-kw-hold)) cs)
           (--mapcat (org-x-dag-project-node-get-task-nodes dag it)))
    (list key)))

(defun org-x-dag-get-project-task-nodes (fun dag)
  "Return project task nodes of DAG."
  (-let (((&plist :adjlist) dag))
    (->> (org-x-dag-get-toplevel-project-nodes)
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
          (level (-> (org-x-dag-id-lookup-prop key :level)
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
            'todo-state (org-x-dag-id-lookup-prop key :todo)
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

(defconst org-x-project-status-priorities
  '((:archivable . -1)
    (:complete . -1)
    (:scheduled-project . 0)
    (:invalid-todostate . 0)
    (:undone-complete . 0)
    (:done-incomplete . 0)
    (:stuck . 0)
    (:wait . 1)
    (:held . 2)
    (:active . 3)
    (:inert . 4)))

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

;; (defmacro org-x--descend-into-project (dag key children statuscode-tree get-task-status callback-form)
;;   ;; define "breaker-status" as the last of the allowed-statuscodes
;;   ;; when this is encountered the loop is broken because we are done
;;   ;; (the last entry trumps all others)
;;   (declare (indent 3))
;;   (let* ((allowed-statuscodes (-map #'car statuscode-tree))
;;          (trans-tbl (->> statuscode-tree
;;                          (--map (-let (((a . bs) it)) (--map (cons it a) bs)))
;;                          (-flatten-n 1)))
;;          (breaker-status (-last-item allowed-statuscodes))
;;          (initial-status (car allowed-statuscodes)))
;;     `(save-excursion
;;        (let ((project-status ,initial-status)
;;              (this-child nil)
;;              (it-kw nil)
;;              (new-status nil))
;;          ;; loop through tasks one level down until breaker-status found
;;          (while (and children (not (eq project-status ,breaker-status)))
;;            (setq this-child (car children)
;;                  it-kw (plist-get this-child :todo))
;;            ;; If project returns an allowed status then use that. Otherwise look
;;            ;; up the value in the translation table and return error if not
;;            ;; found.
;;            (-if-let (cs (org-x-dag-node-get-headline-children dag this-child))
;;                (unless (member (setq new-status
;;                                      (funcall ,callback-form
;;                                               ,dag this-child cs))
;;                                ',allowed-statuscodes)
;;                  (setq new-status (alist-get new-status ',trans-tbl)))
;;              (goto-char (org-x-dag-key-get-point this-child))
;;              (setq new-status (nth ,get-task-status ',allowed-statuscodes)))
;;            (when (org-x--compare-statuscodes ',allowed-statuscodes
;;                    new-status > project-status)
;;              (setq project-status new-status))
;;            (!cdr children))
;;          project-status))))

(defun org-x-dag-get-max-index (ys xs)
  "Return the member of XS that has the highest index in YS."
  (--max-by (> (-elem-index it ys) (-elem-index other ys)) xs))

(defmacro org-x-dag-descend-into-project (dag keys parent-tags codetree
                                              task-form callback)
  (declare (indent 3))
  (let ((allowed-codes (-map #'car codetree))
        (trans-tbl (--mapcat (-let (((a . bs) it))
                               (--map (cons it a) bs))
                             codetree)))
    `(cl-flet
         ((get-project-or-task-status
           (key)
           (-if-let (children (org-x-dag-node-get-headline-children ,dag key))
               (let* ((tags (-> (org-x-dag-id-lookup-prop key :tags)
                                (append ,parent-tags)
                                (org-x-dag-collapse-tags)))
                      (child-results (funcall ,callback ,dag key tags children))
                      ;; ASSUME the car of the results will be the toplevel
                      ;; key/status pair for this (sub)project
                      (top-status (plist-get (car child-results) :status))
                      (top-status* (if (member top-status ',allowed-codes)
                                       top-status
                                     (alist-get top-status ',trans-tbl))))
                 (cons top-status* child-results))
             (let ((it-kw (org-x-dag-id-lookup-prop key :todo)))
               (org-x-dag-with-key key
                 (-> ,task-form
                     (nth ',allowed-codes)
                     (list)))))))
       (let* ((results (-map #'get-project-or-task-status ,keys))
              (status (->> (-map #'car results)
                           (org-x-dag-get-max-index ',allowed-codes))))
         (cons status (-mapcat #'cdr results))))))

(defun org-x-dag-headline-get-project-status (dag key tags children)
  ;; ASSUME children will always be at least 1 long
  (goto-char (org-x-dag-key-get-point key))
  (let ((keyword (org-x-dag-id-lookup-prop key :todo)))
    (-let (((status . child-results)
            (cond
             ((org-x-headline-is-scheduled nil)
              (list :scheduled-project))
             ((equal keyword org-x-kw-hold)
              (list (if (org-x-headline-is-inert-p) :inert :held)))
             ((member keyword org-x--project-invalid-todostates)
              (list :invalid-todostate))
             ((equal keyword org-x-kw-canc)
              (list (if (org-x-headline-is-archivable-p) :archivable :complete)))
             ((equal keyword org-x-kw-done)
              (org-x-dag-descend-into-project dag children tags
                ((:archivable)
                 (:complete)
                 (:done-incomplete :stuck :inert :held :wait :active
                                   :scheduled-project :invalid-todostate
                                   :undone-complete))
                (if (member it-kw org-x-done-keywords)
                    (if (org-x-headline-is-archivable-p) 0 1)
                  2)
                #'org-x-dag-headline-get-project-status))
             ((equal keyword org-x-kw-todo)
              (org-x-dag-descend-into-project dag children tags
                ((:undone-complete :complete :archivable)
                 (:stuck :scheduled-project :invalid-todostate :done-incomplete)
                 (:held)
                 (:wait)
                 (:inert)
                 (:active))
                (cond
                 ((and (not (member it-kw org-x-done-keywords))
                       (org-x-headline-is-inert-p))
                  4)
                 ((equal it-kw org-x-kw-todo)
                  (if (org-x-headline-is-scheduled nil) 5 1))
                 ((equal it-kw org-x-kw-hold)
                  2)
                 ((equal it-kw org-x-kw-wait)
                  3)
                 ((equal it-kw org-x-kw-next)
                  5)
                 (t 0))
                #'org-x-dag-headline-get-project-status))
             (t (error "Invalid keyword detected: %s" keyword)))))
      (cons (list :key key :status status :tags tags) child-results))))

(defun org-x-dag-headline-get-iterator-project-status (dag key children)
  (org-x-dag-with-key key
    (let* ((kw (org-x-dag-id-lookup-prop key :todo))
           (status
            (cond
             ((or (member kw org-x--project-invalid-todostates)
                  (org-x-headline-is-scheduled nil))
              (list :project-error))
             ((equal kw org-x-kw-canc)
              (list :empt))
             ;; TODO this is a bit awkward since I don't care about the child statuses
             ;; and I don't care about tags
             ((equal kw org-x-kw-done)
              (org-x-dag-descend-into-project dag children nil
                ((:empt)
                 (:project-error :unscheduled :actv))
                (if (member it-kw org-x-done-keywords) 0 1)
                #'org-x-dag-headline-get-iterator-project-status))
             ((equal kw org-x-kw-todo)
              (org-x-dag-descend-into-project dag children nil
                ((:unscheduled :project-error)
                 (:empt)
                 (:actv))
                (let ((ts (org-x-headline-is-scheduled t)))
                  (cond
                   ((not ts) 0)
                   ((> org-x-iterator-active-future-offset (- ts (float-time))) 1)
                   (t 2)))
                #'org-x-dag-headline-get-iterator-project-status))
             (t (error "Invalid keyword detected: %s" kw)))))
      status)))

(defun org-x-dag-headline-get-iterator-task-status (key)
  (org-x-dag-with-key key
    (let ((kw (org-x-dag-id-lookup-prop key :todo)))
      (if (member kw org-x-done-keywords) :empt
        (-if-let (ts (or (org-x-headline-is-scheduled t)
                         (org-x-headline-is-deadlined t)))
            (if (< org-x-iterator-active-future-offset (- ts (float-time)))
                :actv
              :empt)
          :unscheduled)))))

(defun org-x-dag-headline-get-iterator-status (dag key)
  (cl-flet
      ((get-status
        (key)
        (-if-let (children (org-x-dag-node-get-headline-children dag key))
            (->> children
                 (org-x-dag-headline-get-iterator-project-status dag key)
                 (car))
          (org-x-dag-headline-get-iterator-task-status key))))
    (->> (org-x-dag-node-get-headline-children dag key)
         (-map #'get-status)
         (org-x-dag-get-max-index org-x--iter-statuscodes))))

(defmacro org-x-dag-with-keys-in-files (keys form)
  (declare (indent 1))
  `(->> (-group-by #'org-x-dag-key-get-file ,keys)
        (--mapcat (org-x-with-file (car it)
                    (--mapcat ,form (cdr it))))
        (-non-nil)))

(defmacro org-x-dag-with-files (files pre-form form)
  (declare (indent 2))
  (let* ((lookup-form '(ht-get file->ids it-file))
         (pre-form* (if pre-form
                        `(--filter ,pre-form ,lookup-form)
                      lookup-form)))
    `(-let (((&plist :file->ids) org-x-dag))
       (cl-flet
           ((proc-file
             (it-file)
             (-when-let (keys ,pre-form*)
               (org-x-with-file it-file
                 (--mapcat ,form keys)))))
         (-non-nil (-mapcat #'proc-file ,files))))))

(defmacro org-x-dag-with-key (key &rest body)
  (declare (indent 1))
  `(progn
     (goto-char (org-x-dag-key-get-point ,key))
     ,@body))

(defun org-x-dag-scan-projects ()
  (cl-flet*
      ((format-result
        (cat result)
        (-let* (((&plist :key :status :tags) result)
                (priority (alist-get status org-x-project-status-priorities)))
          (when (>= priority 0)
            (org-x-dag-with-key key
              (-> (org-x-dag-format-tag-node cat tags key)
                  (org-add-props nil
                      'x-toplevelp (org-x-dag-id-lookup-prop key :toplevelp)
                      'x-status status
                      'x-priority priority)))))))
    (let ((keys (->> (org-x-dag-get-toplevel-project-nodes)
                     (-map #'car))))
      (org-x-dag-with-keys-in-files keys
        (org-x-dag-with-key it
          (let ((cat (org-get-category))
                (tags (-> (org-x-dag-id-lookup-prop it :tags)
                          (append org-file-tags)
                          (org-x-dag-collapse-tags))))
            ;; TODO don't hardcode these things
            (unless (or (member org-x-tag-incubated tags)
                        (save-excursion
                          (-> org-x-prop-parent-type
                              (org--property-local-values nil)
                              (car)
                              (equal org-x-prop-parent-type-iterator))))
              (->> (org-x-dag-node-get-headline-children org-x-dag it)
                   (org-x-dag-headline-get-project-status org-x-dag it tags)
                   (--map (format-result cat it))))))))))

(defun org-x-dag-scan-iterators ()
  (cl-flet*
      ((format-result
        (tags cat key)
        (-let* ((status (org-x-dag-headline-get-iterator-status (plist-get org-x-dag :dag) key)))
          (org-x-dag-with-key key
            (-> (org-x-dag-format-tag-node cat tags key)
                (org-add-props nil
                    'x-status status))))))
    (let ((keys (->> (org-x-dag-get-toplevel-project-nodes)
                     (-map #'car))))
      (org-x-dag-with-keys-in-files keys
        (org-x-dag-with-key it
          (let ((cat (org-get-category))
                (tags (-> (org-x-dag-id-lookup-prop it :tags)
                          (append org-file-tags)
                          (org-x-dag-collapse-tags))))
            ;; TODO don't hardcode these things
            (when (and (not (member org-x-tag-incubated tags))
                       (save-excursion
                         (-> org-x-prop-parent-type
                             (org--property-local-values nil)
                             (car)
                             (equal org-x-prop-parent-type-iterator))))
              (list (format-result tags cat it)))))))))

;; TODO sloppy nonDRY hack
(defun org-x-dag-id->headline-children (id)
  (org-x-dag-node-get-headline-children (plist-get org-x-dag :dag) id))

(defun org-x-dag-get-task-nodes (id)
  (declare (indent 2))
  (cl-labels
      ((descend
        (children)
        ;; TODO don't hardcode this
        (->> (--remove (member (org-x-dag-id-lookup-prop it :todo)
                               (list org-x-kw-canc org-x-kw-hold))
                       children)
             (--mapcat (-if-let (cs (org-x-dag-id->headline-children it))
                           (descend cs)
                         (list it))))))
    (-some-> (org-x-dag-id->headline-children id)
      (descend))))

(defun org-x-dag-id->is-floating-p (id)
  (-> (plist-get org-x-dag :dag)
      (dag-get-floating-nodes)
      (ht-get id)))

(defun org-x-dag-id->is-toplevel-p (id)
  (or (org-x-dag-id-lookup-prop id :toplevelp)
      (org-x-dag-id->is-floating-p id)))

(defun org-x-dag-scan-tasks ()
  (cl-flet
      ((format-key
        (category is-standalone key)
        (let ((tags (->> (org-x-dag-get-inherited-tags org-file-tags
                                                       (plist-get org-x-dag :dag)
                                                       key)
                         (append (org-x-dag-id-lookup-prop key :tags))
                         (org-x-dag-collapse-tags))))
          ;; filter out incubators
          (org-x-dag-with-key key
            (unless (or (member org-x-tag-incubated tags)
                        (org-x-headline-is-scheduled nil)
                        (org-x-headline-is-deadlined nil))
              (let* ((s (org-x-headline-get-task-status-0 (org-x-dag-id-lookup-prop key :todo)))
                     (p (alist-get s org-x-headline-task-status-priorities)))
                (unless (= p -1)
                  (-> (org-x-dag-format-tag-node category tags key)
                      (org-add-props nil
                          'x-is-standalone is-standalone
                          'x-status s)))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (let ((category (org-get-category)))
        (-if-let (project-tasks (org-x-dag-get-task-nodes it))
            (--map (format-key category nil it) project-tasks)
          (list (format-key category t it)))))))

;; (defun org-x-dag-scan-tags ()
;;   (let* ((dag org-x-dag)
;;          (nodes (org-x-dag-get-toplevel-project-nodes dag)))
;;     (->> (--group-by (org-x-dag-key-get-file (car it)) nodes)
;;          (--mapcat
;;           (-let (((path . nodes) it))
;;             (org-x-with-file path
;;               (->> (-map #'car nodes)
;;                    (--mapcat
;;                     (progn
;;                       (goto-char (org-x-dag-key-get-point it))
;;                       (org-x-dag-format-tag-node dag (org-get-tags (point)) it))))))))))

(defun org-x-dag-get-inherited-tags (init dag key)
  (let* ((this-file (org-x-dag-key-get-file key)))
    (cl-labels
        ((ascend
          (k tags)
          (-if-let (parent (->> (dag-get-parents k dag)
                                (--first (equal (org-x-dag-key-get-file it)
                                                this-file))))
              (->> (org-x-dag-id-lookup-prop parent :tags)
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

;; (defun org-x-dag-tags-view (_match)
;;   (org-x-dag-sync t)
;;   (let ((org-agenda-files (org-x-get-action-files)))
;;     (nd/with-advice
;;         (('org-scan-tags :override (lambda (&rest _) (org-x-dag-scan-tags))))
;;       (org-tags-view '(4) "TODO"))))

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
