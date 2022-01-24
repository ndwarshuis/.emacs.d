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

(defun org-x-dag-build-meta (file point level todo tags parent)
  (list :file file
        :point point
        :level level
        :todo todo
        :tags tags
        :buffer-parent parent))

;; state lookup functions
;;
;; all functions with `org-x-dag->' or `org-x-dag-id->' depend on the value of
;; `org-x-dag'

(defun org-x-dag->metatable ()
  (plist-get org-x-dag :id->meta))

(defun org-x-dag->dag ()
  (plist-get org-x-dag :dag))

(defun org-x-dag->adjacency-list ()
  (dag-get-adjacency-list (org-x-dag->dag)))

(defun org-x-dag-id->metaprop (id prop)
  (-> (org-x-dag->metatable)
      (ht-get id)
      (plist-get prop)))

(defun org-x-dag-id->file (id)
  "Return file for ID."
  (org-x-dag-id->metaprop id :file))

(defun org-x-dag-id->point (id)
  "Return point for ID."
  (org-x-dag-id->metaprop id :point))

(defun org-x-dag-id->todo (id)
  "Return todo keyword for ID."
  (org-x-dag-id->metaprop id :todo))

(defun org-x-dag-id->local-tags (id)
  "Return local tags for ID."
  (org-x-dag-id->metaprop id :tags))

(defun org-x-dag-id->is-done-p (id)
  "Return t if ID has done keywords."
  (member (org-x-dag-id->todo id) org-x-done-keywords))

(defun org-x-dag-id->is-floating-p (id)
  (-> (plist-get org-x-dag :dag)
      (dag-get-floating-nodes)
      (ht-get id)))

(defun org-x-dag-id->is-toplevel-p (id)
  (or (not (org-x-dag-id->metaprop id :buffer-parent))
      ;; TODO pretty sure this will never be used
      (org-x-dag-id->is-floating-p id)))

(defun org-x-dag-id->tags (inherit? init id)
  (cl-labels
      ((ascend
        (id tags)
        (-if-let (parent (org-x-dag-id->metaprop id :buffer-parent))
            ;; tags in the front of the list have precedence over latter tags,
            ;; so putting parent tags at the end means child tags have
            ;; precedence
            (->> (org-x-dag-id->local-tags parent)
                 (append tags)
                 (ascend parent))
          tags)))
    ;; likewise, init tags have the lowest precedence (the likely use case for
    ;; this argument is for file tags)
    (let ((local-tags (org-x-dag-id->local-tags id))
          (parent-tags (and inherit? (ascend id nil))))
      (append local-tags parent-tags init))))

(defun org-x-dag-id->headline-children (id)
  (->> (plist-get org-x-dag :dag)
       (dag-get-children id)
       (--filter (equal (org-x-dag-id->metaprop it :buffer-parent) id))))

(defun org-x-dag-files->ids (files)
  (let ((filemap (plist-get org-x-dag :file->ids)))
    (--mapcat (ht-get filemap it) files)))

;;; BUFFER SCANNING

(defun org-x-dag-get-local-property (prop)
  (car (org--property-local-values prop nil)))

(defun org-x-dag-get-link-property ()
  (-some->> (org-x-dag-get-local-property org-x-prop-goal)
    (s-split ";")
    (--map (->> (s-trim it)
                (s-match "^\\[\\[id:\\(.*\\)\\]\\[.*\\]\\]$")
                (cadr)))))

(defun org-x-dag-get-buffer-nodes (file kws)
  "Return a list of nodes from FILE.

A node will only be returned if the headline to which it points
has a valid (meaning in KWS) keyword and either its parent has a
valid keyword or none of its parents have valid keywords."
  (let ((more t)
        cur-path this-point this-key this-level this-todo has-todo this-parent
        tags this-file-links acc acc-meta this-parent-key)
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
        ;; TODO this might be optimizable
        ;; Get tags (must be done from the first column)
        (setq this-tags (org--get-local-tags))
        ;; Get the level if the following char is a space (if it isn't this is
        ;; actually a bolded object that starts on the first column like
        ;; '*blabla*'
        (while (= ?* (following-char)) (forward-char 1))
        (when (= 32 (following-char))
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
                this-parent-key (nth 1 this-parent))
          ;; Add the current headline to accumulator if it has a keyword, but only
          ;; if its parent has a keyword or none of its parents have keywords
          (when (and has-todo (or this-parent-key (--none-p (nth 1 it) cur-path))
                     (setq this-key (org-x-dag-get-local-property "ID")))
            ;; If parent is not a todo and we want tag inheritance, store all tags
            ;; above this headline (sans file-tags which we can get later easily)
            ;; (org-entry-get nil org-x-prop-parent-type)
            (setq tags (if (and (not this-parent-key) org-use-tag-inheritance)
                           (->> cur-path
                                (--mapcat (nth 2 it))
                                (append this-tags))
                         this-tags)
                  this-meta (org-x-dag-build-meta file
                                                  this-point
                                                  this-level
                                                  (substring-no-properties this-todo)
                                                  tags
                                                  this-parent-key))
            (!cons (cons this-key this-meta) acc-meta)
            (!cons (cons this-key (append (list (nth 1 this-parent))
                                          (org-x-dag-get-link-property)))
                   acc))
          ;; Add current headline to stack
          (!cons (list this-level this-key this-tags) cur-path)))
      (setq more (= 0 (forward-line 1))))
    ;; TODO reverse these to make things make sense later?
    (list acc acc-meta)))

(defun org-x-dag-get-file-nodes (file)
  "Return all nodes in FILE in one pass."
  (org-x-with-file file
    (org-x-dag-get-buffer-nodes file org-todo-keywords-1)))

;;; DAG SYNCHRONIZATION/CONSTRUCTION

(defun org-x-dag-get-files ()
  "Return a list of all files to be used in the DAG."
  `(,(org-x-get-lifetime-goal-file)
    ,(org-x-get-endpoint-goal-file)
    ,@(org-x-get-action-and-incubator-files)))

(defun org-x-dag-get-md5 (path)
  "Get the md5 checksum of PATH."
  (org-x-with-file path (buffer-hash)))

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

(defun org-x-dag-read-files (files)
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
    (ht-set ht (car it) (cdr it))))

(defun org-x-dag-update-dag (to-insert to-remove)
  (let* ((dag (org-x-dag->dag))
         (dag* (if (dag-is-empty-p dag) (dag-alist-to-dag to-insert)
                 (dag-edit-nodes to-remove to-insert dag))))
    (plist-put org-x-dag :dag dag*)))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG."
  (-let* (((&plist :id->meta :file->ids) org-x-dag)
          (files2rem (append to-update to-remove))
          (files2ins (append to-update to-insert))
          (ids2rem (org-x-dag-files->ids files2rem))
          ((ids2ins meta2ins fms2ins) (org-x-dag-read-files files2ins)))
    (org-x-dag-update-ht ids2rem meta2ins id->meta)
    (org-x-dag-update-ht files2rem fms2ins file->ids)
    (org-x-dag-update-dag ids2ins ids2rem)))

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

;; NODE FORMATTING

(defconst org-x-dag-tag-prefix-order (list org-x-tag-misc-prefix
                                           org-x-tag-resource-prefix
                                           org-x-tag-location-prefix
                                           org-x-tag-category-prefix)
  "Order in which tags should appear in the agenda buffer (from right to left.")

(defun org-x-dag-collapse-tags (tags)
  "Return TAGS with duplicates removed.

In the case of mutually exclusive tags, only the first tag
encountered will be returned."
  (-let (((x non-x) (--separate (memq (elt it 0) org-x-exclusive-prefixes) tags)))
    (->> (--group-by (elt it 0) x)
         (--map (car (cdr it)) )
         (append (-uniq non-x))
         ;; this removes the 'inherited' property on some of the tags, which
         ;; makes the agenda look cleaner (to me) since there are no
         ;; double-colons to separate inherited from non-inherited
         ;;
         ;; NOTE: this appears to have no effect on `org-agenda-tags' (eg the
         ;; inherited tags still show up in the menu properly)
         (-map #'substring-no-properties))))

(defun org-x-dag-sort-tags (tags)
  (cl-flet
      ((get-ranking
        (tag)
        (-if-let (i (-elem-index (elt tag 0) org-x-dag-tag-prefix-order))
            (1+ i)
          0)))
    (->> (--map (cons it (get-ranking it)) tags)
         (--sort (< (cdr it) (cdr other)))
         (-map #'car))))

(defun org-x-dag-add-default-props (item)
  (org-add-props item nil
    'org-not-done-regexp org-not-done-regexp
    'org-todo-regexp org-todo-regexp
    'org-complex-heading-regexp org-complex-heading-regexp
    'mouse-face 'highlight))

(defun org-x-dag-format-tag-node (category tags key)
  ;; ASSUME I don't use subtree-level categories
  (-let* (;; (category (org-get-category))
          (tags* (->> tags
                      (org-x-dag-collapse-tags)
                      (org-x-dag-sort-tags)))
          (head (org-get-heading))
          (level (-> (org-x-dag-id->metaprop key :level)
                     (make-string ?s)))
          ;; no idea what this does...
          (help-echo (format "mouse-2 or RET jump to Org file %S"
                             (abbreviate-file-name
                              (or (buffer-file-name (buffer-base-buffer))
                                  (buffer-name (buffer-base-buffer))))))
          (marker (org-agenda-new-marker))
          ;; no idea what this function actually does
          ((ts . ts-type) (org-agenda-entry-get-agenda-timestamp (point)))
          (item (org-agenda-format-item "" head level category tags*))
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
            'todo-state (org-x-dag-id->todo key)
            'priority priority
            'ts-date ts
            ;; misc
            'type (concat "tagsmatch" ts-type)
            'help-echo help-echo))))

;;; HEADLINE PREDICATES
;;
;; The following are predicates that require the point to be above the
;; headline in question

(defun org-x-dag-headline-is-deadlined-p (want-time)
  (org-x-headline-has-timestamp org-deadline-time-regexp want-time))

(defun org-x-dag-headline-is-scheduled-p (want-time)
  (org-x-headline-has-timestamp org-scheduled-time-regexp want-time))

(defun org-x-dag-headline-is-closed-p (want-time)
  (org-x-headline-has-timestamp org-closed-time-regexp want-time))

(defun org-x-dag-headline-is-iterator-p ()
  (->> (org-x-dag-get-local-property org-x-prop-parent-type)
       (equal org-x-prop-parent-type-iterator)))

(defun org-x-headline-has-timestamp (re want-time)
  (let ((end (save-excursion (outline-next-heading))))
    (-when-let (p (save-excursion (re-search-forward re end t)))
      (if want-time (org-2ft (match-string 1)) p))))

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

(defun org-x-dag-time-is-archivable-p (epochtime)
  (< (- (float-time) epochtime) (* 60 60 24 org-x-archive-delay)))

(defun org-x-headline-get-task-status-0 (kw)
  (if (member kw org-x-done-keywords)
      (-if-let (c (org-x-dag-headline-is-closed-p t))
          (if (org-x-dag-time-is-archivable-p c)
              :archivable
            :complete)
        :done-unclosed)
    (cond
     ((org-x-headline-is-expired-p) :expired)
     ((org-x-headline-is-inert-p) :inert)
     ((org-x-dag-headline-is-closed-p nil) :undone-closed)
     (t :active))))

(defun org-x-dag-get-max-index (ys xs)
  "Return the member of XS that has the highest index in YS."
  (--max-by (> (-elem-index it ys) (-elem-index other ys)) xs))

(defmacro org-x-dag-descend-into-project (keys parent-tags codetree task-form
                                               callback)
  (declare (indent 2))
  (let ((allowed-codes (-map #'car codetree))
        (trans-tbl (--mapcat (-let (((a . bs) it))
                               (--map (cons it a) bs))
                             codetree)))
    `(cl-flet
         ((get-project-or-task-status
           (key)
           (-if-let (children (org-x-dag-id->headline-children key))
               (let* ((tags (org-x-dag-id->tags nil ,parent-tags key))
                      (child-results (funcall ,callback key tags children))
                      ;; ASSUME the car of the results will be the toplevel
                      ;; key/status pair for this (sub)project
                      (top-status (plist-get (car child-results) :status))
                      (top-status* (if (member top-status ',allowed-codes)
                                       top-status
                                     (alist-get top-status ',trans-tbl))))
                 (cons top-status* child-results))
             (let ((it-kw (org-x-dag-id->todo key)))
               (org-x-dag-with-key key
                 (-> ,task-form
                     (nth ',allowed-codes)
                     (list)))))))
       (let* ((results (-map #'get-project-or-task-status ,keys))
              (status (->> (-map #'car results)
                           (org-x-dag-get-max-index ',allowed-codes))))
         (cons status (-mapcat #'cdr results))))))

(defun org-x-dag-headline-get-project-status (key tags children)
  ;; ASSUME children will always be at least 1 long
  (org-x-dag-with-key key
    (let ((keyword (org-x-dag-id->todo key)))
      (-let (((status . child-results)
              (cond
               ((org-x-dag-headline-is-scheduled-p nil)
                (list :scheduled-project))
               ((equal keyword org-x-kw-hold)
                (list (if (org-x-headline-is-inert-p) :inert :held)))
               ((member keyword org-x--project-invalid-todostates)
                (list :invalid-todostate))
               ((equal keyword org-x-kw-canc)
                (list (if (org-x-headline-is-archivable-p) :archivable :complete)))
               ((equal keyword org-x-kw-done)
                (org-x-dag-descend-into-project children tags
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
                (org-x-dag-descend-into-project children tags
                  ((:undone-complete :complete :archivable)
                   (:stuck :scheduled-project :invalid-todostate
                           :done-incomplete)
                   (:held)
                   (:wait)
                   (:inert)
                   (:active))
                  (cond
                   ((and (not (member it-kw org-x-done-keywords))
                         (org-x-headline-is-inert-p))
                    4)
                   ((equal it-kw org-x-kw-todo)
                    (if (org-x-dag-headline-is-scheduled-p nil) 5 1))
                   ((equal it-kw org-x-kw-hold)
                    2)
                   ((equal it-kw org-x-kw-wait)
                    3)
                   ((equal it-kw org-x-kw-next)
                    5)
                   (t 0))
                  #'org-x-dag-headline-get-project-status))
               (t (error "Invalid keyword detected: %s" keyword)))))
        (cons (list :key key :status status :tags tags) child-results)))))

(defun org-x-dag-headline-get-iterator-project-status (key children)
  (org-x-dag-with-key key
    (let* ((kw (org-x-dag-id->todo key))
           (status
            (cond
             ((or (member kw org-x--project-invalid-todostates)
                  (org-x-dag-headline-is-scheduled-p nil))
              (list :project-error))
             ((equal kw org-x-kw-canc)
              (list :empt))
             ;; TODO this is a bit awkward since I don't care about the child statuses
             ;; and I don't care about tags
             ((equal kw org-x-kw-done)
              (org-x-dag-descend-into-project children nil
                ((:empt)
                 (:project-error :unscheduled :actv))
                (if (member it-kw org-x-done-keywords) 0 1)
                #'org-x-dag-headline-get-iterator-project-status))
             ((equal kw org-x-kw-todo)
              (org-x-dag-descend-into-project children nil
                ((:unscheduled :project-error)
                 (:empt)
                 (:actv))
                (let ((ts (org-x-dag-headline-is-scheduled-p t)))
                  (cond
                   ((not ts) 0)
                   ((> org-x-iterator-active-future-offset (- ts (float-time))) 1)
                   (t 2)))
                #'org-x-dag-headline-get-iterator-project-status))
             (t (error "Invalid keyword detected: %s" kw)))))
      status)))

(defun org-x-dag-headline-get-iterator-task-status (key)
  (org-x-dag-with-key key
    (if (org-x-dag-id->is-done-p key) :empt
      (-if-let (ts (or (org-x-dag-headline-is-scheduled-p t)
                       (org-x-dag-headline-is-deadlined-p t)))
          (if (< org-x-iterator-active-future-offset (- ts (float-time)))
              :actv
            :empt)
        :unscheduled))))

(defun org-x-dag-headline-get-iterator-status (key)
  (cl-flet
      ((get-status
        (key)
        (-if-let (children (org-x-dag-id->headline-children key))
            (->> children
                 (org-x-dag-headline-get-iterator-project-status key)
                 (car))
          (org-x-dag-headline-get-iterator-task-status key))))
    (->> (org-x-dag-id->headline-children key)
         (-map #'get-status)
         (org-x-dag-get-max-index org-x--iter-statuscodes))))

(defmacro org-x-dag-with-key (key &rest body)
  (declare (indent 1))
  `(progn
     (goto-char (org-x-dag-id->point ,key))
     ,@body))

;;; SCANNERS
;;
;; Not sure what to call these, they convert the DAG to a list of agenda strings

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
                 ;; NOTE there are other ways in org to get the category; the
                 ;; only one I ever cared about was the filename. Very simple,
                 ;; category = filename. Done
                 (let ((it-category (f-base it-file)))
                   (--mapcat ,form keys))))))
         (-non-nil (-mapcat #'proc-file ,files))))))

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
                      'x-toplevelp (org-x-dag-id->is-toplevel-p key)
                      'x-status status
                      'x-priority priority))))))
       (format-key
        (cat key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          ;; TODO don't hardcode these things
          (org-x-dag-with-key key
            (unless (or (member org-x-tag-incubated tags)
                        (org-x-dag-headline-is-iterator-p))
              (-some->> (org-x-dag-id->headline-children key)
                (org-x-dag-headline-get-project-status key tags)
                (--map (format-result cat it))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (format-key it-category it))))

(defun org-x-dag-scan-iterators ()
  (cl-flet*
      ((format-result
        (tags cat key)
        (-let ((status (org-x-dag-headline-get-iterator-status key)))
          (org-x-dag-with-key key
            (-> (org-x-dag-format-tag-node cat tags key)
                (org-add-props nil
                    'x-status status))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (let ((tags (org-x-dag-id->tags t org-file-tags it)))
        (unless (member org-x-tag-incubated tags)
          (org-x-dag-with-key it
            (when (org-x-dag-headline-is-iterator-p)
              (list (format-result tags it-category it)))))))))
  
(defun org-x-dag-get-task-nodes (id)
  (declare (indent 2))
  (cl-labels
      ((descend
        (children)
        ;; TODO don't hardcode this
        (->> (--remove (member (org-x-dag-id->todo it)
                               (list org-x-kw-canc org-x-kw-hold))
                       children)
             (--mapcat (-if-let (cs (org-x-dag-id->headline-children it))
                           (descend cs)
                         (list it))))))
    (-some-> (org-x-dag-id->headline-children id)
      (descend))))

(defun org-x-dag-scan-tasks ()
  (cl-flet
      ((format-key
        (category is-standalone key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          ;; filter out incubators
          (org-x-dag-with-key key
            (unless (or (member org-x-tag-incubated tags)
                        (org-x-dag-headline-is-scheduled-p nil)
                        (org-x-dag-headline-is-deadlined-p nil))
              (let* ((s (org-x-headline-get-task-status-0 (org-x-dag-id->todo key)))
                     (p (alist-get s org-x-headline-task-status-priorities)))
                (unless (= p -1)
                  (-> (org-x-dag-format-tag-node category tags key)
                      (org-add-props nil
                          'x-is-standalone is-standalone
                          'x-status s)))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (-if-let (project-tasks (org-x-dag-get-task-nodes it))
          (--map (format-key it-category nil it) project-tasks)
        (list (format-key it-category t it))))))

(defun org-x-dag-scan-incubated ()
  (cl-flet
      ((format-key
        (category key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          (when (member org-x-tag-incubated tags)
            (org-x-dag-with-key key
              (let* ((sch (org-x-dag-headline-is-scheduled-p t))
                     (dead (org-x-dag-headline-is-deadlined-p t))
                     (is-project (org-x-dag-id->headline-children key)))
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-project-p is-project
                        'x-scheduled sch
                        'x-deadlined dead))))))))
    (org-x-dag-with-files (org-x-get-action-and-incubator-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (list (format-key it-category it)))))

(defun org-x-dag-scan-archived ()
  (cl-flet
      ((format-key
        (category key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          (unless (member org-x-tag-incubated tags)
            (org-x-dag-with-key key
              (-let (((is-archivable is-project)
                      (-if-let (children (org-x-dag-id->headline-children key))
                          (-> (org-x-dag-headline-get-project-status org-x-dag key tags children)
                              (alist-get org-x-project-status-priorities)
                              (eq :archivable)
                              (list t))
                        (-> (org-x-headline-get-task-status-0 (org-x-dag-id->todo key))
                            (alist-get org-x-headline-task-status-priorities)
                            (eq :archivable)
                            (list t)))))
                (when is-archivable
                  (-> (org-x-dag-format-tag-node category tags key)
                      (org-add-props nil
                          'x-project-p is-project)))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (let ((category (org-get-category)))
        (org-x-dag-with-key it
          (if (org-x-dag-headline-is-iterator-p)
              (--map (format-key category it) (org-x-dag-id->headline-children it))
            (list (format-key category it))))))))

;;; AGENDA VIEWS

;; (defun org-x-dag-show-tasks (_match)
;;   (org-x-dag-sync t)
;;   ;; hack to make the loop only run once
;;   (let ((org-agenda-files (list (car (org-x-get-action-files)))))
;;     (nd/with-advice
;;         (('org-scan-tags :override (lambda (&rest _) (org-x-dag-scan-tasks))))
;;       (org-tags-view '(4) "TODO"))))

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
