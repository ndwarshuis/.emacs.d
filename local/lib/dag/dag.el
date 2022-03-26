;;; dag.el --- Functions for directed acyclic graphs -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: tools
;; Package-Requires: ((emacs "27.2") (dash "2.18"))
;; Version: 0.0.1

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

;; DAGs and stuff

;;; Code:

(require 'ht)
(require 'dash)

(defun dag--ht-create (what)
  "Create a hash table.

Hash table will always be created with the `equal' test.

The type of WHAT will determine how the hash table is build:
- number: allocate a certain size
- nul: allocate an empty hash table
- anything else: assume its an alist and allocate with that"
  (cond
   ((null what) (ht-create #'equal))
   ((numberp what) (make-hash-table :size what :test #'equal))
   (t (ht<-alist what #'equal))))

(defun dag--bimap-create (init)
  "Create a bidirectional adjacency list.

INIT is an alist where the car is a child key and the cdr is the
parent keys for that child."
  (let ((ht-child (dag--ht-create nil))
        (ht-parent (dag--ht-create nil))
        key parents)
    (--each init
      (setq key (car it)
            parents (cdr it))
      (ht-set ht-child key parents)
      (ht-set ht-parent key nil))
    (--each init
      (setq key (car it)
            parents (cdr it))
      (while parents
        (->> (ht-get ht-parent (car parents))
             (cons key)
             (ht-set ht-parent (car parents)))
        (!cdr parents)))
    (list :parent ht-parent :child ht-child)))

;; (defun dag--bimap-remove (keys bimap)
;;   (-let (((&plist :parent :child) bimap))
;;     (--each keys
;;       (--each (ht-get child it)
;;         (ht-set parent it (remove it it))
;;       (ht-remove child it)
;;       (ht-remove parent it))))))

(defun dag--plist-cons (plist prop x)
  (plist-put plist prop (cons x (plist-get plist prop))))

(defun dag--plist-remove (plist prop x)
  (plist-put plist prop (remove x (plist-get plist prop))))

(defun dag--adjlist-remove-edge (which key edge adjlist)
  (ht-set adjlist key (dag--plist-remove (ht-get adjlist key) which edge)))

(defun dag--adjlist-add-edge (which key edge adjlist)
  (ht-set adjlist key (dag--plist-cons (ht-get adjlist key) which edge)))

(defun dag--adjlist-remove-child-edge (key edge adjlist)
  (dag--adjlist-remove-edge :children key edge adjlist))

(defun dag--adjlist-remove-parent-edge (key edge adjlist)
  (dag--adjlist-remove-edge :parents key edge adjlist))

(defun dag--adjlist-add-parent-edge (key edge adjlist)
  (dag--adjlist-add-edge :parents key edge adjlist))

(defun dag--adjlist-add-child-edge (key edge adjlist)
  (dag--adjlist-add-edge :children key edge adjlist))

(defun dag--ht-cons (h k x)
  (ht-set h k (cons x (ht-get h k))))

(defun dag--ht-remove (h k x)
  (ht-set h k (remove x (ht-get h k))))

(defun dag--adjlist-get-relations (key adjlist)
  (ht-get adjlist key))

(defun dag-relation-get-children (rel)
  (plist-get rel :children))

(defun dag-relation-get-parents (rel)
  (plist-get rel :parents))

(defun dag--adjlist-get-children (key adjlist)
  (->> (ht-get adjlist key)
       (dag-relation-get-children)))

(defun dag--adjlist-get-parents (key adjlist)
  (->> (ht-get adjlist key)
       (dag-relation-get-parents)))

(defun dag--new-relationship (p c n)
  (list :parents p :children c :node-meta n))

(defmacro dag--each-key (h &rest body)
  (declare (indent 1))
  `(--each (ht-keys ,h) ,@body))

(defun dag--get-topological-order (adjlist)
  ;; this is just Kahn's algorithm
  (let ((parent-degrees (dag--ht-create (ht-size adjlist)))
        (node-queue)
        (ordered-nodes)
        (cur-node)
        (cur-degree))
    ;; TODO is there a way to do this without making a new hash table?
    ;; Get parent degrees and init the node queue with root nodes (those with
    ;; parent degree of zero)
    (dag--each-key adjlist
      (setq cur-degree (length (dag--adjlist-get-parents it adjlist)))
      (ht-set parent-degrees it cur-degree)
      (when (= 0 cur-degree)
        (!cons it node-queue)))
    ;; Traverse down the DAG starting at the root node(s). When encountering a
    ;; child, reduce the parent degrees of that child by one an add it to the
    ;; end of the node queue. Continue until the node queue is empty.
    (while node-queue
      (setq cur-node (car node-queue))
      (!cdr node-queue)
      (!cons cur-node ordered-nodes)
      (--each (dag--adjlist-get-children cur-node adjlist)
        (setq cur-degree (1- (ht-get parent-degrees it)))
        (ht-set parent-degrees it cur-degree)
        (when (= 0 cur-degree)
          (setq node-queue (-snoc node-queue it)))))
    ;; If all parent degrees have been reduced to 0, then return order as this
    ;; is a valid DAG. NOTE: if the DAG is invalid the parent degrees don't tell
    ;; us which nodes have cycles.
    (when (--all-p (= 0 it) (ht-values parent-degrees))
      (nreverse ordered-nodes))))

(defun dag--alist-to-ht (parent-adjlist)
  (let ((h (dag--ht-create nil))
        (broken-edges (dag--ht-create nil))
        (parents)
        (relations)
        (parent-relations)
        (cur))
    ;; Convert alist to initial hash table by making each value a plist with
    ;; parent and child connections
    ;;
    ;; O(N)
    (while parent-adjlist
      (setq cur (car parent-adjlist))
      (ht-set h
              (plist-get cur :id)
              (dag--new-relationship (plist-get cur :parents)
                                     nil
                                     (plist-get cur :node-meta)))
      (!cdr parent-adjlist))
    ;; Add child relationships: For each node key, get the parent relation keys,
    ;; and for each of these, lookup the key in the hash table and add the
    ;; parent relation key to the child relation keys of the lookup key. If this
    ;; fails (eg a node is linked to a parent that doesn't exist) remove this
    ;; key from the parent relations list and add the node key and parent key to
    ;; a list of 'broken' parent edges.
    ;;
    ;; O(E+N)
    (dag--each-key h
      (setq relations (ht-get h it)
            parents (plist-get relations :parents))
      (while parents
        (setq cur (car parents)
              parents (!cdr parents)
              parent-relations (ht-get h cur))
        (if parent-relations
            (dag--plist-cons parent-relations :children it)
          (dag--ht-cons broken-edges it cur)
          (dag--plist-remove relations :parents cur))))
    (list h broken-edges)))

(defun dag--longest-paths (adjlist suborder)
  (let ((distances (dag--ht-create (ht-size adjlist)))
        ;; Flip the order so we climb up the topological order from the deepest
        ;; nodes first
        ;; NOTE Need to use reverse here an not nreverse so the original list
        ;; isn't modified by side effect
        (rsuborder (reverse suborder))
        cur cur-distance child-distance)
    ;; initialize distances (all except first to 0)
    (ht-set distances (car rsuborder) 0)
    (--each (cdr rsuborder) (ht-set distances it nil))
    ;; walk up the parent edges and add one to each distance 
    (while rsuborder
      (setq cur (car rsuborder))
      (--each (dag--adjlist-get-parents cur adjlist)
        (when (setq cur-distance (ht-get distances cur))
          (setq child-distance (ht-get distances it))
          (when (or (null child-distance) (< child-distance (1+ cur-distance)))
            (ht-set distances it (1+ cur-distance)))))
      (!cdr rsuborder))
    distances))

(defun dag--transitive-reduction (adjlist order)
  (let (extra-edges cur distances cur-distance)
    (while order
      (setq distances (dag--longest-paths adjlist order)
            cur (car order))
      (dag--each-key distances
        (setq cur-distance (ht-get distances it))
        (when (and cur-distance (< 1 cur-distance))
          (!cons (cons cur it) extra-edges)))
      (setq order (cdr order)))
    extra-edges))

(defun dag--create (adjlist broken-edges)
  (list :adjlist adjlist
        :broken-edges broken-edges
        :order (dag--get-topological-order adjlist)))

(defun dag--prune-broken-edges (broken-edges)
  (dag--each-key broken-edges
    (unless (ht-get broken-edges it)
      (ht-remove broken-edges it)))
  broken-edges)


(defun dag--adjlist-remove-nodes-0 (to-remove adjlist)
  (let (r r-rel child-rel broken-acc)
    (while to-remove
      ;; If the node to be removed is in the adjacency list, get a list of its
      ;; parents, remove the node from the child list of each parent, then
      ;; delete the node itself.
      (setq r (car to-remove))
      (when (setq r-rel (ht-get adjlist r))
        (--each (plist-get r-rel :parents)
          (when (and (not (member it to-remove)) (ht-contains-p adjlist it))
            (dag--adjlist-remove-child-edge it r adjlist)))
        ;; If a child edge refers to a node that is not about to be removed,
        ;; remove the parent edge from the the child and add it to broken edges.
        ;; Otherwise do nothing because the child will be removed later anyways.
        (--each (plist-get r-rel :children)
          (when (and (setq child-rel (ht-get adjlist it))
                     (not (member it to-remove)))
            (ht-set adjlist it (dag--plist-remove child-rel :parents r))
            (!cons (cons it r) broken-acc)))
        (ht-remove adjlist r))
      (!cdr to-remove))
    (list adjlist broken-acc)))

(defun dag--adjlist-remove-nodes (to-remove adjlist broken-edges)
  (-let (((adjlist* broken) (dag--adjlist-remove-nodes-0 to-remove adjlist)))
    (--each to-remove 
      (ht-remove broken-edges it))
    (--each broken
      (dag--ht-cons broken-edges (car it) (cdr it)))
    (list adjlist* broken-edges)))

(defmacro dag--intersection-difference (xs ys &optional zs)
  "Calculate the intersection and difference of XS and YS.
XS will contain all its members that are also in YS, and YS will
retain all its values that are not in XS. Both are modified in
place.

If ZS is given, additionally store the difference of YS and XS in
ZS."
  (let ((found-form (if zs `(if found (!cons i ,xs) (!cons i ,zs))
                      `(when found (!cons i ,xs)))))
    `(let (i j tmp-xs tmp-ys found)
       (setq tmp-xs ,xs
             ,xs nil)
       (while tmp-xs
         (setq i (car tmp-xs)
               tmp-ys ,ys
               ,ys nil
               found nil)
         (while tmp-ys
           (if (eq i (setq j (car tmp-ys)))
               (setq ,ys (append ,ys (cdr tmp-ys))
                     tmp-ys nil
                     found t)
             (!cons j ,ys)
             (!cdr tmp-ys)))
         ,found-form
         (!cdr tmp-xs)))))

(defun dag--mend-edge (adjlist broken edge)
  (let ((keys (ht-keys broken))
        found i j tmp1 tmp2)
    (while (and (not found) keys)
      (setq i (car keys)
            tmp1 (ht-get broken i)
            tmp2 nil)
      ;; remove something without scanning a list twice (isn't this basically a
      ;; zipper being used in a really weird way?)
      (while tmp1
        (if (not (equal edge (setq j (car tmp1))))
            (!cons j tmp2)
          (ht-set broken i (append tmp2 (cdr tmp1)))
          (setq found i))
        (!cdr tmp1))
      (!cdr keys))
    (when found
      (dag--adjlist-add-parent-edge found edge adjlist)
      (dag--adjlist-add-child-edge edge found adjlist))))

(defun dag--adjlist-insert-nodes (to-insert adjlist broken-ht)
  (let (i i-key i-rel edges-to-add parent-edges broken-edges edges-to-remove
          parent-rel to-insert* meta-to-add)
    (while to-insert
      (setq i (car to-insert)
            i-key (plist-get i :id)
            edges-to-add (plist-get i :parents)
            meta-to-add (plist-get i :node-meta))
      ;; Add new node:
      ;;
      ;; If the node does not exist, add an empty relationship (it will be
      ;; filled in the next loop). If the node is a parent in the broken-hashes
      ;; table, transfer it to the adjacency list.
      (if (not (setq i-rel (ht-get adjlist i-key)))
          (progn
            (ht-set adjlist i-key (dag--new-relationship nil nil meta-to-add))
            (dag--mend-edge adjlist broken-ht i-key))
        ;; If the node does exist, get the edges that shouldn't be changed
        ;; (added & current), the edges that are to be added (added - current)
        ;; and the edges that are to be remove (current - added) (note 'added'
        ;; and 'current' are treated as sets with the appropriate notation). Set
        ;; the new parent slot to the edges that aren't to be changed, remove
        ;; the child references in all parent nodes, and keep the edges to be
        ;; added for later.
        (setq parent-edges (plist-get i-rel :parents))
        (dag--intersection-difference parent-edges edges-to-add edges-to-remove)
        (ht-set adjlist i-key (-> (plist-put i-rel :parents parent-edges)
                                  (plist-put :node-meta meta-to-add)))
        (--each edges-to-remove
          (dag--adjlist-remove-child-edge it i-key adjlist))
        ;; Similar to above, get the edges to be added and the nodes that are to
        ;; remain, and set the broken edges hash table to the latter.
        (setq broken-edges (ht-get broken-ht i-key))
        (dag--intersection-difference broken-edges edges-to-add)
        (ht-set broken-ht i-key broken-edges))
      (!cons (cons i-key edges-to-add) to-insert*)
      (!cdr to-insert))
    ;; Add edges in a separate loop since we need all the inserted nodes to be
    ;; present before testing if an edge is broken
    (while to-insert*
      (setq i (car to-insert*)
            i-key (car i))
      ;; Add new node to the child list of newly linked parents. This needs to
      ;; be done separately from above since we don't know if the new edges are
      ;; broken or not
      (--each (cdr i)
        (if (not (setq parent-rel (ht-get adjlist it)))
            (dag--ht-cons broken-ht i-key it)
          (ht-set adjlist it (dag--plist-cons parent-rel :children i-key))
          (dag--adjlist-add-parent-edge i-key it adjlist)))
      (!cdr to-insert*))
    (list adjlist (dag--prune-broken-edges broken-ht))))

(defun dag-plist-to-dag (parent-adjlist)
  "Convert PARENT-ADJLIST to a DAG.

PARENT-ADJLIST is a list of plists where each member represents a
node in which the :id is the node ID, :parents are the ID's of
the parents of the ID in :id, and :node-meta is arbitrary
metadata associated with the node.

Return a DAG object."
  (-let (((a b) (dag--alist-to-ht parent-adjlist)))
    (dag--create a b)))

(defun dag-empty ()
  "Return an empty DAG."
  (dag--create (dag--ht-create nil) (dag--ht-create nil)))

(defun dag-remove-nodes (to-remove dag)
  (-let (((&plist :adjlist a :broken-edges b) dag))
    (->> (dag--adjlist-remove-nodes to-remove a b)
         (apply #'dag--create))))

(defun dag-insert-nodes (to-insert dag)
  (-let* (((&plist :adjlist a :broken-edges b) dag)
          ((a* b*) (dag--adjlist-insert-nodes to-insert a b)))
    (dag--create a* b*)))

(defun dag-edit-nodes (to-remove to-insert dag)
  (if (not (or to-remove to-insert)) dag
    (-let* ((to-remove* (-difference to-remove (-map #'car to-insert)))
            ((&plist :adjlist a :broken-edges b) dag)
            ((a* b*) (dag--adjlist-remove-nodes to-remove* a b))
            ((a** b**) (dag--adjlist-insert-nodes to-insert a* b*)))
      (dag--create a** b**))))

(defun dag-get-adjacency-list (dag)
  (plist-get dag :adjlist))

(defun dag-get-broken-edges (dag)
  (plist-get dag :broken-edges))

(defun dag-get-topological-order (dag)
  (plist-get dag :order))

(defalias 'dag-get-all-nodes 'dag-get-topological-order)

(defun dag-get-length (dag)
  (length (dag-get-topological-order dag)))

(defun dag-is-valid-p (dag)
  (< 0 (dag-get-length dag)))

(defun dag-is-empty-p (dag)
  (= 0 (ht-size (dag-get-adjacency-list dag))))
  
(defun dag-get-node (key dag)
  (-some-> (dag-get-adjacency-list dag)
    (ht-get key)))

(defun dag-get-relationships (key dag)
  (->> (dag-get-adjacency-list dag)
       (dag--adjlist-get-relations key)))

(defun dag-get-parents (key dag)
  (->> (dag-get-relationships key dag)
       (dag-relation-get-parents)))

(defun dag-get-children (key dag)
  (->> (dag-get-relationships key dag)
       (dag-relation-get-children)))

(defmacro dag-get-nodes-and-edges-where (dag form)
  (declare (indent 1))
  `(let ((it-adjlist (dag-get-adjacency-list ,dag))
         acc it-rel)
     (dag--each-key it-adjlist
       (setq it-rel (ht-get it-adjlist it))
       (when ,form (!cons (cons it it-rel) acc)))
     acc))

(defun dag-get-leaf-nodes (dag)
  (dag-get-nodes-and-edges-where dag
    (not (dag--adjlist-get-children it it-adjlist))))

(defun dag-get-root-nodes (dag)
  (dag-get-nodes-and-edges-where dag
    (not (dag--adjlist-get-parents it it-adjlist))))

(defun dag-get-extra-nodes (dag)
  (-let (((&plist :adjlist :order) dag))
    (dag--transitive-reduction adjlist order)))

;; (defun dag-remove-node (key dag)
;;   (-let* (((&plist :nodes :order) dag))
;;     (-when-let (rel (ht-get nodes key))
;;       (plist-put dag :order (remove key order))
;;       (ht-remove nodes key)
;;       (--each (plist-get rel :children)
;;         (dag-remove-node it dag)))))

(provide 'dag)
;;; dag.el ends here
