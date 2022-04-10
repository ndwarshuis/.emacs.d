;;; org-x-const.el --- Extra Org Constants -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>

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

;;; Code:

(require 'dash)

;;; TODO KEYWORDS

(defconst org-x-kw-todo "TODO"
  "Headline todo keyword for open task or project.")

(defconst org-x-kw-next "NEXT"
  "Headline todo keyword for next task.")

(defconst org-x-kw-wait "WAIT"
  "Headline todo keyword for task that is waiting on something.")

(defconst org-x-kw-hold "HOLD"
  "Headline todo keyword for task or project that is held.")

(defconst org-x-kw-done "DONE"
  "Headline todo keyword for completed task or project.")

(defconst org-x-kw-canc "CANC"
  "Headline todo keyword for canceled task or project.")

(defconst org-x-done-keywords `(,org-x-kw-done ,org-x-kw-canc)
  "Headline todo keywords that mark a task as 'complete'.")

(defconst org-x-meeting-keywords (cons org-x-kw-todo org-x-done-keywords)
  "Allowed keywords for meetings.")

;;; TAGS

(defun org-x-prepend-char (char string)
  "Return STRING with CHAR appended to the front."
  (concat (char-to-string char) string))

(defconst org-x-tag-location-prefix ?@
  "Prefix character denoting location context tag.")

(defconst org-x-tag-resource-prefix ?#
  "Prefix character denoting resource context tag.")

(defconst org-x-tag-misc-prefix ?%
  "Prefix character denoting misc tag.")

(defconst org-x-tag-category-prefix ?_
  "Prefix character denoting life category tag.")

(defconst org-x-exclusive-prefixes (list org-x-tag-category-prefix
                                         org-x-tag-location-prefix)
  "Tag prefixes which denote mutually exclusive groups.")

(defconst org-x-tag-errand
  (org-x-prepend-char org-x-tag-location-prefix "errand")
  "Tag denoting an errand location.")

(defconst org-x-tag-home
  (org-x-prepend-char org-x-tag-location-prefix "home")
  "Tag denoting a home location.")

(defconst org-x-tag-work
  (org-x-prepend-char org-x-tag-location-prefix "work")
  "Tag denoting a work location.")

(defconst org-x-tag-travel
  (org-x-prepend-char org-x-tag-location-prefix "travel")
  "Tag denoting a travel location.")

(defconst org-x-tag-laptop
  (org-x-prepend-char org-x-tag-resource-prefix "laptop")
  "Tag denoting a laptop resource.")

(defconst org-x-tag-phone
  (org-x-prepend-char org-x-tag-resource-prefix "phone")
  "Tag denoting a phone resource.")

(defconst org-x-tag-deep
  (org-x-prepend-char org-x-tag-misc-prefix "deep")
  "Tag denoting deep work.")

(defconst org-x-tag-note
  (org-x-prepend-char org-x-tag-misc-prefix "note")
  "Tag denoting a note.")

(defconst org-x-tag-incubated
  (org-x-prepend-char org-x-tag-misc-prefix "inc")
  "Tag denoting an incubated task.")

(defconst org-x-tag-maybe
  (org-x-prepend-char org-x-tag-misc-prefix "maybe")
  "Tag denoting a maybe task.")

(defconst org-x-tag-subdivision
  (org-x-prepend-char org-x-tag-misc-prefix "subdiv")
  "Tag denoting a task awaiting subdivision.")

(defconst org-x-tag-flagged
  (org-x-prepend-char org-x-tag-misc-prefix "flag")
  "Tag denoting a flagged task.")

(defconst org-x-tag-meeting
  (org-x-prepend-char org-x-tag-misc-prefix "meeting")
  "Tag denoting a meeting.")

;; (defconst org-x-tag-environmental
;;   (org-x-prepend-char org-x-tag-category-prefix "env")
;;   "Tag denoting an environmental life category.")

;; (defconst org-x-tag-financial
;;   (org-x-prepend-char org-x-tag-category-prefix "fin")
;;   "Tag denoting a financial life category.")

;; (defconst org-x-tag-intellectual
;;   (org-x-prepend-char org-x-tag-category-prefix "int")
;;   "Tag denoting an intellectual life category.")

;; (defconst org-x-tag-metaphysical
;;   (org-x-prepend-char org-x-tag-category-prefix "met")
;;   "Tag denoting an metaphysical life category.")

;; (defconst org-x-tag-physical
;;   (org-x-prepend-char org-x-tag-category-prefix "phy")
;;   "Tag denoting an physical life category.")

;; (defconst org-x-tag-professional
;;   (org-x-prepend-char org-x-tag-category-prefix "pro")
;;   "Tag denoting a professional life category.")

;; (defconst org-x-tag-recreational
;;   (org-x-prepend-char org-x-tag-category-prefix "rec")
;;   "Tag denoting a recreational life category.")

;; (defconst org-x-tag-social
;;   (org-x-prepend-char org-x-tag-category-prefix "soc")
;;   "Tag denoting a social life category.")

(defconst org-x-tag-no-agenda "NA"
  "Tag denoting a headlines that shouldn't go in the agenda.")

(defconst org-x-tag-no-archive "NRXIV"
  "Tag denoting a headlines that shouldn't go in the archive.")

(defconst org-x-tag-refile "REFILE"
  "Tag denoting a headlines that are to be refiled.")

(defconst org-x-life-categories
  (->> (list "environmental"
             "financial"
             "intellectual"
             "metaphysical"
             "physical"
             "professional"
             "recreational"
             "social")
       (--map (let* ((abbr (substring it 0 3))
                     (key (intern abbr))
                     (tag (org-x-prepend-char org-x-tag-category-prefix abbr)))
                (list key :tag tag :desc it))))
  "Alist of life categories.
The car of each member is a symbol representing the category, the
cdr is a plist which has entries for :tag and :desc which are the
org tag and a long name respectively for the category.")

(defun org-x-life-category-plist-get (key category-sym)
  (plist-get (alist-get category-sym org-x-life-categories) key))

(defun org-x-life-category-tag (category-sym)
  (org-x-life-category-plist-get :tag category-sym))

(defun org-x-life-category-desc (category-sym)
  (org-x-life-category-plist-get :desc category-sym))

;;; PROPERTIES

;; all follow the nomenclature `org-x-prop-PROPNAME' (key) or
;; `org-x-prop-PROPNAME-VALNAME' (value)

(defconst org-x-prop-parent-type "PARENT_TYPE"
  "Property denoting iterator/periodical headline.")

(defconst org-x-prop-parent-type-periodical "periodical"
  "Property value for a periodical parent type.")

(defconst org-x-prop-parent-type-iterator "iterator"
  "Property value for an iterator parent type.")

(defconst org-x-prop-time-shift "TIME_SHIFT"
  "Property denoting time shift when cloning iterator/periodical headlines.")

(defconst org-x-prop-location "X-LOCATION"
  "Property denoting location for meetings.")

;; TODO this is a WIP
(defconst org-x-prop-thread "THREAD"
  "Property denoting an email thread to track.")

(defconst org-x-prop-routine "X-ROUTINE"
  "Property denoting a routine group.")

(defconst org-x-prop-routine-morning "morning"
  "Property value for morning routine.")

(defconst org-x-prop-routine-evening "evening"
  "Property value for evening routine.")

(defconst org-x-prop-created "CREATED"
  "Property denoting when a headline was created.")

(defconst org-x-prop-expire "X-EXPIRE"
  "Property denoting when a headline will expire.")

(defconst org-x-prop-days-to-live "X-DAYS_TO_LIVE"
  "Property denoting after how many days a headline will expire.")

(defconst org-x-prop-goal "X-GOAL"
  "Property denoting the goal this headline fulfills.")

(defconst org-x-prop-allocate "X-ALLOCATE"
  "Property the property denoting intended time allocation.")

;;; DRAWERS

(defconst org-x-drwr-agenda "AGENDA_ITEMS"
  "Drawer to hold agenda items in meetings.")

(defconst org-x-drwr-action "ACTION_ITEMS"
  "Drawer to hold action items in meetings.")

(defconst org-x-drwr-categories "X_CATEGORIES"
  "Drawer to hold ranked categories for a quarterly plan.")

;;; PUBLIC VARS

(defconst org-x-archive-delay 30
  "The number of days to wait before tasks are considered archivable.")

(defconst org-x-inert-delay-days 90
  "The number of days to wait before tasks are considered inert.")

(defconst org-x-iterator-active-future-offset (* 7 24 60 60)
  "Iterators must have at least one task this far in the future to be active.")

(defconst org-x-periodical-active-future-offset
  org-x-iterator-active-future-offset
  "Periodicals must have at least one heading this far in the future to be active.")

(provide 'org-x-const)
;;; org-x-const.el ends here
