;;; org-x-macs.el --- Org for Apple Devs ;) -*- lexical-binding: t; -*-

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

(require 'org-agenda)
(require 'dash)

(defmacro org-x-with-file (path &rest body)
  "Open PATH and execute BODY."
  (declare (indent 1))
  `(with-current-buffer (find-file-noselect ,path)
     (save-excursion
       ,@body)))

;; lift buffer commands into agenda context

;; TODO body prints a message and update is 'update-all' then the
;; message will be overwritten
(defmacro org-x-agenda-cmd-wrapper (update &rest body)
  "Execute BODY in context of agenda buffer.
Specifically, navigate to the original header, execute BODY, then
update the agenda buffer. If UPDATE is 'update-headline', get the
headline string and use it to update the agenda (this is only
needed when the headline changes obviously). When update is
'update-all', reload the entire buffer. When UPDATE is nil, do
nothing."
  (declare (indent 1))
  (-let* ((newhead (make-symbol "newhead"))
          (hdmarker (make-symbol "hdmarker"))
          ((update-form get-head-form)
           (cond
            ((eq update 'update-headline)
             (list `((org-agenda-change-all-lines ,newhead ,hdmarker))
                   `((setq ,newhead (org-get-heading)))))
            ((eq update 'update-all)
             (list '((org-agenda-redo))
                   nil)))))
    `(progn
       (org-agenda-check-no-diary)
       (let* ((,hdmarker (or (org-get-at-bol 'org-hd-marker)
                             (org-agenda-error)))
              (buffer (marker-buffer ,hdmarker))
              (pos (marker-position ,hdmarker))
              (inhibit-read-only t)
              ,newhead)
         (org-with-remote-undo buffer
           (with-current-buffer buffer
             (widen)
             (goto-char pos)
             (org-show-context 'agenda)
             ,@body
             ,@get-head-form)
           ,@update-form
	       (beginning-of-line 1))))))

(provide 'org-x-macs)
;;; org-x-macs.el ends here
