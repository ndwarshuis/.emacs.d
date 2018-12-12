;; lovingly stolen from aaron harris
(defmacro nd/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun nd/filter-list-prefix (prefix str-list)
  "Return a subset of STR-LIST whose first characters are PREFIX."
  (seq-filter (lambda (i)
                (and (stringp i)
                     (string-prefix-p prefix i)))
              str-list))

(defun nd/move-key (keymap-from keymap-to key)
  "Move KEY from KEYMAP-FROM keymap to KEYMAP-TO keymap."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defun nd/get-apps-from-mime (mimetype)
  "Return all applications that can open a given MIMETYPE.
The list is comprised of alists where pairs are of the form (name . command)."
  (let* ((case-fold-search nil)
         (mime-regex (concat "^MimeType=.*" mimetype ";.*$"))
         (desktop-dirs '("/usr/share/applications"
                         "/usr/local/share/applications"
                         "~/.local/share/applications"))
         (desktop-files (mapcan (lambda (d) (directory-files d t ".*\\.desktop" t)) desktop-dirs))
         (app-list))
    (dolist (file desktop-files app-list)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((tb (buffer-string)))
          (if (string-match mime-regex tb)
              (let* ((exec (progn (string-match "^Exec=\\(.*\\)$" tb)
                                  (match-string 1 tb)))
                     (name (or
                            (progn (string-match "^Name=\\(.*\\)$" tb)
                                   (match-string 1 tb))
                            exec)))
                (setq app-list (cons `(,name . ,exec) app-list)))))))))

(defun nd/get-apps-bulk-from-mime (mimetype)
  "Like `nd/get-apps-from-mime' but only includes apps that can open
multiple files at once for given MIMETYPE."
  (let ((case-fold-search nil))
    (seq-filter (lambda (a) (string-match ".*%[FU].*" (car a))) (nd/get-apps-from-mime mimetype))))
    
(defun nd/execute-desktop-command (cmd file)
  "Opens FILE using CMD in separate process where CMD is from a 
desktop file exec directive."
  (let* ((cmd-arg (replace-regexp-in-string "%[fuFU]" file cmd t t)))
    (call-process-shell-command (concat cmd-arg " &"))))
  
(defun nd/get-mime-type (file)
  "Get the mime type of FILE."
  (let* ((cmd (concat "file --mime-type -b " file))
         (mt (shell-command-to-string cmd)))
    (replace-regexp-in-string "\n\\'" "" mt)))

(defvar nd/device-mount-dir (concat "/media/" (user-login-name)))

(defun nd/get-mounted-directories (&optional mount-path)
  "Scan MOUNT-PATH (defaults to /media/$USER for devices that have
been mounted by udevil."
  (seq-filter #'file-directory-p (directory-files nd/device-mount-dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun nd/device-mountable-p (devpath)
  "Returns label or uuid if device at DEVPATH is has a readable 
filesystem and is a usb drive."
  (let ((devprops (shell-command-to-string (concat "udevadm info --query=property " devpath))))
    (and (string-match-p (regexp-quote "ID_FS_TYPE") devprops)
         (string-match-p (regexp-quote "ID_BUS=usb") devprops)
         (progn
           (or (string-match "ID_FS_LABEL=\\(.*\\)\n" devprops)
               (string-match "ID_FS_UUID=\\(.*\\)\n" devprops))
           (match-string 1 devprops)))))

(defun nd/get-mountable-devices ()
  "Return paths of all mountable devices. (see `nd/device-mountable-p')."
  (seq-filter #'car
              (mapcar (lambda (d) `(,(nd/device-mountable-p d) . ,d))
                      (directory-files "/dev" t "sd.[0-9]+"))))

(defun nd/mount-device (dev &rest opts)
  "Mount device DEV using udevil."
  (call-process "udevil" nil nil nil "mount" dev))

(defun nd/get-mountpoint (dev)
  "Get the filesystem mountpoint for device DEV."
  (let ((mp (shell-command-to-string (concat "printf %s \"$(findmnt -n -o TARGET " dev ")\""))))
    (and (not (equal "" mp)) mp)))

(defun nd/split-and-follow-horizontally ()
  "Split window horizontally and move focus."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun nd/split-and-follow-vertically ()
  "Split window vertically and move focus."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
    
(defun nd/switch-to-previous-buffer ()
  "Switch the buffer to the last opened buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
  
(defun nd/config-reload ()
  "Reloads main configuration file at runtime."
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/conf/main.org")))

(defun nd/config-visit ()
  "Opens the main conf.org file (the one that really matters)."
  (interactive)
  (find-file "~/.emacs.d/conf/main.org"))

(defun nd/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun nd/close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun nd/org-close-all-buffers ()
  "Kill all org buffers."
  (interactive)
  (mapc 'kill-buffer (org-buffer-list)))

(defun nd/open-urxvt ()
  "Launch urxvt in the current directory."
  (interactive)
  (let ((cwd (expand-file-name default-directory)))
    (call-process "urxvt" nil 0 nil "-cd" cwd)))
