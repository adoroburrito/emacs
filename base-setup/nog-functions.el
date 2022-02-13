;;; nog-functions.el --- functions nog made or copied from others
;;;
;;; Commentary:
;;
;; Functions i've made or copied from the web
;;
;;; Code:

(defun nog-reload ()
  "Tell emacs to read the init.el file again"
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(defun nog-packages-reinstall ()
  "Remove elpa directory and tell emacs to read the init.el file again"
  (interactive)
  (delete-directory "~/.emacs.d/elpa" t)
  (nog-reload)
)

(defun open-in-project-dir ()
  (let ((project-dir (ffip-project-root))
	(file-name (buffer-file-name)))
    (if project-dir
	(progn
	  (neotree-dir project-dir)
	  (neotree-find file-name))
      (message "Could not find git project root."))))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (unless (boundp 'nog-neotree-open)
    (setq nog-neotree-open nil))

  (if (null nog-neotree-open)
      (progn
	(open-in-project-dir)
	(setq nog-neotree-open t))
    (progn
      (neotree-hide)
      (setq nog-neotree-open nil)))
  )

(defun nog-windows-path (path)
  "Format unix path so windows can understand it"
  (interactive)
  (replace-regexp-in-string "/" "\\\\" path)
)

(defun nog-get-filename-format (filename)
  "Split string by dot (.) and get last element of the list"
  (string-trim (car (last (split-string filename "\\.")))))

(defun nog-insert-shell-command-output (command prefix)
  "Execute shell command, get result, move to end of line,
  insert 2 newlines with the result of the command, prefixed if
  prefix was given"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (newline-and-indent)
  (if prefix
      (insert (concat prefix (shell-command-to-string command)))
    (insert (shell-command-to-string command))))

(defun nog-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun nog-shell-command-output-to-buffer (command)
  "Same as nog-insert-shell-command-output but to a temp buffer instead"
  (interactive)
  (pop-to-buffer (get-buffer-create "*Shell Command Output*"))
  (erase-buffer)
  (insert (shell-command-to-string command))
  (beginning-of-buffer)
  (nog-display-ansi-colors))

(add-hook 'nog-shell-command-output-to-buffer 'nog-rename-shell-buffer)

(defun nog-run-file-through-command ()
  "Get current filename format, based on that format choose which command to run
  (output will be inserted 2 lines below, with provided prefix, if non-nil)"
  (interactive)
  (let (
	(format (nog-get-filename-format (buffer-file-name)))
       )
    (cond
     ((equal format "lisp") (nog-insert-shell-command-output (concat "sbcl --script " (buffer-file-name)) ";; "))
     ((equal format "ts") (nog-shell-command-output-to-buffer (concat "deno run --allow-read --quiet " (buffer-file-name))))
     (t (message (concat "Can't run program for current file! Unknown format: " format)))
    )
  )
)

(defun nog-add-folder-as-project (folder-path project-name)
  "Add folder path to an internal file that tracks known projects"
  (interactive
   (list
    (read-string "Folder path (empty for current folder):")
    (read-string "Project name (required):")))
  (let ((function-prefix "[nog-add-folder-as-project] "))
    (message (concat function-prefix "Provided folder path: " folder-path " | Provided project name: " project-name))
    (if (= (length project-name) 0)
	(message (concat function-prefix "No project name provided!")))
    (unless (= (length project-name) 0)
      (if (= (length folder-path) 0)
          (write-region (concat (file-name-directory buffer-file-name) "|||" project-name "\n") nil (concat user-emacs-directory "known-projects.nog") 'append)
        (write-region (concat folder-path "|||" project-name "\n") nil (concat user-emacs-directory "known-projects.nog") 'append)))))

(defun nog-list-projects ()
  "List all projects that are known (in file ~/.emacs.d/known-projects.nog)"
  (interactive)
  (setq nog-available-projects '())
  (with-temp-buffer
    (insert-file-contents-literally "~/.emacs.d/known-projects.nog")
    (while (not (eobp))
      (let* (
	     (line
	      (buffer-substring-no-properties
	       (line-beginning-position)
	       (line-end-position)))
	     (line-to-list (split-string line "|||"))
	     (project-path (nth 0 line-to-list))
	     (project-name (nth 1 line-to-list))
	    )
	(message (concat "Project path is '" project-path "' and name is '" project-name "'"))
	(add-to-list 'nog-available-projects (cons project-name  project-path))
	(forward-line 1)))
    (find-file (alist-get
		(completing-read "Choose: " nog-available-projects)
		nog-available-projects
		nil
		nil
		'equal))))

;;; nog-functions.el ends here
