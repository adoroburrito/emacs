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

;; Quick help (got the idea from https://www.olivertaylor.net/emacs/quick-help.html)
(defmacro quick-help (name buffer text)
  "Macro for creating callable functions that display help.
Where NAME is name of function, BUFFER is name of buffer, and TEXT is displayed."
  (declare (indent defun))
  `(progn
     (defun ,name nil
       ,buffer
       (interactive)
       (let ((qh-buff (concat "*Quick Help: " ,buffer "*"))
             (qh-text ,text))
         (get-buffer-create qh-buff)
         (with-current-buffer qh-buff
           (insert qh-text)
           (goto-char (point-min))
           (not-modified)
           (read-only-mode)
           (local-set-key (kbd "C-g") (lambda () (interactive) (other-window -1)))
           (local-set-key (kbd "q") 'kill-buffer-and-window))
         (pop-to-buffer qh-buff '((display-buffer-below-selected)
                                  (window-parameters . ((no-other-window . nil)))
                                  (window-height . fit-window-to-buffer)))
         (message "C-g - Previous Window, q - Remove Window")))))

(quick-help qh--it-hotline
  "IT Hotline"
  "IT HOTLINE: 855-555-5555")

(quick-help qh--departments
  "Departments"
  "\
| Department | Manager     | Extension | Time Zone |
|------------+-------------+-----------+-----------|
| Sales      | Dave F      |        16 | LA        |
| IT         | Sydney R    |       198 | NY        |
| Support    | Ellie T     |        29 | DEN       |
| Shipping   | Shaun D     |       345 | ATL       |
| Recieving  | Brian C     |       876 | NY        |
| Marketing  | Elizabeth W |        12 | LA        |
| Coffee     | Donna F     |        34 | NY        |")

(quick-help qh--wheather
  "Weather Whether Wether"
  "The climate is made up of “WEATHER”;
WHETHER it is nice out depends on whether it is raining or not.
A WETHER is just a castrated sheep.")

(define-prefix-command 'quick-help-prompt nil "Quick Help")

(let ((map quick-help-prompt))
  (define-key map "i" '("IT Hotline" . qh--it-hotline))
  (define-key map "d" '("Departments" . qh--departments))
  (define-key map "w" '("Weather Whether Wether" . qh--wheather)))

;;; nog-functions.el ends here
