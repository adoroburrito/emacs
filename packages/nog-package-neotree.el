;;; neotree.el --- neotree
;;;
;;; Commentary:
;;
;; sets up neotree
;;
;;; Code:


(use-package neotree
  :init
  (require 'neotree)
  :config
  (setq neo-theme 'arrow)
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq-default neo-show-hidden-files t)
  (defun not-internal-buffer (buffer)
    (let ((bufname (string-trim (buffer-name buffer))))
      (cond
       ((equal bufname "*scratch*") nil)
       ((equal bufname "*Backtrace*") nil)
       ((equal bufname "*Messages*") nil)
       ((equal bufname "*Buffer List*") nil)
       ((equal bufname "*terminal*") nil)
       ((equal bufname "*tide-server*") nil)
       ((equal bufname "*quelpa-build-checkout*") nil)
       ((equal bufname "*code-conversion-work*") nil)
       ((equal bufname "*Echo Area 0*") nil)
       ((equal bufname "*Echo Area 1*") nil)
       ((equal bufname "*which-key*") nil)
       ((equal bufname "*Help*") nil)
       ((equal bufname "*Minibuf-0*") nil)
       ((equal bufname "*Minibuf-1*") nil)
       ((equal bufname "*Minibuf-2*") nil)
       ((equal bufname "*NeoTree*") nil)
       (t t))))
    (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
	(cond
	 ((or (string-equal "*" (substring (buffer-name) 0 1))
	      (memq major-mode '(magit-process-mode
				 magit-status-mode
				 magit-diff-mode
				 magit-log-mode
				 magit-file-mode
				 magit-blob-mode
				 magit-blame-mode
				 )))
	  "Emacs")
	 ((derived-mode-p 'prog-mode)
	  "Editing")
	 ((derived-mode-p 'dired-mode)
	  "Dired")
	 ((memq major-mode '(helpful-mode
			     help-mode))
	  "Help")
	 ((memq major-mode '(org-mode
			     org-agenda-clockreport-mode
			     org-src-mode
			     org-agenda-mode
			     org-beamer-mode
			     org-indent-mode
			     org-bullets-mode
			     org-cdlatex-mode
			     org-agenda-log-mode
			     diary-mode))
	  "OrgMode")
	 (t
	  (seq-filter (lambda (element) (not-internal-buffer element)) (buffer-list))))))
)

(provide 'nog-package-neotree)

;;; neotree.el ends here
