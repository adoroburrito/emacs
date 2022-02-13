;;; nog-package-fzf.el --- fzf frontend
;;;
;;; Commentary:
;;
;; fzf helper for some functions
;;
;;; Code:

(defun nog-package-fzf-unknown ()
    (global-set-key (kbd "C-c p") (lambda ()
	(interactive)
	(message "Fzf package is not setup correctly! Please check file '.emacs.d/packages/nog-package-fzf.el'")))

    (global-set-key (kbd "C-c a") (lambda ()
	(interactive)
	(message "Fzf package is not setup correctly! Please check file '.emacs.d/packages/nog-package-fzf.el'"))))

(defun nog-package-fzf-mac ()
  (message "Trying to setup fzf package for: mac")
  (if (not (executable-find "fzf"))
      (message "Failed to setup fzf for mac: fzf binary not found!")
    (use-package fzf
      :config
      (global-set-key (kbd "C-c p") 'fzf-git-files)
      (global-set-key (kbd "C-c a") 'fzf))))

(if (eq system-type 'darwin)
    (nog-package-fzf-mac)
  (progn
    (message (concat "Can't install fzf for system: " system-type ". Check file '.emacs.d/packages/nog-package-fzf.el'"))
    (nog-package-fzf-unknown)))

(provide 'nog-package-fzf)

;;; nog-package-fzf.el ends here
