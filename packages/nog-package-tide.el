;;; nog-package-tide.el --- tide
;;;
;;; Commentary:
;;
;; sets up tide
;;
;;; Code:

(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode))
  :config
  (setq company-tooltip-align-annotations t))

(provide 'nog-package-tide)

;;; nog-package-tide.el ends here
