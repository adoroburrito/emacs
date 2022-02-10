;;; company.el --- ivy
;;;
;;; Commentary:
;;
;; sets up company
;;
;;; Code:

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'nog-package-company)

;;; company.el ends here
