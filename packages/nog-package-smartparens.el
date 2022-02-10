;;; smartparens.el --- smartparens
;;;
;;; Commentary:
;;
;; sets up smartparens
;;
;;; Code:

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))

(provide 'nog-package-smartparens)

;;; smartparens.el ends here
