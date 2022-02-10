;;; which-key.el --- which-key
;;;
;;; Commentary:
;;
;; sets up which-key
;;
;;; Code:

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.05)
  :diminish which-key-mode)

(provide 'nog-package-which-key)

;;; which-key.el ends here
