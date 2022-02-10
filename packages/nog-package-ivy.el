;;; ivy.el --- ivy
;;;
;;; Commentary:
;;
;; sets up ivy
;;
;;; Code:

(use-package ivy
  :config
  (ivy-mode 1) ; globally at startup
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(provide 'nog-package-ivy)

;;; ivy.el ends here
