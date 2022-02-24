;;; flycheck.el --- flycheck
;;;
;;; Commentary:
;;
;; sets up flycheck
;;
;;; Code:

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint typescript-tide typescript-tslint))))

(use-package flycheck-posframe
  :after (flycheck posframe)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  

(provide 'nog-package-flycheck)

;;; flycheck.el ends here
