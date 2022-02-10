;;; org.el --- org mode
;;;
;;; Commentary:
;;
;; sets up org mode
;;
;;; Code:

(use-package org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)))) ; this line activates dot

(provide 'nog-package-org)

;;; org.el ends here
