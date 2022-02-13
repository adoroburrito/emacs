;;; company.el --- undo-tree
;;;
;;; Commentary:
;;
;; sets up undo-tree
;;
;;; Code:

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(provide 'nog-package-undo-tree)

;;; nog-package-undo-tree.el ends here
