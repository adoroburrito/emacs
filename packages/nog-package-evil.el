;;; evil.el --- evil mode
;;;
;;; Commentary:
;;
;; sets up evil mode
;;
;;; Code:

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (add-to-list 'evil-emacs-state-modes 'nav-mode)
  (evil-add-command-properties #'nog-shell-command-output-to-buffer :jump t)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-mode))

(provide 'nog-package-evil)

;;; evil.el ends here
