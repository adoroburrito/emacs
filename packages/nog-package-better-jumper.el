;;; nog-package-better-jumper.el --- better jumper
;;;
;;; Commentary:
;;
;; sets up better-jumper
;;
;;; Code:

(use-package better-jumper
  :requires evil
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward)))


(provide 'nog-package-better-jumper)

;;; nog-package-better-jumper.el ends here
