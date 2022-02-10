;;; ob-mermaid.el --- ob-mermaid
;;;
;;; Commentary:
;;
;; sets up ob-mermaid
;;
;;; Code:

(use-package ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/Cellar/mermaid-cli/8.11.0/bin/mmdc"))

(provide 'nog-package-ob-mermaid)

;;; ob-mermaid.el ends here
