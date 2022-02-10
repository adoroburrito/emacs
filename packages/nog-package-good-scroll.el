;;; good-scroll.el --- good scroll on windows
;;;
;;; Commentary:
;;
;; sets up good-scroll on windows
;;
;;; Code:

(unless (eq system-type 'darwin)
  (use-package good-scroll
    :quelpa (good-scroll :fetcher github :repo "io12/good-scroll.el")
    :config
    (good-scroll-mode 1)))

(provide 'nog-package-good-scroll)

;;; good-scroll.el ends here
