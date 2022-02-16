;;; package-manager.el --- setup package manager with use package and load packages
;;;
;;; Commentary:
;;
;; This file is meant to setup use package and load all packages
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Bootstrapping "use-package"  ;;
;; and quelpa (for github pkgs) ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; start importing packages mercilessly
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))

;; critical to the workflow
(require 'nog-package-evil)
(require 'nog-package-undo-tree)
(require 'nog-package-org)
(require 'nog-package-ivy)
(require 'nog-package-company)
(require 'nog-package-which-key)
(require 'nog-package-smartparens)
(require 'nog-package-neotree)
(require 'nog-package-all-the-icons)
(require 'nog-package-ffip)
(require 'nog-package-centaur-tabs)

;; UI customization 
(require 'nog-package-doom-themes)
(require 'nog-package-doom-modeline)
(require 'nog-package-good-scroll)

;; everything else
(require 'nog-package-flycheck)
(require 'nog-package-tide)
(require 'nog-package-ob-mermaid)
(require 'nog-package-better-jumper)
(require 'nog-package-fzf)

;;; package-manager.el ends here
