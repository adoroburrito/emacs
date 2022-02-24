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

(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "[Package manager] Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "[Package manager] Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "[Package manager] Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature))
     nil)))

;; basics
(try-require 'nog-package-evil)
(try-require 'nog-package-undo-tree)
(try-require 'nog-package-ivy)
(try-require 'nog-package-company)
(try-require 'nog-package-which-key)
(try-require 'nog-package-smartparens)
(try-require 'nog-package-neotree)
(try-require 'nog-package-all-the-icons)
(try-require 'nog-package-ffip)
(try-require 'nog-package-centaur-tabs)

;; UI customization 
(try-require 'nog-package-doom-themes)
(try-require 'nog-package-doom-modeline)
(try-require 'nog-package-good-scroll)

;; everything else
(try-require 'nog-package-flycheck)
(try-require 'nog-package-tide)
(try-require 'nog-package-fzf)
(try-require 'nog-package-posframe)

;;org stuff
(try-require 'nog-package-ob-mermaid)
(try-require 'nog-package-ob-typescript)
(try-require 'nog-package-org)

;;; package-manager.el ends here
