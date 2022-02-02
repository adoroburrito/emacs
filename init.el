;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Setting up basic configs ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'wombat)
(set-frame-font "JetBrains Mono-18" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; Setting up package manager ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; Bootstrapping "use-package" ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

;;;;;;;;;;;;;;
;;          ;;
;; Packages ;;
;;          ;;
;;;;;;;;;;;;;;

(use-package evil
  :config
  (evil-mode))

(use-package ivy
  :config
  (ivy-mode 1) ; globally at startup
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))

(use-package neotree
  :init
  (require 'neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  )

(use-package rust-mode)
(use-package racer)
(use-package company)
(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Custom functions ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
    (file-name (buffer-file-name)))
    (if project-dir
    (progn
      (neotree-dir project-dir)
      (neotree-find file-name))
      (message "Could not find git project root."))))

;;;;;;;;;;;;;;;;;
;;             ;;
;; Keybindings ;;
;;             ;;
;;;;;;;;;;;;;;;;;

(global-set-key [f8] 'neotree-project-dir)
(global-set-key (kbd "C-c q") (lambda ()
                       (interactive)
                   (other-window -1)))
(global-set-key (kbd "C-c h") 'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here! ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-rust cargo racer rust-mode neotree smartparens which-key flycheck company ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
