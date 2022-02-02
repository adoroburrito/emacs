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
(set-frame-font "JetBrains Mono-18" nil t)
(setq exec-path (append exec-path '("/Users/nog/.nvm/versions/node/v16.4.0/bin/")))

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
  (add-to-list 'evil-emacs-state-modes 'nav-mode)
  (evil-mode))
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
(use-package all-the-icons)

(use-package find-file-in-project)

(use-package rust-mode)
(use-package racer)
(use-package company)
(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(use-package tide
  :config
  (progn
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode))
)

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Custom functions ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun open-in-project-dir ()
  (let ((project-dir (ffip-project-root))
	(file-name (buffer-file-name)))
    (if project-dir
	(progn
	  (neotree-dir project-dir)
	  (neotree-find file-name))
      (message "Could not find git project root."))))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (unless (boundp 'nog-neotree-open)
    (setq nog-neotree-open nil))

  (if (null nog-neotree-open)
      (progn
	(open-in-project-dir)
	(setq nog-neotree-open t))
    (progn
      (neotree-hide)
      (setq nog-neotree-open nil)))
  )

(defun nog-reload-packages ()
  "Remove elpa directory and tell emacs to read the init.el file again"
  (interactive)
  (delete-directory "~/.emacs.d/elpa" t)
  (load-file "~/.emacs.d/init.el")
  )

(defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))


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
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el ends here! ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(doom-themes one-themes tide find-file-in-project all-the-icons flycheck-rust cargo racer rust-mode neotree smartparens which-key flycheck company ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
