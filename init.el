;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Setting up basic configs ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-frame-font "Cascadia Code-12" nil t)
(setq default-frame-alist '((font . "Cascadia Code-18")))
(global-display-line-numbers-mode t)
(setq exec-path (append exec-path '("/Users/nog/.nvm/versions/node/v16.4.0/bin/")))
(add-to-list 'default-frame-alist '(height . 28)) ;; Vertical frame size
(add-to-list 'default-frame-alist '(width . 82)) ;; Horizontal frame size
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; Setting up package manager ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun nog-reload ()
  "Tell emacs to read the init.el file again"
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(defun nog-packages-reinstall ()
  "Remove elpa directory and tell emacs to read the init.el file again"
  (interactive)
  (delete-directory "~/.emacs.d/elpa" t)
  (nog-reload)
)


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

;;;;;;;;;;;;;;
;;          ;;
;; Packages ;;
;;          ;;
;;;;;;;;;;;;;;

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (add-to-list 'evil-emacs-state-modes 'nav-mode)
  (evil-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

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
  (setq neo-theme 'arrow)
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
)

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

(use-package good-scroll
  :quelpa (good-scroll :fetcher github :repo "io12/good-scroll.el")
  :config
  (good-scroll-mode 1)
)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t))

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

(defun nog-windows-path (path)
  "Format unix path so windows can understand it"
  (interactive)
  (replace-regexp-in-string "/" "\\\\" path)
)

(defun nog-get-filename-format (filename)
  "Split string by dot (.) and get last element of the list"
  (string-trim (car (last (split-string filename "\\.")))))

(defun nog-insert-shell-command-output (command prefix)
  "Execute shell command, get result, move to end of line,
  insert 2 newlines with the result of the command, prefixed if
  prefix was given"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (newline-and-indent)
  (if prefix
      (insert (concat prefix (shell-command-to-string command)))
    (insert (shell-command-to-string command))))

(defun nog-run-file-through-command ()
  "Get current filename format, based on that format choose which command to run
  (output will be inserted 2 lines below, with provided prefix, if non-nil)"
  (interactive)
  (let (
	(format (nog-get-filename-format (buffer-file-name)))
       )
    (cond
     ((equal format "lisp") (nog-insert-shell-command-output (concat "sbcl --script " (buffer-file-name)) ";; "))
     (t (message (concat "Can't run program for current file! Unknown format: " format)))
    )
  )
)

;;;;;;;;;;;;;;;;;
;;             ;;
;; Keybindings ;;
;;             ;;
;;;;;;;;;;;;;;;;;

(global-set-key [f8] 'neotree-project-dir)
(global-set-key [f12] (lambda ()
			(interactive)
			(find-file "~/.emacs.d/init.el")))
(global-set-key [f7] 'nog-run-file-through-command)
(global-set-key (kbd "C-c r") (lambda ()
				(interactive)
				(nog-reload)))
(global-set-key (kbd "C-c C-r") (lambda ()
				  (interactive)
				  (nog-packages-reinstall)))
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

(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

(global-set-key [next] #'good-scroll-up-full-screen)
(global-set-key [prior] #'good-scroll-down-full-screen)

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
   '(slime centaur-tabs good-scroll doom-modeline quelpa-use-package quelpa doom-themes one-themes tide find-file-in-project all-the-icons flycheck-rust cargo racer rust-mode neotree smartparens which-key flycheck company ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
