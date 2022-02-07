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
(defvar nog-font-size 16)
(defvar nog-preferred-font "Iosevka")

(defvar nog-font (concat nog-preferred-font "-" (number-to-string nog-font-size)))

(defun font-exists-p (font)
  "Check if font exists"
  (if (null (x-list-fonts font)) nil t))

(unless (not (font-exists-p nog-font))
  (set-frame-font nog-font nil t)
  (setq default-frame-alist (list (cons 'font nog-font))))

(unless (font-exists-p nog-font)
  (if (eq system-type 'darwin)
      ; for OS X if true
      (let ((system-font (concat "Menlo-" (number-to-string nog-font-size))))
	(set-frame-font system-font nil t)
	(setq default-frame-alist '((font . system-font))))
      ; else
      (let ((system-font (concat "Consolas-" (number-to-string nog-font-size))))
	(set-frame-font system-font nil t)
	(setq default-frame-alist '((font . system-font))))
  ))

(global-display-line-numbers-mode t)
(setq exec-path (append exec-path '("/Users/nog/.nvm/versions/node/v16.4.0/bin/" "/opt/homebrew/bin/")))
(add-to-list 'default-frame-alist '(height . 28)) ;; Vertical frame size
(add-to-list 'default-frame-alist '(width . 82)) ;; Horizontal frame size
(require 'subr-x)
(require 'ansi-color)

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
  (evil-add-command-properties #'nog-shell-command-output-to-buffer :jump t)
  (evil-mode))

(use-package org
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)))) ; this line activates dot

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
  :config (doom-modeline-mode 1))

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
  (defun not-internal-buffer (buffer)
    (let ((bufname (string-trim (buffer-name buffer))))
      (cond
       ((equal bufname "*scratch*") nil)
       ((equal bufname "*Backtrace*") nil)
       ((equal bufname "*Messages*") nil)
       ((equal bufname "*Buffer List*") nil)
       ((equal bufname "*terminal*") nil)
       ((equal bufname "*tide-server*") nil)
       ((equal bufname "*quelpa-build-checkout*") nil)
       ((equal bufname "*code-conversion-work*") nil)
       ((equal bufname "*Echo Area 0*") nil)
       ((equal bufname "*Echo Area 1*") nil)
       ((equal bufname "*which-key*") nil)
       ((equal bufname "*Help*") nil)
       ((equal bufname "*Minibuf-0*") nil)
       ((equal bufname "*Minibuf-1*") nil)
       ((equal bufname "*Minibuf-2*") nil)
       ((equal bufname "*NeoTree*") nil)
       (t t))))
    (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
	(cond
	 ((or (string-equal "*" (substring (buffer-name) 0 1))
	      (memq major-mode '(magit-process-mode
				 magit-status-mode
				 magit-diff-mode
				 magit-log-mode
				 magit-file-mode
				 magit-blob-mode
				 magit-blame-mode
				 )))
	  "Emacs")
	 ((derived-mode-p 'prog-mode)
	  "Editing")
	 ((derived-mode-p 'dired-mode)
	  "Dired")
	 ((memq major-mode '(helpful-mode
			     help-mode))
	  "Help")
	 ((memq major-mode '(org-mode
			     org-agenda-clockreport-mode
			     org-src-mode
			     org-agenda-mode
			     org-beamer-mode
			     org-indent-mode
			     org-bullets-mode
			     org-cdlatex-mode
			     org-agenda-log-mode
			     diary-mode))
	  "OrgMode")
	 (t
	  (seq-filter (lambda (element) (not-internal-buffer element)) (buffer-list))))))
)

(use-package find-file-in-project)

(unless (eq system-type 'darwin)
  (use-package good-scroll
    :quelpa (good-scroll :fetcher github :repo "io12/good-scroll.el")
    :config
    (good-scroll-mode 1)))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t))

(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/Cellar/mermaid-cli/8.11.0/bin/mmdc"))

(use-package better-jumper
  :requires evil
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward)))

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

(defun nog-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun nog-shell-command-output-to-buffer (command)
  "Same as nog-insert-shell-command-output but to a temp buffer instead"
  (interactive)
  (pop-to-buffer (get-buffer-create "*Shell Command Output*"))
  (erase-buffer)
  (insert (shell-command-to-string command))
  (beginning-of-buffer)
  (nog-display-ansi-colors))

(add-hook 'nog-shell-command-output-to-buffer 'nog-rename-shell-buffer)

(defun nog-run-file-through-command ()
  "Get current filename format, based on that format choose which command to run
  (output will be inserted 2 lines below, with provided prefix, if non-nil)"
  (interactive)
  (let (
	(format (nog-get-filename-format (buffer-file-name)))
       )
    (cond
     ((equal format "lisp") (nog-insert-shell-command-output (concat "sbcl --script " (buffer-file-name)) ";; "))
     ((equal format "ts") (nog-shell-command-output-to-buffer (concat "deno run --allow-read --quiet " (buffer-file-name))))
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
(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)


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
 '(custom-safe-themes
   '("0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))
 '(package-selected-packages
   '(better-jumper ob-mermaid lsp-treemacs lsp-ivy lsp-ui lsp-mode slime centaur-tabs good-scroll doom-modeline quelpa-use-package quelpa doom-themes one-themes tide find-file-in-project all-the-icons flycheck-rust cargo racer rust-mode neotree smartparens which-key flycheck company ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
