;;; nog-keybindings.el --- keybindings nog made or copied from others
;;;
;;; Commentary:
;;
;; Keybindings i've made or copied from the web
;;
;;; Code:

(global-set-key (kbd "C-c h") 'quick-help-prompt)
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
(global-set-key (kbd "C-c h") 'company-complete) (global-set-key (kbd "C-c b") 'ivy-switch-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


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

;;; nog-keybindings.el ends here
