;;; init.el --- start point of my config
;;;
;;; Commentary:
;;
;; This file is meant to just set other files up, commenting what is happening throughout the way
;;
;;; Code:

(defun loading-message (to-load finished)
  (if (not finished)
      (message (concat "Loading '" to-load "' configs..."))
    (message (concat "Finished loading '" to-load "' configs."))))
  
;; load base configuration for Emacs
(loading-message "basic configs" nil)
(load (concat user-emacs-directory "base-setup/basic-configs.el"))
(loading-message "basic configs" t)

;; load package manager configuration and all packages
(loading-message "package manager" nil)
(load (concat user-emacs-directory "base-setup/package-manager.el"))
(loading-message "package manager" t)

;; load nog functions
(loading-message "nog functions" nil)
(load (concat user-emacs-directory "base-setup/nog-functions.el"))
(loading-message "nog functions" t)

;; load nog keybindings
(loading-message "nog keybindings" nil)
(load (concat user-emacs-directory "base-setup/nog-keybindings.el"))
(loading-message "nog keybindings" t)

;; load third party stuff
(loading-message "nog keybindings" nil)
(load (concat user-emacs-directory "base-setup/third-party.el"))
(loading-message "nog keybindings" t)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))
 '(package-selected-packages
   '(clojure-mode flycheck-posframe posframe magit fzf better-jumper ob-mermaid lsp-treemacs lsp-ivy lsp-ui lsp-mode slime centaur-tabs good-scroll doom-modeline quelpa-use-package quelpa doom-themes one-themes tide find-file-in-project all-the-icons flycheck-rust cargo racer rust-mode neotree smartparens which-key flycheck company ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
