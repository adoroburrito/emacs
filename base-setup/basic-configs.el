;;; basic-configs.el --- setup basic configs for emacs
;;;
;;; Commentary:
;;
;; These are the basic configs that don't rely on external packages or anything, it just sets up
;; default Emacs stuff with my liking.  Loading this file >should< work in any default Emacs
;;
;;; Code:

;; requiring modules we will need later
(require 'subr-x)
(require 'ansi-color)

;; basic default options
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode t)
(add-to-list 'default-frame-alist '(height . 28)) ;; Vertical frame size
(add-to-list 'default-frame-alist '(width . 82)) ;; Horizontal frame size

;; font configs
(defvar nog-font-size 16)
(defvar nog-preferred-font "Iosevka")
(defvar nog-font (concat nog-preferred-font "-" (number-to-string nog-font-size)))

(defun font-exists-p (font)
  "Check if font exists"
  (if (null (x-list-fonts font)) nil t))

;; if the font exists, use it
(unless (not (font-exists-p nog-font))
  (set-frame-font nog-font nil t)
  (setq default-frame-alist (list (cons 'font nog-font))))

;; if the font doesn't exists, use fallback fonts, according to the OS
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

;; adding paths for executables
(setq exec-path (append exec-path '("/Users/nog/.nvm/versions/node/v16.4.0/bin/" "/opt/homebrew/bin/")))
