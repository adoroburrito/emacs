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
(setq make-backup-files nil) ;; disable backup files (*~, #*)

;; enforce unix style for EVERYTHANG
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; auto refresh files
(global-auto-revert-mode t)

;; font configs
(defvar nog-font-size 12)
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
;; to-do: fix this
;;(unless (font-exists-p nog-font)
  ;;(if (eq system-type 'darwin)
      ;; for OS X if true
      ;;(let ((system-font (concat "Menlo-" (number-to-string nog-font-size))))
	;;(set-frame-font system-font nil t)
	;;(setq default-frame-alist '((font . system-font))))
      ;;; else
      ;;(let ((system-font (concat "Consolas-" (number-to-string nog-font-size))))
	;;(set-frame-font system-font nil t)
	;;(setq default-frame-alist '((font . system-font))))
  ;;))

;; add timestamp to messages buffer
;; copied from: https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (sh/current-time-microseconds) " ")))))

(advice-add 'message :before 'sh/ad-timestamp-message)

;; adding paths for executables
(setq exec-path (append exec-path '("/Users/nog/.nvm/versions/node/v16.4.0/bin/" "/opt/homebrew/bin/")))

(when (equal window-system 'w32)
  (setq server-use-tcp t))

; Below needs to be set before you require 'server
(setq server-auth-dir
      (let ((dir (concat user-emacs-directory
                         "server_" (format "%s_%s"
                                           emacs-major-version
                                           emacs-minor-version)
                         "_" (system-name) ; Use the var `system-name' directly
                                        ; if using emacs older than 25.1.
                         "/")))
        (make-directory dir :parents)
        dir))

(with-eval-after-load 'server
  (when (equal window-system 'w32)
    ;; Suppress error "directory  ~/.emacs.d/server is unsafe". It is needed
    ;; needed for the server to start on Windows.
    (defun server-ensure-safe-dir (dir) "Noop" t)))

(require 'server)
;; Start a server if (server-running-p) does not return t (e.g. if it
;; returns nil or :other)
(or (eq (server-running-p) t)
    (server-start))
