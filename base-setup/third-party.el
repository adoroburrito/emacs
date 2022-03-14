;;; third-party.el --- setup third party scripts
;;;
;;; Commentary:
;;
;; This file is meant to setup third party scripts
;;
;;; Code:

(defvar missing-packages-list2 nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "[Third party] Checking for script `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "[Third party] Checking for script `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "[Third party] Checking for script `%s'... Missing" feature)
       (add-to-list 'missing-packages-list2 feature))
     nil)))

;; start importing packages mercilessly
(add-to-list 'load-path (expand-file-name "third-party" user-emacs-directory))

(try-require 'third-party-evil-collection-sly)

;;; third-party.el ends here
