;;; cnf-linux.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Linux)

;;; Code:

;; ------------------------------------------------------------------------
;; elpy (python-mode) for linux

(defun use-system-python2 ()
  "Use system python2 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython2")
  ;; (setq python-check-command
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/pyflakes")
  (setq elpy-rpc-python-command "python2")
  ;; (setq elpy-rpc-pythonpath
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages")
  ;; (setq flycheck-python-flake8-executable
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/flake8")
  )

(defun use-system-python3 ()
  "Use system python3 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython3")
  ;; (setq python-check-command
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/bin/pyflakes")
  (setq elpy-rpc-python-command "python3")
  ;; (setq elpy-rpc-pythonpath
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages")
  ;; (setq flycheck-python-flake8-executable
  ;;       "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/bin/flake8")
  )

;; set start python3
(use-system-python3)

;; ------------------------------------------------------------------------

(provide 'cnf-linux.el)
;;; cnf-linux.el ends here
