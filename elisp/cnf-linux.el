;;; cnf-linux.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Linux)

;;; Code:

;; ------------------------------------------------------------------------
;; elpy (python-mode) for Linux

(defun use-system-python2 ()
  "Use system python2 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython2")
  ;; (setq python-check-command "<path_to>/pyflakes")
  (setq elpy-rpc-python-command "python2")
  ;; (setq elpy-rpc-pythonpath  "<path_to>/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

(defun use-system-python3 ()
  "Use system python3 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython3")
  ;; (setq python-check-command "<path_to>/pyflakes")
  (setq elpy-rpc-python-command "python3")
  ;; (setq elpy-rpc-pythonpath  "<path_to>/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

;; set start python3
(use-system-python3)

;; ------------------------------------------------------------------------

(provide 'cnf-linux.el)
;;; cnf-linux.el ends here
