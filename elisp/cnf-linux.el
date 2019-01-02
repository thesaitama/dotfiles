;;; cnf-linux.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Linux)

;;; Code:

;; ------------------------------------------------------------------------
;; linux dired

(defun open-linux (path)
  "Open specified PATH with xdg-open."
  (call-process "xdg-open" nil 0 nil path))

(defun my-dired-open-linux ()
  "Open by dired (linux)."
  (interactive)
  (cond ((file-accessible-directory-p (dired-get-file-for-visit))
         (call-interactively 'dired-find-alternate-file))
        (t
         (funcall 'open-linux (dired-get-file-for-visit)))
        )
  )

(add-hook 'dired-mode-hook
          '(lambda()
             (define-key dired-mode-map "o" 'my-dired-open-linux)))

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
;; xclip

(if (or (display-graphic-p)
  (eq (getenv "SSH_CONNECTION") nil)
 )
    (xclip-mode 1)
)

;; > sudo apt-get install xclip

;; ------------------------------------------------------------------------
;; plantuml-mode

(set-variable 'plantuml-jar-path "/usr/share/plantuml/plantuml.jar") ;; depends on OS

;; ------------------------------------------------------------------------

(provide 'cnf-linux.el)
;;; cnf-linux.el ends here
