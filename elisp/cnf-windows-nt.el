;;; cnf-windows-nt.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Windows NT)

;;; Code:

;; ------------------------------------------------------------------------
;; GUI

(if window-system (progn

  ;; font
  (set-face-attribute 'default nil :family "MS Gothic" :height 100)
  ;; (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "MS Gothic"))
  (setq use-default-font-for-symbols nil)

  )
)

;; ------------------------------------------------------------------------
;; elpy (python-mode) for Windows

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
  (setq elpy-rpc-python-command "C:/Python36/python")
  (setq elpy-rpc-pythonpath  "C:/Python36/Lib/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

(defun use-anaconda-python3 ()
  "Use Anaconda python3 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "C:/ProgramData/Anaconda3/Scripts/ipython")
  (setq python-check-command "C:/ProgramData/Anaconda3/Scripts/pyflakes")
  (setq elpy-rpc-python-command "C:/ProgramData/Anaconda3/python3")
  (setq elpy-rpc-pythonpath  "C:/ProgramData/Anaconda3/Lib/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

;; set start python3
(use-anaconda-python3)

;; ------------------------------------------------------------------------
;; path

(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")

;; ------------------------------------------------------------------------
;; shell

;; (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
;; (setq shell-command-switch "-i")
;; (setq explicit-shell-file-name shell-file-name)
;; (setenv "PATH"
;;     (concat ".:/usr/local/bin:/mingw/bin:/bin:"
;;         (replace-regexp-in-string " " "\\\\ "
;;             (replace-regexp-in-string "\\\\" "/"
;;                 (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
;;                     (getenv "PATH"))))))

;; ------------------------------------------------------------------------

(provide 'cnf-windows-nt.el)
;;; cnf-windows-nt.el ends here
