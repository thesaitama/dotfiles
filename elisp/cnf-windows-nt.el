;;; cnf-windows-nt.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Windows NT)

;;; Code:

;; ------------------------------------------------------------------------
;; GUI

(if window-system (progn
   (load-theme 'wombat t)
   (setq initial-frame-alist
     (append (list
              '(border-color . "#353535")
              '(mouse-color . "#f9f8f0")
              '(menu-bar-lines . 1)
              )
  initial-frame-alist))
  (setq default-frame-alist
     (append (list
              '(background-color . "#222222")
              '(foreground-color . "#f9f8f0")
              '(cursor-color . "#f9f8f0")
              )
       default-frame-alist)
     )
  ;; font-lock
  (set-face-foreground 'font-lock-type-face "#feb008")
  (set-face-foreground 'font-lock-builtin-face "#b998d2")
  (set-face-foreground 'font-lock-comment-face "#8ec46e")
  (set-face-foreground 'font-lock-comment-delimiter-face "#8ec46e")
  (set-face-foreground 'font-lock-string-face "#fd8507")
  (set-face-foreground 'font-lock-keyword-face "#5faadc")
  (set-face-foreground 'font-lock-function-name-face "#feb008")
  (set-face-foreground 'font-lock-variable-name-face "#dcbb23")
  (set-face-foreground 'font-lock-constant-face "#dcbb23")
  (set-face-foreground 'font-lock-preprocessor-face "#dcbb23")
  (set-face-foreground 'font-lock-warning-face "#daa0b5")
  (set-face-foreground 'tool-bar "#50a3b1")
  (set-face-background 'region "#b5dad9")
  (set-face-foreground 'isearch "#f9f8f0")
  (set-face-background 'isearch "#daa0b5")
  (set-face-foreground 'minibuffer-prompt "#69afde")
  (set-face-foreground 'fringe "#cccccc")
  (set-face-background 'fringe "#333333")
  (set-face-foreground 'mode-line "#777777")

  ;; font
  (set-face-attribute 'default nil :family "MS Gothic" :height 110)
  ;; (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "MS Gothic"))
  (setq use-default-font-for-symbols nil)

  ;; UI items
  (scroll-bar-mode 0)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

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
  (setq elpy-rpc-python-command "python3")
  ;; (setq elpy-rpc-pythonpath  "<path_to>/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

;; set start python3
(use-system-python3)

;; ------------------------------------------------------------------------

(provide 'cnf-windows-nt.el)
;;; cnf-windows-nt.el ends here
