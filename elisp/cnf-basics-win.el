;;; cnf-basics-win.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; server

(require 'server)
(unless (eq (server-running-p) 't)
  (server-start)

  (defun iconify-emacs-when-server-is-done ()
    (unless server-clients (iconify-frame)))

  ;; change C-x C-c mapping
  (global-set-key (kbd "C-x C-c") 'server-edit)
  (defalias 'exit 'save-buffers-kill-emacs)
  ;; when load Emacs and iconify
  ;; (add-hook 'after-init-hook 'iconify-emacs-when-server-is-done)

  ;; confirm kill Emacs
  (setq confirm-kill-emacs 'yes-or-no-p)
  )

;; ------------------------------------------------------------------------
;; color scheme

(load-theme 'wombat t)

(setq initial-frame-alist
      (append (list
               '(border-color . "#353535")
               '(mouse-color . "#f9f8f0")
               '(internal-border-width . 0)
               )
              initial-frame-alist))
(setq default-frame-alist
      (append (list
               '(background-color . "#272727")
               '(foreground-color . "#f9f8f0")
               '(cursor-color . "#f9f8f0")
               )
              default-frame-alist))

;; window transparency
(set-frame-parameter nil 'alpha 95)

;; GUI background
(set-face-background 'default "#282828")

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
(set-face-background 'region "#626262")
(set-face-foreground 'isearch "#f9f8f0")
(set-face-background 'isearch "#daa0b5")
(set-face-foreground 'minibuffer-prompt "#69afde")
(set-face-foreground 'fringe "#cccccc")
(set-face-background 'fringe "#1a1a1a")
(set-face-foreground 'mode-line "#777777")
(set-face-background 'mode-line "#434944")

;; (set-face-background 'highlight-symbol-face "Gray30)"
(set-face-background 'hl-line "#303030")

;; ------------------------------------------------------------------------
;; clipboard

(setq x-select-enable-clipboard t)

;; ------------------------------------------------------------------------
;; (GUI) UX

(tool-bar-mode 0)
(scroll-bar-mode 0)

;; disable dialog-boxn
;; (setq use-dialog-boxn nil)
;; (defalias 'message-box 'message)

;; ------------------------------------------------------------------------
;; (GUI) key bind

(global-set-key (kbd "C-x C-b") 'bs-show)

(global-unset-key (kbd "C-t")) ;; important
(global-set-key (kbd "C-t l") 'windmove-right)
(global-set-key (kbd "C-t h") 'windmove-letf)
(global-set-key (kbd "C-t j") 'windmove-down)
(global-set-key (kbd "C-t k") 'windmove-up)

;; ------------------------------------------------------------------------

(provide 'cnf-basics-win.el)
;;; cnf-basics-win.el ends here
