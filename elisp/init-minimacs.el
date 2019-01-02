;;; init-minimancs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ init-minimacs.el
;; light weight and legacy Emacs setting
;;

;;; Code:

;; ------------------------------------------------------------------------

(setq initial-scratch-message ";; minimacs\n")

;; ------------------------------------------------------------------------
;; load basic settings

(load "~/dotfiles/elisp/cnf-basics.el")

;; ------------------------------------------------------------------------
;; load lisp for legacy Emacs

(setq load-path (append '("~/dotfiles/elisp/legacy") load-path))

;; ------------------------------------------------------------------------
;; ido

(ido-mode t)
(ido-everywhere t)
(set-variable 'ido-enable-flex-matching t)
(set-variable 'ido-use-faces t)
(set-variable 'ido-confirm-unique-completion t)
(set-variable 'ido-show-dot-for-dired t)

(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; can not work ido-ubiquitous in Emacs 22
;; (when (require 'ido-ubiquitous nil t)
;;   (ido-ubiquitous-mode 1)
;;   )

(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))
             ))

;; ------------------------------------------------------------------------
;; smex

(when (require 'smex nil t)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  )

;; ------------------------------------------------------------------------

(provide 'init-minimacs.el)
;;; init-minimacs.el ends here
