;;; cnf-helm.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; helm

;; (require 'helm)
(require 'helm-config)
(helm-mode +1)

(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-c h") 'helm-mini)
(define-key global-map (kbd "C-c i") 'helm-imenu)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)

(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer."
  (kill-new (buffer-substring (point) (field-end))))
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists."
  (when (file-exists-p candidate) ad-do-it))
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))
;; hide directory ..
(advice-add 'helm-ff-filter-candidate-one-by-one
        :around (lambda (fcn file)
                  (unless (string-match "\\(?:/\\|\\`\\)\\.\\{2\\}\\'" file)
                    (funcall fcn file))))

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-projectile-files-list))

;; fuzzy match
(setq helm-M-x-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(setq helm-split-window-inside-p t)

;; auto resize
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

;; ------------------------------------------------------------------------
;; helm-smex

;; (require 'helm-smex)
(autoload 'helm-smex "ihelm-smex" nil t)
(global-set-key [remap execute-extended-command] #'helm-smex)
(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

;; ------------------------------------------------------------------------
;; helm-swoop

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
(setq helm-swoop-split-with-multiple-windows nil)
(setq helm-swoop-split-direction 'split-window-vertically)

;; ------------------------------------------------------------------------
;; helm-ag

(require 'helm-files)
(require 'helm-ag)
(setq helm-ag-base-command "ag --nogroup --ignore-case --hidden")
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "C-M-s") 'helm-ag-this-file)

;; ------------------------------------------------------------------------
;; helm-elscreen

(global-set-key (kbd "M-g e") 'helm-elscreen)

;; ------------------------------------------------------------------------
;; helm-flyspell

(global-set-key (kbd "M-g M-c") 'helm-flyspell-correct)

;; ------------------------------------------------------------------------
;; bm, helm-bm

(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hook 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

(require 'helm-bm)
(setq helm-source-bm (delete '(multiline) helm-source-bm))

(defun bm-toggle-or-helm ()
  "When 2 times load run helm-bm."
  (interactive)
  (bm-toggle)
  (when (eq last-command 'bm-toggle-or-helm)
    (helm-bm)))
(global-set-key (kbd "M-SPC") 'bm-toggle-or-helm)

;; bug ?
;; (require 'compile)

;; ------------------------------------------------------------------------
;; helm-dash

;;(setq helm-dash-browser-func 'eww)
(setq helm-dash-min-lengh 0)
(define-key global-map (kbd "M-g d") 'helm-dash-at-point)

;; ------------------------------------------------------------------------
;; id-manager

(autoload 'id-manager "id-manager" nil t)
(global-set-key (kbd "M-7") 'id-manager)
(setenv "GPG_AGENT_INFO" nil)

;; ------------------------------------------------------------------------

(provide 'cnf-helm.el)
;;; cnf-helm.el ends here
