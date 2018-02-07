;;; cnf-osx.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for macOS)

;;; Code:

;; ------------------------------------------------------------------------
;; ucs normalize

(require 'ucs-normalize)
(set-file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; ------------------------------------------------------------------------
;; clipboard

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(defun open-mac (path)
  (start-process "dired-open-mac" nil "open" path))

;; ------------------------------------------------------------------------
;; mac quick-lock

(defun quicklook-file (path)
  (interactive)
  (defvar cur nil)
  (defvar old nil)
  (setq old cur)
  (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
  (when old (delete-process old)))

(defun my-dired-open ()
  (interactive)
  (let ((exts-ql   '("jpeg" "jpg" "png" "gif" "tiff"))
        (exts-open '("avi" "mkv" "mp4" "mts" "psd" "ai" "pdf")))
     (cond ((file-accessible-directory-p (dired-get-file-for-visit))
            (call-interactively 'dired-find-alternate-file))
           ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-ql)
            (funcall 'quicklook-file (dired-get-file-for-visit)))
           ((member (downcase (file-name-extension (dired-get-file-for-visit))) exts-open)
            (funcall 'open-mac (dired-get-file-for-visit)))
           (t
            (call-interactively 'dired-find-file-other-window)))))
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "o" 'my-dired-open)))

;; ------------------------------------------------------------------------
;; osx-trash

(with-eval-after-load "dired"
  (setq dired-use-ls-dired nil)
  (when (require 'osx-trash nil t)
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup)))

;; ------------------------------------------------------------------------
;; osx-dictionary

;; Support Chinese word
;;(setq osx-dictionary-use-chinese-text-segmentation t)

;; Key bindings
;;(global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
;;(global-set-key (kbd "C-c i") 'osx-dictionary-search-input)

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
  (set-face-foreground 'isearch-lazy-highlight-face "#f9f8f0")
  (set-face-background 'isearch-lazy-highlight-face "#50a3b1")
  (set-face-foreground 'minibuffer-prompt "#69afde")
  (set-face-foreground 'fringe "#cccccc")
  (set-face-background 'fringe "#333333")
  (set-face-foreground 'mode-line "#777777")
  )
)

;; ------------------------------------------------------------------------
;; NS Window System (Mac Cocoa)

(when (eq window-system 'ns)
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 140) ;; 15pt
  (set-fontset-font nil 'japanese-jisx0208
                    (font-spec :family "Ricty Deminished for Powerline" :size 15))
  (setq face-font-rescale-alist
        '((".*Ricty Deminished for Powerline.*" . 1.2)))

  ;; key
  (setq ns-alternate-modifier (quote meta))
  )

;; ------------------------------------------------------------------------

(provide 'cnf-osx.el)
;;; cnf-osx.el ends here
