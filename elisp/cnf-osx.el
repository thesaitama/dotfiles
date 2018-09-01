;;; cnf-osx.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for macOS)

;;; Code:

;; ------------------------------------------------------------------------
;; ucs normalize

(when (require 'ucs-normalize nil t)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (setq file-name-coding-system 'utf-8-hfs)
  )

;; ------------------------------------------------------------------------
;; mac clipboard

;; can not work with tramp
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; ------------------------------------------------------------------------
;; mac quick-lock

(defun open-mac (path)
  (start-process "dired-open-mac" nil "open" path))

(defun quicklook-file (path)
  "Open QuickLook by PATH."
  (interactive)
  (defvar cur nil)
  (defvar old nil)
  (setq old cur)
  (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
  (when old (delete-process old)))

(defun my-dired-open ()
  "Open by dired."
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
            (call-interactively 'dired-find-file-other-window))
           )
     )
  )

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
;; (setq osx-dictionary-use-chinese-text-segmentation t)

;; Key bindings
;; (global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
;; (global-set-key (kbd "C-c i") 'osx-dictionary-search-input)

;; ------------------------------------------------------------------------
;; NS Window System (Mac Cocoa)

(when (eq window-system 'ns)

  ;; font
  (set-face-attribute 'default nil :family "Menlo" :height 130)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Maru Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Maru Gothic ProN.*" . 1.2))

  (setq-default line-spacing 0.3)

  ;; mac-font
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing t)

  ;; prevent IM flicker (add-hook)
  (setq redisplay-dont-pause nil)

  ;; key
  (setq ns-command-modifier (quote super))
  (setq ns-alternate-modifier (quote meta))
  )

;; ------------------------------------------------------------------------
;; elpy (python-mode) for macOS

(defun use-builtin-python2 ()
  "Use builtin python2 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "/usr/bin/python")
  (setq python-check-command
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/pyflakes")
  (setq elpy-rpc-python-command "/usr/bin/python")
  (setq elpy-rpc-pythonpath
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages")
  (setq flycheck-python-flake8-executable
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/flake8")
  )

(defun use-system-python2 ()
  "Use system python2 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython2")
  (setq python-check-command
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/pyflakes")
  (setq elpy-rpc-python-command "python2")
  (setq elpy-rpc-pythonpath
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages")
  (setq flycheck-python-flake8-executable
        "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/flake8")
  )

(defun use-system-python3 ()
  "Use system python3 for `elpy-mode`."
  (interactive)
  (setq python-shell-interpreter "ipython3")
  (setq python-check-command
        "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/bin/pyflakes")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-pythonpath
        "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages")
  (setq flycheck-python-flake8-executable
        "/opt/local/Library/Frameworks/Python.framework/Versions/3.6/bin/flake8")
  )

;; set start python3
(use-system-python3)

;; ------------------------------------------------------------------------
;; plantuml-mode

(setq plantuml-jar-path "/opt/local/share/java/plantuml.jar")  ;; depends on OS

;; ------------------------------------------------------------------------

(provide 'cnf-osx.el)
;;; cnf-osx.el ends here
