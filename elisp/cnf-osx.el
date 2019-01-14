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

;; for Terminal
(defun copy-from-osx-term ()
  "Handle copy intelligently on osx term."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (file-exists-p pbpaste)
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx-term (text &optional push)
  "Handle paste TEXT (PUSH will ignore) intelligently on osx term."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; for GUI
(defun paste-to-osx-gui (text &rest push)
  "Handle paste TEXT (PUSH will ignore) intelligently on osx."
  (if (display-graphic-p)
      (gui-select-text text)
    (osx-clipboard-cut-function text)))

(defun copy-from-osx-gui ()
  "Handle copy intelligently on osx."
  (if (display-graphic-p)
      (gui-selection-value)
    (osx-clipboard-paste-function)))

(setq interprogram-cut-function 'paste-to-osx-term)
(setq interprogram-paste-function 'copy-from-osx-term)

(when (eq window-system 'ns)
  (setq interprogram-cut-function 'paste-to-osx-gui)
  (setq interprogram-paste-function 'copy-from-osx-gui)
  )

;; ------------------------------------------------------------------------
;; mac dired

(defun open-mac (path)
  "Open specified PATH with open."
  (start-process "dired-open-mac" nil "open" path))

(defun quicklook-file (path)
  "Open QuickLook by PATH."
  (interactive)
  (defvar cur nil)
  (defvar old nil)
  (setq old cur)
  (setq cur (start-process "ql-file" nil "qlmanage" "-p" path))
  (when old (delete-process old)))

(defun my-dired-open-osx ()
  "Open by dired (OSX)."
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
          '(lambda()
             (define-key dired-mode-map "o" 'my-dired-open-osx)))

;; ------------------------------------------------------------------------
;; osx-trash

(with-eval-after-load "dired"
  (set-variable 'dired-use-ls-dired nil)
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

  (set-clipboard-coding-system 'utf-8) ;; clip board

  ;; mode line
  (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "Gray30"))
  (set-face-attribute 'mode-line-inactive nil :box '(:line-width 1 :color "Gray25"))

  ;; font
  (let* ((size 13)
         (asciifont "Menlo")
         (jpfont "Ricty Diminished for Powerline")
         (jpfont-hira "Hiragino Maru Gothic Pro")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont))
         (jp-fontspec-hira (font-spec :family jpfont-hira))
         (osaka-fontspec (font-spec :family "Osaka"))
         )
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; half-width
  (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; ext-Latin
  (set-fontset-font nil '(#x0370 . #x03FF) jp-fontspec) ; Greek
  (set-fontset-font nil '#xFF0B osaka-fontspec) ; ＋
  (set-fontset-font nil '#x2212 osaka-fontspec) ; −
  (set-fontset-font nil '#x00B1 osaka-fontspec) ; ±
  (set-fontset-font nil '#x00D7 osaka-fontspec) ; ×
  (set-fontset-font nil '#x00F7 osaka-fontspec) ; ÷
  )

  (dolist (elt '(("^-apple-hiragino.*" . 1.3)
                 (".*Ricty Diminished for Powerline.*" . 1.3)
                 (".*osaka-bold.*" . 1.4)
                 (".*osaka-medium.*" . 1.4)
                 (".*courier-bold-.*-mac-roman" . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman" . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))

  ;; font list Snippet
  ;; (dolist (x (x-list-fonts "*")) (print x))

  ;; 123456789012345678901234567890
  ;; ABCDEFGHIJKLMNOPQRSTUVWXYZabcd
  ;; あいうえおかきくけこさしすせそ
  ;; ◎○●▲■◎○●▲■◎○●▲■
  ;; ×÷±＋−×÷±＋−×÷±＋−
  ;; 123456789012345678901234567890
  ;; ΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγ

  (setq-default line-spacing 0.3)

  ;; mac-font
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing t)

  ;; prevent IM flicker (add-hook)
  (setq redisplay-dont-pause nil)

  ;; key
  (setq ns-command-modifier (quote super))
  (setq ns-alternate-modifier (quote meta))

  ;; menu-bar
  (menu-bar-mode 1)

  (when (functionp 'mac-auto-ascii-mode)
    (mac-auto-ascii-mode 1))

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
;; Java

(setenv "JDK_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home")
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home")

;; ------------------------------------------------------------------------
;; plantuml-mode

(set-variable 'plantuml-jar-path "/opt/local/share/java/plantuml.jar")  ;; depends on OS

;; ------------------------------------------------------------------------

(provide 'cnf-osx.el)
;;; cnf-osx.el ends here
