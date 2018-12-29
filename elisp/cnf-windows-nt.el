;;; cnf-windows-nt.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration (for Windows NT)

;;; Code:

;; ------------------------------------------------------------------------
;; path

(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
;; (add-to-list 'exec-path "C:/Program Filee/PuTTY")
;; (add-to-list 'exec-path "C:/Program Files/Aspell/bin")

;; ------------------------------------------------------------------------
;; Windows configure

(setq default-directory "~/../../Desktop/")

;; coding
(set-file-name-coding-system 'cp932)    ;; file system
(set-clipboard-coding-system 'utf-16le) ;; clipboard

;; special keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; speedup?
(setq w32-get-true-file-attributes nil)

;; ------------------------------------------------------------------------
;; GUI

(if window-system
    (progn

      ;; font
      (set-face-attribute 'default nil :family "Consolas" :height 100)
      ;; (set-face-attribute 'default nil :family "Inconsolata" :height 120)
      ;; (set-face-attribute 'default nil :family "MS Gothic" :height 100)
      (set-fontset-font nil 'cp932 (font-spec :family "MS Gothic"))
      (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MS Gothic"))
      (setq use-default-font-for-symbols nil)
      (setq-default line-spacing 0.1)

      )
  )

;; ------------------------------------------------------------------------
;; for IME patched version

;; download: https://github.com/mhatta/emacs-26-x86_64-win-ime

(defun enable-ime ()
  (if (fboundp 'w32-ime-initialize)
      '(lambda ()
         ;; Windows IME
         (setq default-input-method "W32-IME")
         (setq-default w32-ime-mode-line-state-indicator "[--]")
         (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
         (w32-ime-initialize)
         ;; change cursor-color depend on IME status
         (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "Coral4")))
         (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "Black")))
         (setq w32-ime-composition-window nil)

         ;; IME setting
         (add-hook 'minibuffer-setup-hook 'deactivate-input-method)
         (add-hook
          'isearch-mode-hook '(lambda ()
                             (deactivate-input-method)
                             (setq w32-ime-composition-window (minibuffer-window))))
         (add-hook
          'isearch-mode-end-hook '(lambda ()
                             (setq w32-ime-composition-window nil)))
         (advice-add
          'helm :around '(lambda (orig-fun &rest args)
                           (let ((select-window-functions nil)
                                 (w32-ime-composition-window (minibuffer-window)))
                             (deactivate-input-method)
                             (apply orig-fun args))))
         )
    nil)
  )

(enable-ime)

;; ------------------------------------------------------------------------
;; win dired

(defun open-win (path)
  "Open specified PATH with shell-execute."
  (w32-shell-execute "open" path))

(defun my-dired-open-win ()
  "Open by dired (win)."
  (interactive)
  (cond ((file-accessible-directory-p (dired-get-file-for-visit))
         (call-interactively 'dired-find-alternate-file))
        (t
         (funcall 'open-win(dired-get-file-for-visit)))
        )
  )

(add-hook 'dired-mode-hook
          '(lambda()
             (define-key dired-mode-map "o" 'my-dired-open-win)))

;; ------------------------------------------------------------------------
;; Java

;; (setenv "JDK_HOME" "<path_to_jdk>")
;; (setenv "JAVA_HOME" "<path_to_jdk>")

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

(defun run-bash ()
  "Run bash."
  (interactive)
  (let ((shell-file-name "C:/Program Files/Git/bin/bash.exe"))
    (shell "*bash*")))

(defun run-powershell ()
  "Run PowerShell."
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
                       nil
                       nil))

(push '("cmdproxy\\.exe$" sjis-dos . sjis-dos) process-coding-system-alist)
(defun run-cmdexe ()
  "Set up an environment for cmd.exe, execute it, and swith to buffer *cmd*."
  (interactive)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "PATH" (replace-regexp-in-string "[^;]+cygwin[^;]+;" "" (getenv "PATH")))
    (set-buffer (get-buffer-create "*cmd*"))
    (shell (get-buffer "*cmd*"))
    (set (make-local-variable 'shell-file-name) "cmdproxy.exe")
    (set (make-local-variable 'explicit-shell-file-name) "cmdproxy.exe")
    (set (make-local-variable 'shell-name) "cmdproxy.exe")
    (set (make-local-variable 'shell-command-switch) "/c")
    (set (make-local-variable 'w32-quote-process-args) nil)
    (set (make-local-variable 'file-name-coding-system) 'sjis-dos)
    (set-buffer-file-coding-system 'sjis-dos)
    (set-buffer-process-coding-system 'sjis-dos 'sjis-dos)
    (auto-complete-mode t)))

(global-set-key (kbd "C-c s") 'run-cmdexe)

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
  (setq python-shell-interpreter "ipython")
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
  (setq elpy-rpc-python-command "C:/ProgramData/Anaconda3/python")
  (setq elpy-rpc-pythonpath  "C:/ProgramData/Anaconda3/Lib/site-packages")
  ;; (setq flycheck-python-flake8-executable "<path_to>/flake8")
  )

;; set start python3
(use-anaconda-python3)

;; ------------------------------------------------------------------------

(provide 'cnf-windows-nt.el)
;;; cnf-windows-nt.el ends here
