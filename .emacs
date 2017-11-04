
;; thesaitama .emacs

;; MacPorts install Path
;; /opt/local/share/emacs/site-lisp/

;; Install
;; * auto-complete
;; * rainbow-mode
;; * rainbow-delimiters
;; * php-mode
;; * js2-mode
;; * mmm-mode
;; * python-mode
;; * jedi
;; * flycheck
;; * helm
;; * yasnippet
;; * yasnippet-snippets
;; * helm-c-yasnippet
;; * magit
;; * neotree

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;list-packages
;;package-list-packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; auto-install
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Character Code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; Auto reload bufffer
(global-auto-revert-mode 1)

;; Key Bind
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window)
(global-set-key (kbd "C-x C-b") 'bs-show) ;; replace list-buffers

;; wdired
(add-hook 'dired-load-hook (lambda ()
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

;; Mouse
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 3)))

;; recentf
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))
(setq-default find-file-visit-truename t)

;; Display Image File
(auto-image-file-mode t)

;; Auto Compression
(auto-compression-mode t)

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode) ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode) ;; fundamental-mode
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t) ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t) ;; 曖昧マッチ

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; macOSX
(when (eq system-type 'darwin)
  ;; ucs normalize
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

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
)

;; End of Line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; Startup Message
(setq inhibit-startup-message t)

;; Display function
(which-function-mode 1)

;; Backup file
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; Frame
(setq initial-frame-alist
  (append (list
   '(border-color . "black")
   '(mouse-color . "black")
   '(menu-bar-lines . 1)
   )
  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; Color
(global-font-lock-mode t)
(set-face-foreground 'font-lock-type-face "darkyellow")
(set-face-foreground 'font-lock-builtin-face "blue")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-string-face "darkorange")
(set-face-foreground 'font-lock-keyword-face "blue")
(set-face-foreground 'font-lock-function-name-face "blue") ; lightskyblue
(set-face-foreground 'font-lock-variable-name-face "goldenrod")
(set-face-foreground 'font-lock-constant-face "darkblue") ; aquamarine
(set-face-foreground 'font-lock-preprocessor-face "darkyellow")
(set-face-foreground 'font-lock-warning-face "pink")
(set-face-foreground 'tool-bar "cyan")
(set-face-background 'region "lightblue")
(set-face-foreground 'isearch "black")
(set-face-background 'isearch "lightpink")
(set-face-foreground 'isearch-lazy-highlight-face "black")
(set-face-background 'isearch-lazy-highlight-face "cyan")
(set-face-foreground 'minibuffer-prompt "blue")
;; Color markdown-mode
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold))))
 '(markdown-pre-face ((t (:inherit org-formula))))
 )

;; rainbow-mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; Highlight editing line
;; (defface hlline-face '(
;;   (((class color) (background dark))  (:background "#666"))
;;   (((class color) (background light)) (:background "#eee"))
;;   (tool-bar     ()))
;;   "*Face used by hl-line.")
;; (setq hl-line-face 'hlline-face)
;; (global-hl-line-mode)

;; Title bar caracter
(setq frame-title-format (concat "%b - emacs@" system-name))

;; Tool bar
(setq tool-bar-mode 0)

;; Menu bar
(menu-bar-mode -1)

;; Region Display
(setq transient-mark-mode t)

;; modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Line Number
(global-linum-mode 0)
(set-face-attribute 'linum nil
  :foreground "aaa"
  :background "666"
  :height 0.9)
(setq linum-format "%4d ")

;; Paren match
(show-paren-mode t)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")

;; Colored white spaces
(defface my-face-b-1 '((t (:background "lightyellow"))) nil)
(defface my-face-b-2 '((t (:background "darkbray"))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode(before my-font-lock-mode ())
(font-lock-add-keywords
 major-mode '(
   ("　" 0 my-face-b-1 append)
   ("\t" 0 my-face-b-2 append)
   ("[ ]+$" 0 my-face-u-1 append)
   )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
	(if font-lock-mode
	nil
	(font-lock-mode t))))

;; w3m
(require 'w3m)
(eval-after-load "w3m-search"
  '(add-to-list 'w3m-search-engine-alist
		'("google" "https://encrypted.google.com/search?num=100&ie=utf-8&oe=utf-8&hl=ja&safe=off&filter=0&pws=0&complete=0&gbv=1&q=%s" utf-8)))
(setq w3m-search-default-engine "google")
(setq w3m-home-page "http://www.google.co.jp")
(setq w3m-use-cookies t)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list '(0 4 8 12 16 20 24 28 32))

;; php-mode
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
  '(lambda ()
     (setq php-manual-path "/usr/share/doc/php/html")
     (setq php-search-url "http://www.phppro.jp/")
     (setq php-manual-url "http://www.phppro.jp/phpmanual")
     ))

;; mmm-mode (html, php)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-css)
(mmm-add-classes
 '((embedded-js
    :submode javascript-mode
    :front "<script[^>]*>"
    :back "</script>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-js)

;; javascript-mode
(autoload 'javascript-mode "javascript" "JavaScript mode" t)

;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; psgml
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; xml-mode (RELAX, RELAX NG, iht)
(setq auto-mode-alist
      (append '(("\\.\\(xml\\|rlx\\|pml\\|rng\\)$" . xml-mode))
       auto-mode-alist))

;; html-mode (xhtml, html)
(setq auto-mode-alist
      (append '(("\\(\\.x?html?\\|iht\\)\\([.]?\\w+\\)*$" . html-mode))
       auto-mode-alist))

;; python-mode
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (markdown-mode elscreen tabbar neotree magit python-info jedi-direx company-jedi navi2ch json-mode js2-mode helm-google sudo-edit helm-c-yasnippet yasnippet-snippets rainbow-delimiters yasnippet rainbow-mode flycheck python-mode jedi auto-complete w3m mmm-mode helm ##)))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

;M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional

;; dabbrev
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(set-face-attribute 'helm-selection nil :background "lightblue" :foreground "black")[<0;62;18M

;; yasnippet
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)

;; spell check (flyspell)
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; magit
(require 'magit)
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-blame)

;; neotree
(require 'neotree)
(setq neo-show-hidden-files t)

;; Font Style
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty Diminished" :foundry "outline" :slant normal :weight normal :height 150 :width normal)))))



