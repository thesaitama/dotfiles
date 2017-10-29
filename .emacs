
;;/opt/local/share/emacs/site-lisp/

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;list-packages
;;package-list-packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)


;; auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; 文字コード設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; OSX用設定
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

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; バックアップファイルを作成しない
(setq backup-inhibited t)

;; オープニングメッセージ表示しない
(setq inhibit-startup-message t)

;; キーバインドの変更
(define-key global-map "\C-h" 'backward-delete-char)
(define-key global-map "\C-c l" 'toggle-truncate-lines)
(define-key global-map "\C-t" 'other-window)

;; フレームの初期化
(setq initial-frame-alist
  (append (list
   '(border-color . "black")
   '(mouse-color . "black")
   '(menu-bar-lines . 1)
   )
  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 文字色の設定
(global-font-lock-mode t)
(set-face-foreground 'font-lock-type-face "darkyellow")
(set-face-foreground 'font-lock-builtin-face "blue") ; 組み込み定数
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
(set-face-foreground 'isearch "black") ; 検索文字列
(set-face-background 'isearch "lightpink") ; 検索文字列
(set-face-foreground 'isearch-lazy-highlight-face "black")
(set-face-background 'isearch-lazy-highlight-face "cyan")
(set-face-foreground 'minibuffer-prompt "blue") ; ミニバッファ

;; 編集行を目立たせる
;(defface hlline-face '(
;	(((class color) (background dark))  (:background "#aaa"))
;    (((class color) (background light)) (:background "#111"))
;    (tool-bar     ()))
;  "*Face used by hl-line.")
;(setq hl-line-face 'hlline-face)
;(global-hl-line-mode)

;; タイトルバーに表示する文字列
(setq frame-title-format
  (concat "%b - emacs@" system-name))

;; ツールバーを表示しない
(setq tool-bar-mode 0)

;; メニューバーを表示しない
(menu-bar-mode -1)

;; C-SPC で色を付ける
(setq transient-mark-mode t)

;; modeline
(line-number-mode t) ; 行番号
(column-number-mode t) ; カラム番号
(size-indication-mode t);
;(display-battery-mode t);

;; line number
(global-linum-mode 0)
(set-face-attribute 'linum nil
	:foreground "aaa"
	:height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d ")

;; 対の括弧を明示する
(show-paren-mode t)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")

;; 全角スペースとかに色を付ける
(defface my-face-b-1 '((t (:background "lightyellow"))) nil)
(defface my-face-b-2 '((t (:background "lightgray"))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
(font-lock-add-keywords
 major-mode
 '(
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

;; emacs-w3m 設定
;(require 'w3m-load)
;(setq w3m-use-cookies t)
;(setq w3m-cookie-accept-bad-cookies t)
;(setq w3m-add-referer t)

;; タブの設定
(setq-default tab-width 4)
(setq tab-width 4)

;;タブは4文字ごとに
;;追加　タブの設定は以下のようにしないとだめ
(setq-default tab-stop-list
  '(0 4 8 12 16 20 24))
(setq indent-tabs-mode t)

;; C-mode
(add-hook 'c-mode-hook '(lambda ()
	  (setq c-basic-offset 4)
	  (setq tab-width 4)
	  ;(c-set-style "bsd")
	  ;(setq c-auto-newline t)
)t)

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

;; mmm-mode (html)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\.html\\'" 'embedded-css)

;; css-mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))



;; psgml
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; xml-mode (RELAX, RELAX NG, iht)
(setq auto-mode-alist
      (append
       '(("\\.\\(xml\\|rlx\\|plm\\|rng\\|iht\\)$" . xml-mode))
       auto-mode-alist))

;; html-mode (xhtml, html)
(setq auto-mode-alist
      (append
       '(("\\.x?html\\([.]?\\w+\\)*$" . html-mode))
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
 '(package-selected-packages (quote (w3m mmm-mode helm ##)))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty Diminished" :foundry "outline" :slant normal :weight normal :height 150 :width normal)))))

;; helm
(require 'helm-config)
(helm-mode 1)
