
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

;; character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; auto reload bufffer
(global-auto-revert-mode 1)

;; key bind
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window)
(global-set-key (kbd "C-x C-b") 'bs-show) ;; replace list-buffers

;; mouse
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

;; display image file
(auto-image-file-mode t)

;; auto-compression
(auto-compression-mode t)

;; auto-complete
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

;; end of line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; startup message
(setq inhibit-startup-message t)

;; display function
(which-function-mode 1)

;; backup file
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; frame
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
;(global-hl-line-mode t)
;(custom-set-faces '(hl-line ((t (:background "color-236")))))

;; title-bar character
(setq frame-title-format (concat "%b - emacs@" system-name))

;; tool-bar
(setq tool-bar-mode 0)

;; menu-bar
(menu-bar-mode -1)

;; region display
(setq transient-mark-mode t)

;; modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; line number
(global-linum-mode 0)
(custom-set-faces
 '(linum ((t (:inherit (shadow default) :background "Gray23")))))
(setq linum-format "%4d ")

;; paren match
(show-paren-mode t)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")
(setq show-paren-style 'mixed)

;; colored white spaces
(defface my-face-b-1 '((t (:background "lightyellow"))) nil)
(defface my-face-b-2 '((t (:background "darkgray"))) nil)
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

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list '(0 4 8 12 16 20 24 28 32))

;; dabbrev
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-c i") 'helm-imenu)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(custom-set-faces
 '(helm-selection ((t (:background "lightblue" :foreground "black"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-directory ((t (:background "green" :foreground "white"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "ivory"))))
 )

;; spell check (flyspell)
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; os switch 
(cond ((equal window-system nil)
       (load "~/dotfiles/program.el"))
      ((equal system-type 'gnu/linux)
       (load "~/dotfiles/browser.el")
       (load "~/dotfiles/program.el"))
      ((equal system-type 'windows-nt)
       (load "~/dotfiles/program.el"))
      ((equal system-type 'darwin)
       (load "~/dotfiles/osx.el")
       (load "~/dotfiles/program.el")
       (load "~/dotfiles/browser.el"))
)
