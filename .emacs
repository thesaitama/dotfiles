
;; thesaitama .emacs

;; Install

;; MacPorts site-lisp path
;; /opt/local/share/emacs/site-lisp/

;; ------------------------------------------------------------------------
;; backage.el

;; M-x list-packages
;; M-x package-list-packages

(defvar my-favorite-package-list
  '(auto-install
    package-utils
    auto-complete
    sequential-command
    ac-html
    ac-js2
    ac-php
    anzu
    highlight-symbol
    foreign-regexp
    undo-tree
    rainbow-mode
    rainbow-delimiters
    web-mode
    php-mode
    js2-mode
    python-mode
    jedi
    flycheck
    flycheck-popup-tip
    imenus
    helm
    helm-swoop
    yasnippet
    yasnippet-snippets
    helm-c-yasnippet
    qiita
    helm-qiita
    yagist
    projectile
    helm-projectile
    magit
    neotree
    iflipb
    popwin
    shell-pop
    google-translate)
  "packages to be installed")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; ------------------------------------------------------------------------
;; my-list-load

(defun my-lisp-load (filename)
"Load lisp from FILENAME"
  (let ((fullname (expand-file-name (concat "spec/" filename) user-emacs-directory)) lisp)
    (when (file-readable-p fullname)
      (with-temp-buffer
        (progn (insert-file-contents fullname)
               (setq lisp
                     (condition-case nil (read (current-buffer)) (error ())))))) lisp))

;; ------------------------------------------------------------------------
;; auto-install

(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------
;; key bind

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window)
(global-set-key (kbd "C-x C-b") 'bs-show) ;; replace list-buffers
(global-set-key (kbd "<f9>") 'other-window)

;; ------------------------------------------------------------------------
;; highlight-symbol

(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

;; ------------------------------------------------------------------------
;; sequential-command

(require 'sequential-command-config)
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)
(when (require 'org nil t)
  (define-key org-mode-map "\C-a" 'org-seq-home)
  (define-key org-mode-map "\C-e" 'org-seq-end))
(define-key esc-map "u" 'seq-upcase-backward-word)
(define-key esc-map "c" 'seq-capitalize-backward-word)
(define-key esc-map "l" 'seq-downcase-backward-word)

;; ------------------------------------------------------------------------
;; anzu

(require 'anzu)
(global-anzu-mode +1)
;(setq anzu-use-migemo t)
(setq anzu-search-threshold 1000)
(setq anzu-minimum-input-length 3)
(global-set-key (kbd "C-c r") 'anzu-query-replace)
(global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)

;; ------------------------------------------------------------------------
;; iflipb

(global-set-key (kbd "<f8>") 'iflipb-next-buffer)
(global-set-key (kbd "<f7>") 'iflipb-previous-buffer)

;; ------------------------------------------------------------------------
;; auto-complete

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t) ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; backup file
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; ------------------------------------------------------------------------
;; color set-face

(global-font-lock-mode t)
(set-face-foreground 'font-lock-type-face "darkyellow")
(set-face-foreground 'font-lock-builtin-face "magenta")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")
(set-face-foreground 'font-lock-string-face "darkorange")
(set-face-foreground 'font-lock-keyword-face "blue")
(set-face-foreground 'font-lock-function-name-face "yellow") ; lightskyblue
(set-face-foreground 'font-lock-variable-name-face "goldenrod")
(set-face-foreground 'font-lock-constant-face "orange")
(set-face-foreground 'font-lock-preprocessor-face "darkyellow")
(set-face-foreground 'font-lock-warning-face "pink")
(set-face-foreground 'tool-bar "cyan")
(set-face-background 'region "lightblue")
(set-face-foreground 'isearch "black")
(set-face-background 'isearch "lightpink")
(set-face-foreground 'isearch-lazy-highlight-face "black")
(set-face-background 'isearch-lazy-highlight-face "cyan")
(set-face-foreground 'minibuffer-prompt "blue")
(set-face-foreground 'fringe "blue")
(set-face-background 'fringe "gray12")

;; ------------------------------------------------------------------------
;; color (custom set face)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-directory ((t (:background "Gray25" :foreground "white"))))
 '(helm-ff-dotted-directory ((t (:background "glay" :foreground "white"))))
 '(helm-ff-executable ((t (:inherit font-lock-builtin-face :foreground "orange"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "ivory"))))
 '(helm-ff-symlink ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-selection ((t (:background "LightSkyBlue" :foreground "black"))))
 '(helm-source-header ((t (:background "BrightBlue" :foreground "white"))))
 '(linum ((t (:inherit (shadow default) :background "Gray23"))))
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold))))
 '(markdown-pre-face ((t (:inherit org-formula)))))

;; ------------------------------------------------------------------------
;; UI / UX

;; disable bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; title-bar character
(setq frame-title-format (concat "%b - emacs@" (system-name)))

;; tool-bar
(setq tool-bar-mode 0)

;; menu-bar
(menu-bar-mode -1)

;; region display
(setq transient-mark-mode t)

;; line number
(global-linum-mode 0)
(setq linum-format "%4d ")

;; highlight editing line
;(global-hl-line-mode t)
;(custom-set-faces '(hl-line ((t (:background "color-236")))))

;; frame
(setq initial-frame-alist
  (append (list
   '(border-color . "black")
   '(mouse-color . "black")
   '(menu-bar-lines . 1)
   )
  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; startup message
(setq inhibit-startup-message t)

;; end of line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; display image file
(auto-image-file-mode t)

;; auto compression
(auto-compression-mode t)

;; auto reload bufffer
(global-auto-revert-mode 1)

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ------------------------------------------------------------------------
;; mouse

(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 3)))

;; ------------------------------------------------------------------------
;; recentf

(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))
(setq-default find-file-visit-truename t)

;; ------------------------------------------------------------------------
;; paren

(show-paren-mode t)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")
(setq show-paren-style 'mixed)

(electric-pair-mode 1)

;; ------------------------------------------------------------------------
;; rainbow-mode

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; ------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------
;; colored white spaces

(defface my-face-b-1 '((t (:background "lightyellow"))) nil)
(defface my-face-b-2 '((t (:background "darkgray"))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-b-2)
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

;; ------------------------------------------------------------------------
;; indent-tabs

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'sh-mode-hook '(lambda () (setq tab-width 2)(setq sh-basic-offset 2)
        (setq sh-indentation 2)))

;; ------------------------------------------------------------------------
;; dabbrev

;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; ------------------------------------------------------------------------
;; helm

(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-c h") 'helm-mini)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-c i") 'helm-imenu)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate) ad-do-it))
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))
;; hide directory ..
(advice-add 'helm-ff-filter-candidate-one-by-one
        :around (lambda (fcn file)
                  (unless (string-match "\\(?:/\\|\\`\\)\\.\\{2\\}\\'" file)
                    (funcall fcn file))))
;; auto resize
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

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
;; 値がtの場合はウィンドウ内に分割、nilなら別のウィンドウを使用
(setq helm-swoop-split-with-multiple-windows nil)
(setq helm-swoop-split-direction 'split-window-vertically)

;; ------------------------------------------------------------------------
;; helm-ag

(require 'helm-files)
(require 'helm-ag)

(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "C-M-s") 'helm-ag-this-file)

;; ------------------------------------------------------------------------
;; spell check (flyspell)

(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; ------------------------------------------------------------------------
;; eaw (ambiguous width characters)

(load "~/dotfiles/locale-eaw-emoji.el")
(eaw-and-emoji-fullwidth)

;; ------------------------------------------------------------------------
;; os switch

(cond ((equal system-type 'gnu/linux)
       (load "~/dotfiles/webservice.el")
       (load "~/dotfiles/browser.el")
       (load "~/dotfiles/program.el"))
      ((equal system-type 'windows-nt)
       (load "~/dotfiles/program.el"))
      ((equal system-type 'darwin)
       (load "~/dotfiles/osx.el")
       (load "~/dotfiles/program.el")
       (load "~/dotfiles/webservice.el")
       (load "~/dotfiles/browser.el"))
)

;; ------------------------------------------------------------------------
;; eshell

(setq eshell-command-aliases-list
      (append
       (list
        (list "ls" "ls -a")
        (list "o" "xdg-open")
        (list "emacs" "find-file $1")
        (list "e" "find-file $1")
        (list "d" "dired .")
        )))
(setq eshell-path-env (getenv "PATH"))

;; ------------------------------------------------------------------------
;; shell-pop

(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
;;(setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
;;(setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
;;(setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
(global-set-key (kbd "C-c s") 'shell-pop)

;; ------------------------------------------------------------------------
;; undo-tree

(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
;;(defun undo-tree-split-side-by-side (original-function &rest args)
;;  "Split undo-tree side-by-side"
;;  (let ((split-height-threshold nil)
;;        (split-width-threshold 0))
;;    (apply original-function args)))
;;(advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

;; ------------------------------------------------------------------------
;; foreign-regexp

(require 'foreign-regexp)

;; ------------------------------------------------------------------------
;; modeline

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; display function
(which-function-mode 1)

;; clean mode line
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (font-lock-mode . "")
    (elisp-slime-nav-mode . " EN")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (fundamental-mode . "Fund")
    (lisp-mode . "El")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; ------------------------------------------------------------------------
;; smart-mode-line

(require 'smart-mode-line)
;; bug hack
(setq sml/active-background-color "gray60")
(setq sml/read-only-char "%%")
(setq sml/modified-char "*")
;; hide Helm and auto-complete
(setq sml/hidden-modes '(" Helm" " AC" " yas" " ARev" " Anzu"))
;; hack (privent overflow)
(setq sml/extra-filler -10)
;;; sml/replacer-regexp-list
(add-to-list 'sml/replacer-regexp-list '("^.+/junk/[0-9]+/" ":J:") t)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; theme
;;(sml/apply-theme 'respectful)
;;(sml/apply-theme 'light)
(sml/apply-theme 'dark)

;; ------------------------------------------------------------------------
;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(foreign-regexp/regexp-type (quote perl))
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-projectile-files-list)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (helm-ag imenus helm-swoop package-utils sequential-command helm-etags-plus smart-mode-line anzu highlight-symbol ac-html ac-js2 ac-php undo-tree shell-pop flycheck-popup-tip helm-qiita qiita helm-projectile iflibpb php-mode popwin iflipb markdown-mode elscreen tabbar neotree magit python-info jedi-direx company-jedi navi2ch json-mode js2-mode helm-google sudo-edit helm-c-yasnippet yasnippet-snippets rainbow-delimiters yasnippet rainbow-mode flycheck python-mode jedi auto-complete w3m mmm-mode helm ##)))
 '(reb-re-syntax (quote foreign-regexp))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

