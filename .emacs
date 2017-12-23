
;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;; thesaitama@ .emacs

;; Install

;; MacPorts site-lisp path
;; /opt/local/share/emacs/site-lisp/

;; enable cl
(eval-when-compile (require 'cl))

;; inhibit warnings
(setq byte-compile-warnings '(free-vars bytecomp))
(setq ad-redefinition-action 'accept)

;; ------------------------------------------------------------------------
;; backage.el

;; M-x list-packages
;; M-x package-list-packages

(defvar my-favorite-package-list
  '(auto-install
    0xc
    eldoc-extension
    package-utils
    auto-complete
    avy
    fuzzy
    sequential-command
    editorconfig
    quickrun
    ac-html
    ac-js2
    ac-php
    anzu
    expand-region
    highlight-symbol
    foreign-regexp
    undo-tree
    rainbow-mode
    rainbow-delimiters
    web-mode
    php-mode
    js2-mode
    js2-refactor
    json-mode
    typescript-mode
    tss
    yaml-mode
    python-mode
    jedi
    elpy
    flycheck
    flycheck-popup-tip
    imenus
    imenu-anywhere
    imenu-list
    helm
    helm-swoop
    helm-ag
    helm-gtags
    bm
    helm-bm
    yasnippet
    yasnippet-snippets
    helm-c-yasnippet
    qiita
    helm-qiita
    yagist
    projectile
    helm-projectile
    magit
    magit-find-file
    neotree
    emamux
    elscreen
    iflipb
    popwin
    multi-term
    shell-pop
    scratch-pop
    smart-mode-line
    w3m
    dired-k
    dired-narrow
    dired-subtree
    google-translate
    japanese-holidays
    osx-trash)
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
;; binary path

(add-to-list 'exec-path "/opt/local/bin")

;; ------------------------------------------------------------------------
;; my-list-load

;; https://masutaka.net/chalow/2016-05-06-2.html

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
;; generic-x

(require 'generic-x)

;; ------------------------------------------------------------------------
;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; ------------------------------------------------------------------------
;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 1)

;; ------------------------------------------------------------------------
;; elscreen

(require 'elscreen)
(elscreen-start)
(setq elscreen-prefix-key (kbd "M-z"))
(setq elscreen-display-tab 20)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; ------------------------------------------------------------------------
;; expand-region

(require 'expand-region)
(global-set-key (kbd "M-,") 'er/expand-region)

;; ------------------------------------------------------------------------
;; highlight-symbol

(require 'highlight-symbol)
(setq highlight-symbol-colors '("LightSeaGreen" "HotPink" "SlateBlue1" "SpringGreen1" "tan" "DarkOrange" "DodgerBlue1" "DeepPink1"))
(setq highlight-symbol-idle-delay 1.0)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
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
;; foreign-regexp

;; avoid ref warnings
(defvar foreign-regexp/regexp-type "")
(defvar foreign-regexp/re-builder/targ-buf-state/.orig-pt "")

(require 'foreign-regexp)

;; ------------------------------------------------------------------------
;; avy

(global-set-key (kbd "M-s") 'avy-goto-char)

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

;; ------------------------------------------------------------------------
;; backup and lock file

(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)
(setq create-lockfiles nil)

;; ------------------------------------------------------------------------
;; color set-face

(global-font-lock-mode t)
(set-face-foreground 'font-lock-type-face "darkyellow")
(set-face-foreground 'font-lock-builtin-face "magenta")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")
(set-face-foreground 'font-lock-string-face "darkorange")
(set-face-foreground 'font-lock-keyword-face "blue")
(set-face-foreground 'font-lock-function-name-face "yellow") ;lightskyblue
(set-face-foreground 'font-lock-variable-name-face "goldenrod")
(set-face-foreground 'font-lock-constant-face "orange")
(set-face-foreground 'font-lock-preprocessor-face "darkyellow")
(set-face-foreground 'font-lock-warning-face "pink")
(set-face-foreground 'tool-bar "cyan")
(set-face-background 'region "Gray40") ;lightblue
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
 '(diff-added ((((type tty)) (:foreground "green"))))
 '(diff-removed ((((type tty)) (:foreground "red"))))
 '(dired-header ((t (:background "BrightBlue" :foreground "white"))))
 '(dired-subtree-depth-1-face ((t (:background "Gray19"))))
 '(dired-subtree-depth-2-face ((t (:background "Gray20"))))
 '(dired-subtree-depth-3-face ((t (:background "Gray21"))))
 '(dired-subtree-depth-4-face ((t (:background "Gray22"))))
 '(elscreen-tab-background-face ((t (:background "Gray10" :foreground "Gray90"))))
 '(elscreen-tab-control-face ((t (:background "Gray20" :foreground "Gray90"))))
 '(elscreen-tab-current-screen-face ((t (:background "Gray80" :foreground "Gray20"))))
 '(elscreen-tab-other-screen-face ((t (:background "Gray25" :foreground "Gray80"))))
 '(bm-face ((t (:background "Gray25"))))
 '(bm-fringe-face ((t (:background "Gray25"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-directory ((t (:background "Gray25" :foreground "white"))))
 '(helm-ff-dotted-directory ((t (:background "glay" :foreground "white"))))
 '(helm-ff-executable ((t (:inherit font-lock-builtin-face :foreground "orange"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "ivory"))))
 '(helm-ff-symlink ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-match ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "Gray23"))))
 '(helm-source-header ((t (:background "BrightBlue" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "Gray25"))))
 '(hl-line ((t (:background "color-236"))))
 '(holiday ((t (:background "pink"))))
 '(japanese-holiday-saturday ((t (:background "cyan"))))
 '(linum ((t (:inherit (shadow default) :background "Gray22"))))
 '(magit-branch-local ((t (:foreground "magenta"))))
 '(magit-branch-remote ((t (:foreground "blue"))))
 '(magit-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((t (:background "Gray23"))))
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold))))
 '(markdown-pre-face ((t (:foreground "ivory"))))
 '(neo-dir-link-face ((t (:background "Gray25" :foreground "white"))))
 '(neo-file-link-face ((t (:foreground "ivory"))))
 '(neo-vc-default-face ((t (:foreground "ivory"))))
 '(neo-vc-up-to-date-face ((t (:foreground "ivory"))))
 '(package-name ((t (:foreground "blue"))))
 '(web-mode-comment-face ((t (:foreground "green"))))
 '(web-mode-css-at-rule-face ((t (:foreground "magenta"))))
 '(web-mode-css-pseudo-class ((t (:foreground "blue"))))
 '(web-mode-css-selector-face ((t (:foreground "blue"))))
 '(web-mode-doctype-face ((t (:foreground "glay"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "white"))))
 '(web-mode-html-attr-name-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-attr-value-face ((t (:foreground "yellow"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan"))))
 '(web-mode-server-comment-face ((t (:foreground "green")))))

;; ------------------------------------------------------------------------
;; UI / UX

;; find file at point
(ffap-bindings)

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
(global-hl-line-mode t)

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

;; open symlinks no confirmation
(setq vc-follow-symlinks t)

;; ------------------------------------------------------------------------
;; mouse

(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 3)))

;; ------------------------------------------------------------------------
;; dired + dired-x, dired-narrow

(add-hook 'dired-load-hook (lambda () (load "dired-x")))

(setq dired-listing-switches (purecopy "-Ahl"))
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)

;; zip
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))
(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")

  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command 
     (concat "zip " 
             zip-file
             " "
             (concat-string-list 
              (mapcar
               #'(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))

  (revert-buffer)

  ;; remove the mark on all the files  "*" to " "
  ;; (dired-change-marks 42 ?\040)
  ;; mark zip file
  ;; (dired-mark-files-regexp (filename-to-regexp zip-file))
  )

(defun concat-string-list (list) 
   "Return a string which is a concatenation of all elements of the list separated by spaces" 
   (mapconcat #'(lambda (obj) (format "%s" obj)) list " "))

;; key-bind
(define-key dired-mode-map (kbd "M-s") 'dired-narrow-fuzzy)

;; ------------------------------------------------------------------------
;; dired-subtree

(require 'dired-subtree)

(defun dired-subtree-up-dwim (&optional arg)
  "traval parent directory"
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))

;; key-bind
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)

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
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")
(set-face-background 'show-paren-mismatch "red")
(electric-pair-mode 1)

;; ------------------------------------------------------------------------
;; rainbow-mode

(require 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)

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
;; imenu

(setq imenu-auto-rescan t)

;; ------------------------------------------------------------------------
;; imenu-list

(setq imenu-list-position "below")

;; ------------------------------------------------------------------------
;; helm

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

(setq helm-split-window-inside-p t)
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
  "when 2 times load run helm-bm"
  (interactive)
  (bm-toggle)
  (when (eq last-command 'bm-toggle-or-helm)
    (helm-bm)))
(global-set-key (kbd "M-SPC") 'bm-toggle-or-helm)

;;; bug ?
(require 'compile)

;; ------------------------------------------------------------------------
;; id-manager

(autoload 'id-manager "id-manager" nil t)
(global-set-key (kbd "M-7") 'id-manager)
(setenv "GPG_AGENT_INFO" nil)

;; ------------------------------------------------------------------------
;; spell check (flyspell)

(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; ------------------------------------------------------------------------
;; eaw (ambiguous width characters)

;; https://github.com/uwabami/locale-eaw-emoji

(load "~/dotfiles/locale-eaw-emoji.el")
(eaw-and-emoji-fullwidth)

;; ------------------------------------------------------------------------
;; volatile-highlights

;; https://github.com/k-talo/volatile-highlights.el

(load "~/dotfiles/volatile-highlights.el")
(volatile-highlights-mode t)
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)

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
;; pathheader

;(load "~/dotfiles/pathheader.el")

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

;;(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
;;(setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
;;(setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
;;(setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))

(global-set-key (kbd "C-c s") 'shell-pop)

;; ------------------------------------------------------------------------
;; popwin

(require 'popwin)
(popwin-mode 1)
(setq special-display-function 'popwin:special-display-popup-window)
(push '(dired-mode :position top) popwin:special-display-config)
(push '(compilation-mode :noselect t) popwin:special-display-config)
(push '("*quickrun*" :height 15) popwin:special-display-config)
(push '("*Ilist*" :height 15) popwin:special-display-config)
(push '(" *undo-tree*" :width 0.2 :position right) popwin:special-display-config)

;; ------------------------------------------------------------------------
;; scratch-pop

; http://emacs.rubikitch.com/scratch-pop/

(require 'scratch-pop)
(global-set-key (kbd "C-c c") 'scratch-pop)

(define-minor-mode scratch-ext-minor-mode
  "minor mode for *scratch* buffer"
  nil ""
  '(("\C-c\C-c" . scratch-pop-kill-ring-save-exit)
    ("\C-c\C-e" . erase-buffer)))

(with-current-buffer (get-buffer-create "*scratch*")
  (ignore-errors
    (insert-file-contents auto-save-buffers-enhanced-file-related-with-scratch-buffer))
  (setq header-line-format "scratch!!")
  (scratch-ext-minor-mode 1))

(defun scratch-pop-kill-ring-save-exit ()
  "when close *scratch* buffer save to kill-ring"
  (interactive)
  (kill-new (buffer-string))
  (erase-buffer)
  (funcall (if (fboundp 'popwin:close-popup-window)
               'popwin:close-popup-window
             'quit-window)))

;; ------------------------------------------------------------------------
;; multi-term

(setq multi-term-program shell-file-name)

(add-hook 'term-mode-hook '(lambda ()
  (define-key term-raw-map "\C-y" 'term-paste)
  (define-key term-raw-map "\C-q" 'move-beginning-of-line)
  (define-key term-raw-map "\C-f" 'forward-char)
  (define-key term-raw-map "\C-b" 'backward-char)
  (define-key term-raw-map "\C-t" 'set-mark-command)
  (define-key term-raw-map "\C-p" 'term-send-up)
  (define-key term-raw-map "\C-n" 'term-send-down)
  (define-key term-raw-map (kbd "ESC") 'term-send-raw)
  (define-key term-raw-map [delete] 'term-send-raw)
  (define-key term-raw-map [mouse-4] 'term-send-up)
  (define-key term-raw-map [mouse-5] 'term-send-down)
  (define-key term-raw-map "\C-z"
    (lookup-key (current-global-map) "\C-z"))))
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)

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
    (editorbconfig-mode . "")
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
;; calendar

(with-eval-after-load "calendar"
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (when (require 'japanese-holidays nil t)
    (setq calendar-holidays
          (append japanese-holidays
                  holiday-local-holidays holiday-other-holidays))
    (setq calendar-mark-holidays-flag t)
    (setq mark-holidays-in-calendar t)
    (setq japanese-holiday-weekend-marker
          '(holiday nil nil nil nil nil japanese-holiday-saturday))
    (setq japanese-holiday-weekend '(0 6))
    (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  )
)

;; ------------------------------------------------------------------------
;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(emamux:completing-read-type (quote helm))
 '(foreign-regexp/regexp-type (quote perl))
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-projectile-files-list)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (helm-bm helm-elscreen elpy expand-region avy emamux japanese-holidays id-manager 0xc scratch-pop magit-find-file e2wm imenu-list imenu-anywhere dired-subtree dired-narrow dired-filter helm-gtags quickrun fuzzy typescript-mode js2-refactor eldoc-extension yaml-mode dired-k osx-trash web-beautify stock-ticker multi-term multishell osx-dictionary helm-dash helm-ag imenus helm-swoop package-utils sequential-command helm-etags-plus smart-mode-line anzu highlight-symbol ac-html ac-js2 ac-php undo-tree shell-pop flycheck-popup-tip helm-qiita qiita helm-projectile iflibpb php-mode popwin iflipb markdown-mode elscreen tabbar neotree magit python-info jedi-direx company-jedi navi2ch json-mode js2-mode helm-google sudo-edit helm-c-yasnippet yasnippet-snippets rainbow-delimiters yasnippet rainbow-mode flycheck python-mode jedi auto-complete w3m mmm-mode helm ##)))
 '(popwin-mode t)
 '(reb-re-syntax (quote foreign-regexp))
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("multi-term" "*terminal<1>*"
     (quote
      (lambda nil
        (multi-term))))))
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(put 'set-goal-column 'disabled nil)
