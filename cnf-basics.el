;;; cnf-basics.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitam Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; add load-path

(setq load-path
      (append '("~/dotfiles/elisp")
              load-path))

;; ------------------------------------------------------------------------
;; load-prefer-newer .elc or .el

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; ------------------------------------------------------------------------
;; character code

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; ------------------------------------------------------------------------
;; backup and lock file

(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)
(setq create-lockfiles nil)

;; ------------------------------------------------------------------------
;; key bind

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window)
(global-set-key (kbd "C-x C-b") 'bs-show) ;; replace list-buffers

;; ------------------------------------------------------------------------
;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; ------------------------------------------------------------------------
;; color set-face (basic)

(global-font-lock-mode t)
(set-face-foreground 'font-lock-type-face "darkyellow")
(set-face-foreground 'font-lock-builtin-face "magenta")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")
(set-face-foreground 'font-lock-string-face "darkorange")
(set-face-foreground 'font-lock-keyword-face "blue")
(set-face-foreground 'font-lock-function-name-face "yellow")
(set-face-foreground 'font-lock-variable-name-face "goldenrod")
(set-face-foreground 'font-lock-constant-face "orange")
(set-face-foreground 'font-lock-preprocessor-face "darkyellow")
(set-face-foreground 'font-lock-warning-face "pink")

;; ------------------------------------------------------------------------
;; color white spaces

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
   ("[ 　\t]+$" 0 my-face-u-1 append)
   )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
    (if font-lock-mode
        nil
      (font-lock-mode t))))

;; ------------------------------------------------------------------------
;; UI / UX

;; startup message
(setq inhibit-startup-message t)

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
    (editorconfig-mode . " EC")
    (elisp-slime-nav-mode . " EN")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    ;; Major modes
    (emacs-lisp-mode . "El")
    (default-generic-mode . "DGen")
    (generic-mode . "Gen")
    (lisp-interaction-mode . "Li")
    (shell-script-mode . "SS")
    (python-mode . "Py")
    (ruby-mode . "Rb")
    (typescript-mode . "TS")
    (markdown-mode . "Md")
    (fundamental-mode . "Fund")
    ))

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
;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 1)

;; ------------------------------------------------------------------------
;; mouse

(require 'mouse)
(xterm-mouse-mode t)

(require 'mwheel)
(mouse-wheel-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 3)))

;; ------------------------------------------------------------------------
;; show-paren

(show-paren-mode t)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")
(set-face-background 'show-paren-mismatch "red")

;; ------------------------------------------------------------------------
;; electric-pair

(electric-pair-mode 1)

;; https://abicky.net/2013/12/21/195058/

(defadvice electric-pair-post-self-insert-function
  (around electric-pair-post-self-insert-function-around activate)
  "Don't insert the closing pair in comments or strings."
  (unless (nth 8 (save-excursion (syntax-ppss (1- (point)))))
    ad-do-it))

;; ------------------------------------------------------------------------
;; indent-tabs

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'sh-mode-hook '(lambda () (setq tab-width 2)(setq sh-basic-offset 2)
        (setq sh-indentation 2)))

;; ------------------------------------------------------------------------
;; generic-x

(require 'generic-x)

;; ------------------------------------------------------------------------
;; dabbrev

;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

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

(provide 'cnf-basics.el)
;;; cnf-basics.el ends here
