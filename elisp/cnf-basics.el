;;; cnf-basics.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

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

(setq auto-save-default nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)

;; ------------------------------------------------------------------------
;; key bind

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window)
;; replace list-buffers
(global-set-key (kbd "C-x C-b") 'bs-show)
(if (>= emacs-major-version 25)
    (global-set-key (kbd "C-x C-b") 'buffer-menu-open)
)

;; ------------------------------------------------------------------------
;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; ------------------------------------------------------------------------
;; color set-face (basic)

(global-font-lock-mode t)
(set-face-foreground 'font-lock-builtin-face "magenta")
(set-face-foreground 'font-lock-comment-delimiter-face "DarkGreen")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-constant-face "orange")
(set-face-foreground 'font-lock-doc-face "gray")
(set-face-foreground 'font-lock-function-name-face "yellow")
(set-face-foreground 'font-lock-keyword-face "blue")
(set-face-foreground 'font-lock-preprocessor-face "yellow")
(set-face-foreground 'font-lock-regexp-grouping-backslash "gray")
(set-face-foreground 'font-lock-string-face "orange")
(set-face-foreground 'font-lock-type-face "brightred")
(set-face-foreground 'font-lock-variable-name-face "goldenrod")
(set-face-foreground 'font-lock-warning-face "pink")

;; ------------------------------------------------------------------------
;; color white spaces

(defface my-face-b-1 '((t (:background "Gray40"))) nil)
(defface my-face-b-2 '((t (:background "Gray20"))) nil)
(defface my-face-b-3 '((t (:background "Gray80"))) nil)
(defvar my-face-u-1 'my-face-b-1)
(defvar my-face-u-2 'my-face-b-2)
(defvar my-face-u-3 'my-face-b-3)
(defadvice font-lock-mode(before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode '(
                ("　" 0 my-face-u-1 append)
                ("\t" 0 my-face-u-2 append)
                ("[ 　\t]+$" 0 my-face-u-3 append)
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

;; disable auto fill
(setq fill-column nil)

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

;; mark comamnd repeat (C-u C-SPC ...)
(setq set-mark-command-repeat-pop t)

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

;; which function
(which-function-mode 1)

;; ignore case
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ------------------------------------------------------------------------
;; isearch

;; search
(setq case-fold-search t)
(setq isearch-case-fold-search t)

(defadvice isearch-mode
    (around isearch-mode-default-string
            (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; ------------------------------------------------------------------------
;; uniquify

(require 'uniquify)
(setq uniquify-ignore-buffers-re "*[^*]+*")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-min-dir-content 1)

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

;; https://abicky.net/2013/12/21/195058/

(electric-pair-mode 1)
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
;; abbrev

(let ((abbrev-file "~/.abbrev_defs"))
  (setq abbrev-file-name abbrev-file)
  (if (file-exists-p abbrev-file)
      (quietly-read-abbrev-file abbrev-file)))

(setq save-abbrevs 'silently)
(setq default-abbrev-mode t)

;; ------------------------------------------------------------------------
;; dabbrev

;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; ------------------------------------------------------------------------
;; recentf

(require 'recentf)

(defun recentf-save-list-inhibit-message:around (orig-func &rest args)
  (setq inhibit-message t)
  (apply orig-func args)
  (setq inhibit-message nil)
  'around)
(advice-add 'recentf-cleanup :around 'recentf-save-list-inhibit-message:around)
(advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)

;; (defadvice recentf-cleanup
;;   (around no-message activate)
;;   "Suppress the output from `message` to minibuffer."
;;   (cl-flet ((message (format-string &rest args)
;;                      (eval `(format ,format-string ,@args))))
;;     ad-do-it))

(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t 'recentf-save-list))
(setq-default find-file-visit-truename t)
(recentf-mode 1)

;; ------------------------------------------------------------------------
;; dired + wdired + dired-x

(setq dired-listing-switches (purecopy "-avhplGF"))
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq delete-by-moving-to-trash t)

;; zip
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))
(defun dired-zip-files (zip-file)
  "Create an archive containing the marked ZIP-FILEs."
  (interactive "Enter name of ZIP-FILE: ")

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
   "Return a string which is a concatenation of all elements of the LIST separated by spaces."
   (mapconcat #'(lambda (obj) (format "%s" obj)) list " "))

(require 'wdired)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

(add-hook 'dired-load-hook (lambda () (load "dired-x")))

;; ------------------------------------------------------------------------
;; executable make buffer

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ------------------------------------------------------------------------
;; time-stamp

(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(with-eval-after-load "time-stamp"
  (setq time-stamp-start "Last Update:")
  (setq time-stamp-format " %04y-%02m-%02d %02H:%02M:%02S")
  (setq time-stamp-end "$")
  (setq time-stamp-line-limit 15)) ; def=8

;; ------------------------------------------------------------------------
;; display-time-world

(setq display-time-world-time-format "%Z\t %Y %b %d (%a) %R"
      display-time-world-list
      '(("Asia/Tokyo" "Tokyo")
        ("America/Los_Angeles" "Los Angeles")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")))

;; ------------------------------------------------------------------------
;; nxml-mode (built-in)

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq nxml-slash-auto-complete-flag t)
            (setq nxml-child-indent 1)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            )
          )

;; ------------------------------------------------------------------------
;; org-mode

(setq org-log-done 'time)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; ------------------------------------------------------------------------
;; eww

(defvar eww-disable-colorize t)

(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg))
  )

(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)

(defun eww-disable-color ()
  "When eww disable flip colorize."
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload)
  )

(defun eww-enable-color ()
  "When eww enaboe color rize."
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload)
  )

;; ------------------------------------------------------------------------
;; describe-face-at-point

;; https://uwabami.github.io/cc-env/Emacs.html#orgb08f4b8

(defun my:describe-face-at-point ()
  "Describe face at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face))
  )

;; ------------------------------------------------------------------------

(provide 'cnf-basics.el)
;;; cnf-basics.el ends here
