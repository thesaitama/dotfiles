;;; cnf-basics.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; add load-path

(setq load-path (append '("~/dotfiles/elisp"
                          "~/dotfiles/elisp/ext"
                          ) load-path))

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
;; other-window (extension)

(defun other-window-or-split ()
  "Other window or split vertically."
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))

;; ------------------------------------------------------------------------
;; key bind

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-t") 'other-window-or-split)
(global-set-key (kbd "C-x o") 'other-window-or-split)
(global-set-key (kbd "C-x C-o") 'other-window-or-split)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
;; replace list-buffers
(global-set-key (kbd "C-x C-b") 'bs-show)
;; (if (>= emacs-major-version 25)
;;     (global-set-key (kbd "C-x C-b") 'buffer-menu-open)
;; )

;; ------------------------------------------------------------------------
;; ediff

(defvar ediff-window-setup-function 'ediff-setup-windows-plain)
(defvar ediff-split-window-function 'split-window-horizontally)

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

(set-face-foreground 'mode-line "Skyblue")
(set-face-background 'mode-line "Gray30")
(set-face-foreground 'mode-line-buffer-id "orange")
(set-face-foreground 'mode-line-inactive "Gray60")
(set-face-background 'mode-line-inactive "Gray25")

;; ------------------------------------------------------------------------
;; color white spaces

(defface my-face-b-1 '((t :background "Gray40"))
  "Face for Double width space" :group 'my-face)
(defface my-face-b-2 '((t :background "Gray23"))
  "Face for Tab char" :group 'my-face)
(defface my-face-b-3 '((t :background "Gray80"))
  "Face for redundant spaces" :group 'my-face)
(defvar my-face-u-1 'my-face-b-1)
(defvar my-face-u-2 'my-face-b-2)
(defvar my-face-u-3 'my-face-b-3)
(defadvice font-lock-mode (before my-font-lock-mode ())
  "White space patch for font lock mode."
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
;; my-make-scratch

;; http://www.jaist.ac.jp/~n-yoshi/tips/elisp_tips.html#scratch

(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; create "*scratch*" buffer
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; when kill *scratch* kill-buffer, just content delete
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; if user create *scratch* buffer and create new one
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

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

;; menu-bar
(menu-bar-mode 0)

;; region display
(setq transient-mark-mode t)

;; mark comamnd repeat (C-u C-SPC ...)
(setq set-mark-command-repeat-pop t)

;; linum
(if (fboundp 'global-linum-mode)
    (progn
      (global-linum-mode 0)
      (set-variable 'linum-format "%4d ")
      )
  )

(column-number-mode t)
(line-number-mode t)

;; highlight editing line
(global-hl-line-mode t)

;; end of line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(setq eol-mnemonic-undecided "(?)")

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

;; show file size
(size-indication-mode t)

;; can move across splitted frames with direction keys
(windmove-default-keybindings)
(set-variable 'windmove-wrap-around t)

;; ignore case
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; disable set-goal-column
(put 'set-goal-column 'disabled nil)

;; disable text-mode auto-fill
(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; auto scroll when compile
(set-variable 'compilation-scroll-output t)

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
;; imenu

(set-variable 'imenu-auto-rescan t)

;; ------------------------------------------------------------------------
;; save-place

(set-variable 'save-place-file "~/.emacs.d/saved-places")
(if (>= emacs-major-version 25)
    (save-place-mode 1)
  (progn
    (when (require 'saveplace nil t)
      (setq-default save-place t)
      ))
  )

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
(set-variable 'show-paren-style 'mixed)

;; ------------------------------------------------------------------------
;; electric-pair

;; https://abicky.net/2013/12/21/195058/

(if (fboundp 'electric-pair-mode)
    (progn
      (electric-pair-mode 1)
      (defadvice electric-pair-post-self-insert-function
          (around electric-pair-post-self-insert-function-around activate)
        "Don't insert the closing pair in comments or strings."
        (unless (nth 8 (save-excursion (syntax-ppss (1- (point)))))
          ad-do-it))
      )
  )

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
(defvar default-abbrev-mode t)

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

(if (fboundp 'advice-add)
    (progn
      (advice-add 'recentf-cleanup :around 'recentf-save-list-inhibit-message:around)
      (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)
      )
  )
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 60)
(setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))
(setq-default find-file-visit-truename t)
(recentf-mode 1)

;; ------------------------------------------------------------------------
;; dired + wdired + dired-x

(set-variable 'dired-listing-switches "-avhplGF")
(set-variable 'dired-auto-revert-buffer t)
(set-variable 'dired-dwim-target t)
(set-variable 'dired-recursive-copies 'always)
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
(setq wdired-allow-to-change-permissions t)

(add-hook 'dired-load-hook (lambda () (load "dired-x")))

;; ------------------------------------------------------------------------
;; return-current-working-directory

;; https://uwabami.github.io/cc-env/Emacs.html

(defun return-current-working-directory-to-shell ()
  (expand-file-name
   (with-current-buffer
       (if (featurep 'elscreen)
           (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
                  (num (nth 1 (assoc 'screen-history frame-confs)))
                  (cur-window-conf
                   (assoc 'window-configuration
                          (assoc num (assoc 'screen-property frame-confs))))
                  (marker (nth 2 cur-window-conf)))
             (marker-buffer marker))
         (nth 1
              (assoc 'buffer-list
                     (nth 1 (nth 1 (current-frame-configuration))))))
     default-directory)))

;; ------------------------------------------------------------------------
;; executable make buffer

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ------------------------------------------------------------------------
;; time-stamp

(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

(setq time-stamp-start "Last Update:")
(setq time-stamp-format " %04y-%02m-%02d %02H:%02M:%02S")
(setq time-stamp-end "$")
(setq time-stamp-line-limit 15) ; def=8

;; ------------------------------------------------------------------------
;; display-time-world

(set-variable 'display-time-world-time-format "%Z\t %Y %b %d (%a) %R")
(set-variable 'display-time-world-list
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

(set-variable 'org-log-done 'time)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; ------------------------------------------------------------------------
;; eshell

(defvar eshell-command-aliases-list
      (append
       (list
        (list "ls" "ls -a")
        (list "o" "xdg-open")
        (list "emacs" "find-file $1")
        (list "e" "find-file $1")
        (list "d" "dired .")
        )))
(defvar eshell-path-env (getenv "PATH"))

;; ------------------------------------------------------------------------
;; eww

(when (require 'eww nil t)
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
;; Window System

;; if user run window system
(cond (window-system
  (load-if-exist "~/dotfiles/elisp/cnf-basics-win.el"))
)

;; ------------------------------------------------------------------------

(provide 'cnf-basics.el)
;;; cnf-basics.el ends here
