;;; .emacs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ .emacs.el
;;
;; Install
;;
;; MacPorts site-lisp path
;; /opt/local/share/emacs/site-lisp/

;;; Code:

;; enable cl
(eval-when-compile (require 'cl))
(require 'cl-lib)

;; inhibit warnings
(setq byte-compile-warnings '(free-vars bytecomp))
(setq ad-redefinition-action 'accept)

;; ------------------------------------------------------------------------
;; backage.el

;; M-x list-packages
;; M-x package-list-packages

(defvar my-favorite-package-list
  '(auto-install
    exec-path-from-shell
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
    ac-helm
    anzu
    expand-region
    highlight-symbol
    foreign-regexp
    undo-tree
    editorconfig
    comment-tags
    rainbow-mode
    rainbow-delimiters
    web-mode
    php-mode
    php-eldoc
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
    helm-smex
    helm-swoop
    helm-ag
    helm-gtags
    helm-descbinds
    bm
    helm-bm
    yasnippet
    yasnippet-snippets
    helm-c-yasnippet
    qiita
    yagist
    projectile
    helm-projectile
    magit
    magit-find-file
    neotree
    emamux
    elscreen
    popwin
    multi-term
    shell-pop
    scratch-pop
    which-key
    smart-mode-line
    w3m
    dired-k
    dired-narrow
    dired-subtree
    google-translate
    japanese-holidays
    osx-trash)
  "Packages to be installed.")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; ------------------------------------------------------------------------
;; load basic settings

(load "~/dotfiles/cnf-basics.el")

;; ------------------------------------------------------------------------
;; eaw (ambiguous width characters)

;; https://github.com/uwabami/locale-eaw-emoji

(require 'locale-eaw-emoji)

;; ------------------------------------------------------------------------
;; binary path (exec-path-from-shell)

;;(add-to-list 'exec-path "/opt/local/bin")
;;(add-to-list 'exec-path "/usr/bin")

(exec-path-from-shell-initialize)

;; ------------------------------------------------------------------------
;; my-list-load

;; https://masutaka.net/chalow/2016-05-06-2.html

(defun my-lisp-load (filename)
"Load Lisp from FILENAME."
  (let ((fullname (expand-file-name (concat "spec/" filename) user-emacs-directory)) lisp)
    (when (file-readable-p fullname)
      (with-temp-buffer
        (progn (insert-file-contents fullname)
               (setq lisp
                     (condition-case nil (read (current-buffer)) (error ())))))) lisp))

;; ------------------------------------------------------------------------
;; auto-install

(require 'auto-install)
(setq auto-install-use-wget t)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; ------------------------------------------------------------------------
;; elscreen

(require 'elscreen)
(elscreen-start)
(setq elscreen-prefix-key (kbd "M-z"))
(setq elscreen-display-tab 25)
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
;; foreign-regexp

;; avoid ref warnings
(defvar foreign-regexp/regexp-type "")
(defvar foreign-regexp/re-builder/targ-buf-state/.orig-pt "")

(require 'foreign-regexp)

;; ------------------------------------------------------------------------
;; auto-complete

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(ac-set-trigger-key "TAB")
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
(setq ac-menu-height 15)
(setq ac-ignore-case t)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.2)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq-default ac-sources 'ac-source-words-in-same-mode-buffers)
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

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

;; key-bind
(define-key dired-mode-map (kbd "M-s") 'dired-narrow-fuzzy)

;; ------------------------------------------------------------------------
;; dired-subtree

(require 'dired-subtree)

(defun dired-subtree-up-dwim (&optional arg)
  "Traval parent directory."
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))

;; key-bind
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)

;; ------------------------------------------------------------------------
;; rainbow-mode

;;(require 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)

;; ------------------------------------------------------------------------
;; rainbow-delimiters

;;(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 50))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; ------------------------------------------------------------------------
;; imenu

(setq imenu-auto-rescan t)

;; ------------------------------------------------------------------------
;; imenu-list

(setq imenu-list-position "below")

;; ------------------------------------------------------------------------
;; load helm settings

(load "~/dotfiles/cnf-helm.el")

;; ------------------------------------------------------------------------
;; spell check (flyspell)

(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; ------------------------------------------------------------------------
;; volatile-highlights

;; https://github.com/k-talo/volatile-highlights.el

(require 'volatile-highlights)
(volatile-highlights-mode t)
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)

;; ------------------------------------------------------------------------
;; os switch

(cond ((equal system-type 'gnu/linux)
       (load "~/dotfiles/cnf-webservice.el")
       (load "~/dotfiles/cnf-browser.el")
       (load "~/dotfiles/cnf-program.el"))
      ((equal system-type 'windows-nt)
       (load "~/dotfiles/cnf-program.el"))
      ((equal system-type 'darwin)
       (load "~/dotfiles/cnf-osx.el")
       (load "~/dotfiles/cnf-program.el")
       (load "~/dotfiles/cnf-webservice.el")
       (load "~/dotfiles/cnf-browser.el"))
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

;;(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
;;(setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
;;(setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
;;(setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))

(global-set-key (kbd "C-c s") 'shell-pop)

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
;; popwin

(require 'popwin)
(popwin-mode 1)
(setq special-display-function 'popwin:special-display-popup-window)
(push '(dired-mode :position top) popwin:special-display-config)
(push '(compilation-mode :noselect t) popwin:special-display-config)
(push '("*quickrun*" :height 15) popwin:special-display-config)
(push '("*Ilist*" :height 15) popwin:special-display-config)
(push '(" *undo-tree*" :width 0.2 :position right) popwin:special-display-config)
(push '("*comment-tags*" :height 15) popwin:special-display-config) ;; not work

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
  "When close *scratch* buffer save to `kill-ring`."
  (interactive)
  (kill-new (buffer-string))
  (erase-buffer)
  (funcall (if (fboundp 'popwin:close-popup-window)
               'popwin:close-popup-window
             'quit-window)))

;; ------------------------------------------------------------------------
;; which-key

(which-key-setup-side-window-bottom) ; mini buffer
;; (which-key-setup-side-window-right)
;; (which-key-setup-side-window-right-bottom)

(which-key-mode 1)

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
    (editorconfig-mode . " EC")
    (elisp-slime-nav-mode . " EN")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    ;; Major modes
    (emacs-lisp-mode . "El")
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
;; load calendar settings

(load "~/dotfiles/cnf-calendar.el")

;; ------------------------------------------------------------------------
;; load color settings

(load "~/dotfiles/cnf-colors.el")

;; ------------------------------------------------------------------------
;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(emamux:completing-read-type (quote helm))
 '(foreign-regexp/regexp-type (quote perl) t)
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-projectile-files-list)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (helm-descbinds mozc ac-helm php-completion php-eldoc comment-tags helm-ls-git helm-bm helm-elscreen elpy expand-region avy emamux japanese-holidays id-manager 0xc scratch-pop magit-find-file e2wm imenu-list imenu-anywhere dired-subtree dired-narrow dired-filter helm-gtags quickrun fuzzy typescript-mode js2-refactor eldoc-extension yaml-mode dired-k osx-trash web-beautify stock-ticker multi-term multishell osx-dictionary helm-dash helm-ag imenus helm-swoop package-utils sequential-command helm-etags-plus smart-mode-line anzu highlight-symbol ac-html ac-js2 ac-php undo-tree shell-pop flycheck-popup-tip helm-qiita qiita helm-projectile iflibpb php-mode popwin iflipb markdown-mode elscreen tabbar neotree magit python-info jedi-direx company-jedi navi2ch json-mode js2-mode helm-google sudo-edit helm-c-yasnippet yasnippet-snippets rainbow-delimiters yasnippet rainbow-mode flycheck python-mode jedi auto-complete w3m mmm-mode helm ##)))
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

;; ------------------------------------------------------------------------

(provide '.emacs.el)
;;; .emacs.el ends here
