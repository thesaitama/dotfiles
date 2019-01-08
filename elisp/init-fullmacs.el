;;; init-fullmacs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ init-fullmacs.el
;; full spec Emacs setting
;;

;;; Code:

;; ------------------------------------------------------------------------

;; debugger if needed
;; (setq debug-on-error t)

;; enable cl
(eval-when-compile (require 'cl))
(require 'cl-lib)

;; inhibit warnings
(setq byte-compile-warnings '(free-vars bytecomp))
(setq ad-redefinition-action 'accept)

;; GC
(setq gc-cons-threshold (* 256 1024 1024))
(setq garbage-collection-messages t)

;; ------------------------------------------------------------------------

(setq initial-scratch-message ";; saitamacs\n")

;; ------------------------------------------------------------------------
;; backage.el

;; M-x list-packages
;; M-x package-list-packages

(defvar my-favorite-package-list
  '(exec-path-from-shell
    package-utils
    0xc
    smooth-scroll
    recentf-ext
    elscreen
    popwin
    import-popwin
    auto-complete
    fuzzy
    org-ac
    pos-tip
    company
    company-quickhelp
    sequential-command
    avy
    ace-isearch
    anzu
    editorconfig
    expand-region
    highlight-symbol
    foreign-regexp
    multiple-cursors
    electric-operator
    undohist
    undo-tree
    editorconfig
    comment-tags
    csv-mode
    rainbow-mode
    rainbow-delimiters
    web-mode
    web-beautify
    ac-html
    emmet-mode
    ac-emmet
    php-mode
    ac-php
    php-eldoc
    rbenv
    ruby-electric
    inf-ruby
    ac-inf-ruby
    robe
    js2-mode
    js2-refactor
    ac-js2
    tern
    json-mode
    typescript-mode
    tide
    tern-auto-complete
    yaml-mode
    toml-mode
    python-mode
    jedi
    elpy
    omnisharp
    ensime
    go-mode
    go-eldoc
    go-autocomplete
    rust-mode
    racer
    company-racer
    flycheck-rust
    julia-mode
    clojure-mode
    cider
    clj-refactor
    ess
    ess-R-data-view
    haskell-mode
    company-ghc
    powershell
    vbasense
    emacsql
    emacsql-mysql
    emacsql-psql
    emacsql-sqlite
    flycheck
    flycheck-popup-tip
    dumb-jump
    imenus
    imenu-anywhere
    imenu-list
    helm
    helm-smex
    helm-swoop
    helm-ag
    helm-gtags
    helm-descbinds
    helm-flyspell
    helm-elscreen
    helm-dash
    ac-helm
    auto-complete-nxml
    plantuml-mode
    flycheck-plantuml
    bm
    helm-bm
    projectile
    helm-projectile
    magit
    magit-find-file
    markdown-mode
    textile-mode
    restclient
    restclient-helm
    yasnippet
    yasnippet-snippets
    helm-c-yasnippet
    centered-cursor-mode
    quickrun
    emamux
    multi-term
    shell-pop
    scratch-pop
    which-key
    smart-mode-line
    dired-narrow
    dired-subtree
    peep-dired
    japanese-holidays
    osx-trash
    vimrc-mode
    x509-mode
    w3m
    mew
    docker
    ac-ispell
    google-this
    google-translate
    helm-google
    xah-lookup
    howdoi
    qiita
    yagist
    xclip
    osx-clipboard
    )
  "Packages to be installed.")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; ------------------------------------------------------------------------
;; load basic settings

(load-if-exist "~/dotfiles/elisp/cnf-basics.el")

;; ------------------------------------------------------------------------
;; increase priority cp932

(apply 'set-coding-system-priority
       (subst 'japanese-cp932 'japanese-shift-jis
              (coding-system-priority-list)))

;; ------------------------------------------------------------------------
;; modeline encoding

;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db

(defun my-coding-system-name-mnemonic (coding-system)
  "Detect coding system by CODING-SYSTEM."
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  "Detect return char by CODING-SYSTEM."
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system`."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; replace mode-line-mule-info encoding strings
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

;; ------------------------------------------------------------------------
;; modeline cleaner

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (abbrev-mode . "")
    (projectile-mode . "")
    (company-mode . " Comp")
    (editorconfig-mode . " EC")
    (elisp-slime-nav-mode . " EN")
    (flymake-mode . " FlyM")
    (helm-gtags-mode . " HG")
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (font-lock-mode . "")
    (ace-isearch-mode . "")
    (undo-tree-mode . "")
    (highlight-symbol-mode . "")
    (volatile-highlights-mode . "")
    (smooth-scroll-mode . "")
    (emmet-mode . " Em")
    ;; Major modes
    (fundamental-mode . "Fund")
    (generic-mode . "Gen")
    (default-generic-mode . "DGen")
    (emacs-lisp-mode . "El")
    (lisp-interaction-mode . "Li")
    (markdown-mode . "Md")
    (python-mode . "Py")
    (ruby-mode . "Rb")
    (rust-mode . "Rs")
    (shell-script-mode . "Sh")
    (js2-mode . "JS")
    (typescript-mode . "TS")
    (R-mode . "R")
    (visual-basic-mode . "VB")
    ))

(defun clean-mode-line ()
  "Clean up mode line."
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
;; eaw (ambiguous width characters)

;; https://github.com/uwabami/locale-eaw-emoji

(when (require 'locale-eaw-emoji nil t)
  (eaw-and-emoji-fullwidth)
  )

;; ------------------------------------------------------------------------
;; binary path (exec-path-from-shell)

;;(add-to-list 'exec-path "/opt/local/bin")
;;(add-to-list 'exec-path "/usr/bin")

(when (not (eq system-type 'windows-nt))
  (exec-path-from-shell-initialize)
  )

;; ------------------------------------------------------------------------
;; my-list-load

;; https://masutaka.net/chalow/2016-05-06-2.html

(defun my-lisp-load (filename)
  "Load Lisp from FILENAME."
  (let ((fullname (expand-file-name (concat "spec/" filename) user-emacs-directory)) lisp)
    (when (file-readable-p fullname)
      (with-temp-buffer
        (progn
          (insert-file-contents fullname)
          (setq lisp
                (condition-case nil
                    (read (current-buffer))
                  (error ()))))))
    lisp))

;; ------------------------------------------------------------------------
;; recnetf-ext

(autoload 'recentf-ext "recentf-ext" nil t)

;; ------------------------------------------------------------------------
;; elscreen

(require 'elscreen)
(elscreen-start)
(setq elscreen-prefix-key (kbd "C-z"))
(setq elscreen-display-tab 20)
(if (not window-system)
    (progn
      (setq elscreen-display-tab nil)
      ;; (elscreen-create)
      )
  )
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)
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
;; smooth-scroll

(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/vscroll-step-size 5)
(setq smooth-scroll/hscroll-step-size 5)

;; ------------------------------------------------------------------------
;; expand-region

;; (require 'expand-region)
(global-set-key (kbd "M-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; ------------------------------------------------------------------------
;; highlight-symbol

;; (require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange"
                                "DeepPink1"
                                "DodgerBlue1"
                                "HotPink"
                                "SlateBlue1"
                                "SpringGreen1"
                                "tan"
                                "LightSeaGreen")
      )
(setq highlight-symbol-idle-delay 1.0)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

;; ------------------------------------------------------------------------
;; sequential-command

(require 'sequential-command-config)
(global-set-key (kbd "C-a") 'seq-home)
(global-set-key (kbd "C-e") 'seq-end)

(when (require 'org nil t)
  (define-key org-mode-map (kbd "C-a") 'org-seq-home)
  (define-key org-mode-map (kbd "C-e") 'org-seq-end)
  )
(define-key esc-map "u" 'seq-upcase-backward-word)
(define-key esc-map "c" 'seq-capitalize-backward-word)
(define-key esc-map "l" 'seq-downcase-backward-word)

;; ------------------------------------------------------------------------
;; avy

(global-set-key (kbd "M-s") 'avy-goto-char)

;; ------------------------------------------------------------------------
;; ace-isearch

(global-ace-isearch-mode +1)
(setq ace-isearch-function 'avy-goto-char)
(setq ace-isearch-jump-delay 0.7)

;; ------------------------------------------------------------------------
;; anzu

;; (require 'anzu)
(global-anzu-mode +1)
;; (setq anzu-use-migemo t)
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
(setq foreign-regexp/regexp-type 'perl)
(setq reb-re-syntax 'foreign-regexp)

;; ------------------------------------------------------------------------
;; auto-complete

(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(ac-linum-workaround)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(ac-set-trigger-key "TAB")
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
(setq ac-menu-height 15)
(setq ac-max-width 35)
(setq ac-ignore-case t)
(setq ac-delay 0.1)
(setq ac-auto-start 2)
(setq ac-auto-show-menu 0.2)
(setq ac-quick-help-prefer-x t)

(if (<= emacs-major-version 25)
    (setq-default ac-sources 'ac-source-filename ac-source-words-in-same-mode-buffers)
  )
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; for filename completion (ac-source-filename should be earlier)
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))

;; ------------------------------------------------------------------------
;; org-ac

(when (require 'org-ac nil t)
  (org-ac/config-default)
  )

;; ------------------------------------------------------------------------
;; company

(require 'company)
;; (global-company-mode t)
(if window-system
    (progn
      (company-quickhelp-mode t) ;; only support GUI
      ))

(setq completion-ignore-case t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-code-other-buffers (quote all))
(setq company-dabbrev-other-buffers (quote all))
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-tooltip-align-annotations t)

(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
;; (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; ------------------------------------------------------------------------
;; dired-narrow

;; key-bind
(define-key dired-mode-map (kbd "M-s") 'dired-narrow-fuzzy)

;; ------------------------------------------------------------------------
;; dired-subtree

;; (require 'dired-subtree)
(defun dired-subtree-up-dwim (&optional arg)
  "Traval parent directory."
  (interactive "p")
  (or (dired-subtree-up arg)
      (dired-up-directory)))
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)

;; ------------------------------------------------------------------------
;; peep-dired

(define-key dired-mode-map (kbd "P") 'peep-dired)
(define-key dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down)
(define-key dired-mode-map (kbd "S-<SPC>") 'peep-dired-scroll-page-up)
(setq peep-dired-ignored-extensions '("mkv" "avi" "iso" "mp4" "mp4"))

;; ------------------------------------------------------------------------
;; rainbow-mode

(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)

;; ------------------------------------------------------------------------
;; rainbow-delimiters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(setq rainbow-delimiters-outermost-only-face-count 1)

(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  "Rainbow delimiter more vivid colors."
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
;; (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; ------------------------------------------------------------------------
;; multiple-cursors

(require 'multiple-cursors)

;; ------------------------------------------------------------------------
;; imenu-list

(setq imenu-list-position "below")

;; ------------------------------------------------------------------------
;; load helm settings

(load-if-exist "~/dotfiles/elisp/cnf-helm.el")

;; ------------------------------------------------------------------------
;; load mew settings

(load-if-exist "~/cnf-mew.el")

;; ------------------------------------------------------------------------
;; flyspell (spell check)

(setq ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; only enable comment
(mapc
 (lambda (hook) (add-hook hook 'flyspell-prog-mode))
 '(emacs-lisp-mode-hook))
;; enable all
(mapc
 (lambda (hook) (add-hook hook '(lambda () (flyspell-mode 1))))
 '(text-mode-hook))

;; (add-hook 'find-file-hook 'flyspell-mode)
;; (add-hook 'find-file-hook 'flyspell-buffer)

;; > sudo port install aspell aspell-dict-en

;; ------------------------------------------------------------------------
;; ac-ispell

;; Completion words longer than 4 characters

(eval-after-load 'auto-complete '(progn (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(setq ac-ispell-fuzzy-limit 4)
(setq ac-ispell-requires 4)

;; ------------------------------------------------------------------------
;; undohist

(require 'undohist)
(undohist-initialize)
(setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG"))

;; ------------------------------------------------------------------------
;; volatile-highlights

;; https://github.com/k-talo/volatile-highlights.el

(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

;; ------------------------------------------------------------------------
;; os switch

(cond ((equal system-type 'gnu/linux)
       (load-if-exist "~/dotfiles/elisp/cnf-linux.el")
       (load-if-exist "~/dotfiles/elisp/cnf-browser.el")
       )
      ((equal system-type 'windows-nt)
       (load-if-exist "~/dotfiles/elisp/cnf-windows-nt.el")
       )
      ((equal system-type 'darwin)
       (load-if-exist "~/dotfiles/elisp/cnf-osx.el")
       (load-if-exist "~/dotfiles/elisp/cnf-browser.el")
       )
      )

;; ------------------------------------------------------------------------
;; shell-mode

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ------------------------------------------------------------------------
;; shell-pop

(when (not (eq system-type 'windows-nt))
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  (setq shell-pop-window-size 30)
  ;; (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  ;; (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
  (setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
  ;; (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (global-set-key (kbd "C-c s") 'shell-pop)
)

;; ------------------------------------------------------------------------
;; multi-term

(setq multi-term-program shell-file-name)
(add-hook
 'term-mode-hook
 '(lambda ()
    (define-key term-raw-map "\C-y" 'term-paste)
    (define-key term-raw-map "\C-q" 'move-beginning-of-line)
    (define-key term-raw-map "\C-f" 'forward-char)
    (define-key term-raw-map "\C-b" 'backward-char)
    (define-key term-raw-map "\C-t" 'set-mark-command)
    (define-key term-raw-map "\C-p" 'term-send-up)
    (define-key term-raw-map "\C-n" 'term-send-down)
    (define-key term-raw-map "\C-h" 'term-send-backspace)
    (define-key term-raw-map (kbd "ESC") 'term-send-raw)
    (define-key term-raw-map [delete] 'term-send-raw)
    (define-key term-raw-map [mouse-4] 'term-send-up)
    (define-key term-raw-map [mouse-5] 'term-send-down)
    (define-key term-raw-map "\C-z"
      (lookup-key (current-global-map) "\C-z"))))
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)

;; (with-eval-after-load "multi-term"
;;   (setenv "TERMINFO" "~/.terminfo")
;;   (setenv "HOSTTYPE" "intel-mac"))

;; ------------------------------------------------------------------------
;; popwin

(require 'popwin)
(popwin-mode 1)
(setq pop-up-windows t)
(setq special-display-function 'popwin:special-display-popup-window)
;; (push '(dired-mode :position top) popwin:special-display-config)
(push '(compilation-mode :noselect t) popwin:special-display-config)
(push '("*grep*" :noselect t) popwin:special-display-config)
(push '("\\*e?shell\\*" :regexp t :height 15) popwin:special-display-config)
(push '("*bash*" :height 15) popwin:special-display-config)
(push '("*cmd*" :height 15) popwin:special-display-config)
(push '("*PowerShell*" :height 15) popwin:special-display-config)
(push '("*compilation*" :height 15) popwin:special-display-config)
(push '("*quickrun*" :height 15) popwin:special-display-config)
;; (push '("*comment-tags*" :height 15) popwin:special-display-config)
(push '("*Flycheck errors*" :height 15) popwin:special-display-config)
(push '("*Typescript*" :height 15) popwin:special-display-config)
(push '("*ruby*" :height 15) popwin:special-display-config)
(push '("*pry*" :height 15) popwin:special-display-config)
(push '("\\*[Ii]?[Pp]ython.+" :regexp t :height 15) popwin:special-display-config)
(push '("*SQL*" :height 15) popwin:special-display-config)
(push '("*Ilist*" :height 15) popwin:special-display-config)
(push '("*eww*" :height 20 :noselect t) popwin:special-display-config)
(push '("*wclock*" :height 7) popwin:special-display-config)
(push '(" *undo-tree*" :width 0.2 :position right) popwin:special-display-config)
(push '("\\*docker\\-.+" :regexp t :height 15) popwin:special-display-config)
(push '("*HTTP Response*" :height 15) popwin:special-display-config)
(push '("COMMIT_EDITMSG" :height 15) popwin:special-display-config)

;; ------------------------------------------------------------------------
;; import-popwin

(global-set-key (kbd "M-g i") 'import-popwin)

;; ------------------------------------------------------------------------
;; emamux

(setq emamux:completing-read-type (quote helm))

;; ------------------------------------------------------------------------
;; scratch-pop

;; http://emacs.rubikitch.com/scratch-pop/

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

(which-key-setup-side-window-bottom) ; mini buffer (-right, -right-bottom)
(which-key-mode 1)

;; ------------------------------------------------------------------------
;; centered-cursor-mode

(add-hook 'isearch-mode-hook #'(lambda () (centered-cursor-mode 1)))
(add-hook 'isearch-mode-end-hook #'(lambda () (centered-cursor-mode -1)))

;; ------------------------------------------------------------------------
;; undo-tree

(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; ------------------------------------------------------------------------
;; modeline-char

(when (require 'modeline-char nil t))

;; ------------------------------------------------------------------------
;; smart-mode-line

(require 'smart-mode-line)
;; bug hack
(setq sml/active-background-color "Gray30")
(setq sml/read-only-char "%%")
(setq sml/modified-char "*")
;; hide Helm and auto-complete
(setq sml/hidden-modes '(" Helm" " yas" " VHl" " WK"
                         " Fly" " EC" " ARev" " Anzu" " _+_"))
;; hack (privent overflow)
(setq sml/extra-filler -10)
;; (add-to-list 'sml/replacer-regexp-list '("^.+/junk/[0-9]+/" ":J:") t)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark) ; ; theme: respectful / light

;; ------------------------------------------------------------------------
;; load other settings

(load-if-exist "~/dotfiles/elisp/cnf-calendar.el")
(load-if-exist "~/dotfiles/elisp/cnf-webservice.el")
(load-if-exist "~/dotfiles/elisp/cnf-program.el")
(load-if-exist "~/dotfiles/elisp/cnf-user.el")

;; ------------------------------------------------------------------------
;; show load time

(add-hook
 'after-init-hook
 (lambda ()
   (message "init time: %.3f sec"
            (float-time (time-subtract after-init-time before-init-time)))))

;; ------------------------------------------------------------------------

(setq initial-scratch-message ";; saitamacs ok\n")

;; ------------------------------------------------------------------------

(provide 'init-fullmacs.el)
;;; init-fullmacs.el ends here
