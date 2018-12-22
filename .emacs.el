;;; .emacs.el --- thesaitama Emacs configuration

;;  _   _                     _ _
;; | |_| |__   ___  ___  __ _(_) |_ __ _ _ __ ___   __ _
;; | __| '_ \ / _ \/ __|/ _` | | __/ _` | '_ ` _ \ / _` |
;; | |_| | | |  __/\__ \ (_| | | || (_| | | | | | | (_| |
;;  \__|_| |_|\___||___/\__,_|_|\__\__,_|_| |_| |_|\__,_|

;;; Commentary:
;;
;; thesaitama@ .emacs.el
;; Last Update: 2018-12-20 22:31:17
;; tested with: Emacs 26.1, macOS 10.14, Windows 10

;; install
;; > sudo apt-get install libncurses5-dev libgnutls-openssl27 libgnutls28-dev
;; > wget http://ftpmirror.gnu.org/emacs/emacs-26.1.tar.gz
;; > tar xzvf emacs-26.1.tar.gz
;; > cd emacs-26.1
;; > ./configure --without-x
;; > make
;; > sudo make install

;;; Code:

;; enable cl
(eval-when-compile (require 'cl))
(require 'cl-lib)

;; inhibit warnings
(setq byte-compile-warnings '(free-vars bytecomp))
(setq ad-redefinition-action 'accept)

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
    powershell
    vbasense
    emacsql
    emacsql-mysql
    emacsql-psql
    emacsql-sqlite
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
    japanese-holidays
    osx-trash
    vimrc-mode
    x509-mode
    w3m
    mew
    docker
    ac-ispell
    google-translate
    xah-lookup
    howdoi
    qiita
    yagist
    xclip
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
;; utility function

(defun load-if-exist (file-path)
  "Load file if FILE-PATH is exist."
  (if (file-exists-p file-path)
      (load file-path))
  )

;; ------------------------------------------------------------------------
;; load basic settings

(load-if-exist "~/dotfiles/elisp/cnf-basics.el")

;; ------------------------------------------------------------------------
;; modeline cleaner

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (abbrev-mode . "")
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

(require 'expand-region)
(global-set-key (kbd "M-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; ------------------------------------------------------------------------
;; highlight-symbol

(require 'highlight-symbol)
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

(require 'anzu)
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
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

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

(if window-system (progn
                    (company-quickhelp-mode t) ;; only support GUI
                    ))

(setq completion-ignore-case t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase nil)
(setq company-transformers '(company-sort-by-backend-importance))

(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
;; (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

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
;; dired-narrow

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

(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)

;; ------------------------------------------------------------------------
;; rainbow-delimiters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  "Rainbow delimiter more vivid colors."
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; ------------------------------------------------------------------------
;; multiple-cursors

(require 'multiple-cursors)

;; ------------------------------------------------------------------------
;; imenu

(setq imenu-auto-rescan t)

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
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   emacs-lisp-mode-hook
   ))
;; enable all
(mapc
   (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
   '(
     ;; markdown-mode-hook
     text-mode-hook
     ))

;; (add-hook 'find-file-hook 'flyspell-mode)
;; (add-hook 'find-file-hook 'flyspell-buffer)

;; > sudo port install aspell
;; > sudo port install aspell-dict-en

;; ------------------------------------------------------------------------
;; ac-ispell

;; Completion words longer than 4 characters

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)

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

(load-if-exist "~/dotfiles/elisp/cnf-webservice.el")
(load-if-exist "~/dotfiles/elisp/cnf-program.el")
(load-if-exist "~/dotfiles/elisp/cnf-user.el")

;; ------------------------------------------------------------------------
;; shell-mode

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

(when (not (eq system-type 'windows-nt))
  ;; (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  ;; (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
  (setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
  ;; (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (global-set-key (kbd "C-c s") 'shell-pop)
)

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

(which-key-setup-side-window-bottom) ; mini buffer
;; (which-key-setup-side-window-right)
;; (which-key-setup-side-window-right-bottom)

(which-key-mode 1)

;; ------------------------------------------------------------------------
;; centered-cursor-mode

(add-hook 'isearch-mode-hook
          #'(lambda () (centered-cursor-mode 1)))
(add-hook 'isearch-mode-end-hook
          #'(lambda () (centered-cursor-mode -1)))

;; ------------------------------------------------------------------------
;; undo-tree

(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; (defun undo-tree-split-side-by-side (original-function &rest args)
;;   "Split undo-tree side-by-side."
;;   (let ((split-height-threshold nil)
;;         (split-width-threshold 0))
;;     (apply original-function args)))
;; (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

;; ------------------------------------------------------------------------
;; smart-mode-line
;; basic modeline setting in cnf-basics.el

(require 'smart-mode-line)
;; bug hack
(setq sml/active-background-color "Gray60")
(setq sml/read-only-char "%%")
(setq sml/modified-char "*")
;; hide Helm and auto-complete
(setq sml/hidden-modes '(" Helm" " yas" " VHl" " WK" " Fly" " EC" " ARev" " Anzu"))
;; hack (privent overflow)
(setq sml/extra-filler -10)
;; sml/replacer-regexp-list
;; (add-to-list 'sml/replacer-regexp-list '("^.+/junk/[0-9]+/" ":J:") t)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; theme
;; (sml/apply-theme 'respectful)
;; (sml/apply-theme 'light)
(sml/apply-theme 'dark)

;; ------------------------------------------------------------------------
;; load calendar settings

(load-if-exist "~/dotfiles/elisp/cnf-calendar.el")

;; ------------------------------------------------------------------------
;; custom-set-faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "color-28"))))
 '(bm-fringe-face ((t (:background "color-28"))))
 '(company-preview-common ((t (:background nil :foreground "LightGray" :underline t))))
 '(company-scrollbar-bg ((t (:background "Gray40"))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-tooltip ((t (:foreground "black" :background "LightGray"))))
 '(company-tooltip-common ((t (:foreground "black" :background "LightGray"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "SteelBlue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "SteelBlue"))))
 '(diff-added ((((type tty)) (:foreground "green"))))
 '(diff-refine-added ((t (:foreground "white" :background "LimeGreen"))))
 '(diff-refine-removed ((t (:foreground "white" :background "red"))))
 '(diff-removed ((((type tty)) (:foreground "red"))))
 '(dired-header ((t (:background "DarkCyan" :foreground "white"))))
 '(dired-subtree-depth-1-face ((t (:background "Gray19"))))
 '(dired-subtree-depth-2-face ((t (:background "Gray20"))))
 '(dired-subtree-depth-3-face ((t (:background "Gray21"))))
 '(dired-subtree-depth-4-face ((t (:background "Gray22"))))
 '(dired-subtree-depth-5-face ((t (:background "Gray23"))))
 '(dired-subtree-depth-6-face ((t (:background "Gray24"))))
 '(elscreen-tab-background-face ((t (:background "Gray10" :foreground "Gray90"))))
 '(elscreen-tab-control-face ((t (:background "Gray20" :foreground "Gray90"))))
 '(elscreen-tab-current-screen-face ((t (:background "Gray80" :foreground "Gray20"))))
 '(elscreen-tab-other-screen-face ((t (:background "Gray25" :foreground "Gray80"))))
 '(header-line ((t (:background "Gray40" :foreground "Gray85"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-buffer-process ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-ff-directory ((t (:background "Gray25" :foreground "orange"))))
 '(helm-ff-dotted-directory ((t (:background "Gray25" :foreground "white"))))
 '(helm-ff-executable ((t (:inherit font-lock-builtin-face :foreground "cyan"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-symlink ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-grep-file ((t (:inherit font-lock-builtin-face :underline t :foreground "cyan"))))
 '(helm-grep-match ((t (:background "LightCyan" :foreground "black"))))
 '(helm-header ((t (:background "Gray40" :foreground "Gray80"))))
 '(helm-match ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "Gray30"))))
 '(helm-selection-line ((t (:background "Gray20"))))
 '(helm-source-header ((t (:background "DarkCyan" :foreground "white"))))
 '(helm-visible-mark ((t (:background "Gray40"))))
 '(highlight-symbol-face ((t (:background "Gray25"))))
 '(hl-line ((t (:background "color-236"))))
 '(holiday ((t (:background "pink" :foreground "black"))))
 '(isearch ((t (:background "LightPink" :foreground "black"))))
 '(japanese-holiday-saturday ((t (:background "cyan" :foreground "black"))))
 '(lazy-highlight ((t (:background "LightCyan" :foreground "black"))))
 '(link ((t (:foreground "cyan"))))
 '(linum ((t (:inherit (shadow default) :background "Gray22"))))
 '(magit-branch-local ((t (:foreground "magenta"))))
 '(magit-branch-remote ((t (:foreground "Cornflower"))))
 '(magit-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((t (:background "Gray23"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-log-author ((t (:foreground "magentap"))))
 '(magit-section-heading ((t (:foreground "cyan" :weight bold))))
 '(magit-section-highlight ((t (:background "Gray23"))))
 '(markdown-code-face ((t (:inherit default :background "Gray20"))))
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1))))
 '(markdown-header-face-2 ((t (:inherit outline-2))))
 '(markdown-header-face-3 ((t (:inherit outline-3))))
 '(markdown-header-face-4 ((t (:inherit outline-4))))
 '(markdown-header-face-5 ((t (:inherit outline-5))))
 '(markdown-header-face-6 ((t (:inherit outline-6))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(menu ((t (:background "Gray30"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(nxml-attribute-local-name ((t (:foreground "LightBlue"))))
 '(nxml-attribute-value ((t (:foreground "Goldenrod"))))
 '(nxml-cdata-section-content ((t (:foreground "gray"))))
 '(nxml-comment-content ((t (:inherit font-lock-comment-face))))
 '(nxml-comment-delimiter ((t (:foreground "green"))))
 '(nxml-element-local-name ((t (:foreground "cyan"))))
 '(nxml-entity-ref-delimiter ((t (:foreground "red"))))
 '(nxml-entity-ref-name ((t (:foreground "red"))))
 '(nxml-name-face ((t (:foreground "cyan"))))
 '(nxml-tag-delimiter ((t (:foreground "LightBlue"))))
 '(outline-1 ((t (:foreground "LightBlue" :weight bold :underline t))))
 '(outline-2 ((t (:foreground "LightBlue" :weight bold))))
 '(outline-3 ((t (:foreground "cyan" :weight bold))))
 '(outline-4 ((t (:foreground "orange" :weight bold))))
 '(outline-5 ((t (:foreground "goldenrod" :weight bold))))
 '(outline-6 ((t (:foreground "orange"))))
 '(outline-7 ((t (:foreground "goldenrod"))))
 '(package ((t (:foreground "DodgerBlue"))))
 '(package-name ((t (:foreground "DodgerBlue"))))
 '(pulse-highlight-face ((t (:background "Gray35"))))
 '(pulse-highlight-start-face ((t (:background "Gray35"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "red" :foreground "white"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "white"))))
 '(region ((t (:background "Gray40"))))
 '(rst-level-1 ((t (:inherit outline-1))))
 '(rst-level-2 ((t (:inherit outline-2))))
 '(rst-level-3 ((t (:inherit outline-3))))
 '(rst-level-4 ((t (:inherit outline-4))))
 '(rst-level-5 ((t (:inherit outline-5))))
 '(rst-level-6 ((t (:inherit outline-6))))
 '(show-paren-match ((t (:background "DodgerBlue" :foreground "white"))))
 '(show-paren-match-expression ((t (:background "Gray25"))))
 '(show-paren-match-face ((t (:background "black" :foreground "white"))))
 '(show-paren-mismatch ((t (:background "red"))))
 '(tide-hl-identifier-face ((t (:background "Gray28"))))
 '(tool-bar ((t (:foreground "cyan"))))
 '(tty-menu-disabled-face ((t (:background "Gray45" :foreground "Gray10"))))
 '(tty-menu-enabled-face ((t (:background "Gray45" :foreground "White"))))
 '(tty-menu-selected-face ((t (:background "SteelBlue" :foreground "White"))))
 '(web-mode-comment-face ((t (:inherit font-lock-comment-face))))
 '(web-mode-css-at-rule-face ((t (:foreground "magenta"))))
 '(web-mode-css-pseudo-class ((t (:foreground "DodgerBlue"))))
 '(web-mode-css-selector-face ((t (:foreground "DodgerBlue"))))
 '(web-mode-current-element-highlight-face ((t (:background "Gray25"))))
 '(web-mode-doctype-face ((t (:inherit font-lock-doc-face))))
 '(web-mode-html-attr-equal-face ((t (:foreground "white"))))
 '(web-mode-html-attr-name-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face))))
 '(web-mode-html-entity-face ((t (:foreground "DodgerBlue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan"))))
 '(web-mode-server-comment-face ((t (:foreground "green"))))
 '(which-func ((t (:foreground "ivory"))))
 '(which-key-command-description-face ((t (:foreground "white")))))

;; ------------------------------------------------------------------------
;; custom-set-variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(column-number-mode t)
 '(company-dabbrev-code-other-buffers (quote all))
 '(company-dabbrev-other-buffers (quote all))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(elpy-modules (quote (elpy-module-eldoc elpy-module-yasnippet)) t)
 '(emamux:completing-read-type (quote helm))
 '(foreign-regexp/regexp-type (quote perl))
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en")
 '(helm-ag-base-command "ag --nogroup --ignore-case")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-projectile-files-list)))
 '(package-selected-packages
   (quote
    (powershell flycheck-plantuml ssh plantuml-mode smooth-scroll tide helm-dash vimrc-mode helm-flyspell howdoi google-translate xah-lookup osx-trash japanese-holidays dired-subtree dired-narrow w3m smart-mode-line which-key scratch-pop shell-pop multi-term popwin elscreen emamux magit-find-file magit helm-projectile projectile yagist qiita helm-c-yasnippet yasnippet-snippets restclient-helm restclient helm-bm bm helm-descbinds helm-gtags helm-ag helm-smex imenu-list imenu-anywhere imenus flycheck-popup-tip flycheck elpy jedi python-mode yaml-mode typescript-mode json-mode js2-refactor php-eldoc web-mode rainbow-delimiters rainbow-mode comment-tags undo-tree foreign-regexp highlight-symbol expand-region anzu ac-helm ac-php ac-js2 ac-html quickrun editorconfig sequential-command fuzzy avy pos-tip auto-complete package-utils exec-path-from-shell 0xc)))
 '(popwin-mode t)
 '(reb-re-syntax (quote foreign-regexp))
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("multi-term" "*terminal<1>*"
     (quote
      (lambda nil
        (multi-term))))) t)
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(show-paren-mode t)
 '(size-indication-mode t))

(put 'set-goal-column 'disabled nil)

;; ------------------------------------------------------------------------

(setq initial-scratch-message ";; saitamacs ok\n")

;; ------------------------------------------------------------------------

(provide '.emacs.el)
;;; .emacs.el ends here
