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
  '(exec-path-from-shell
    0xc
    package-utils
    auto-complete
    pos-tip
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
    web-beautify
    php-mode
    php-eldoc
    rbenv
    ruby-electric
    inf-ruby
    robe
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
    helm-ag
    helm-gtags
    helm-descbinds
    helm-flyspell
    bm
    helm-bm
    restclient
    restclient-helm
    yasnippet
    yasnippet-snippets
    helm-c-yasnippet
    qiita
    yagist
    projectile
    helm-projectile
    magit
    magit-find-file
    emamux
    elscreen
    popwin
    import-popwin
    multi-term
    shell-pop
    scratch-pop
    which-key
    smart-mode-line
    w3m
    dired-narrow
    dired-subtree
    japanese-holidays
    osx-trash
    xah-lookup
    google-translate
    howdoi
    )
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
;; clenad modeline

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
;; eaw (ambiguous width characters)

;; https://github.com/uwabami/locale-eaw-emoji

(require 'locale-eaw-emoji)
(eaw-and-emoji-fullwidth)

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

;; (require 'auto-install)
;; (setq auto-install-use-wget t)
;; (setq auto-install-directory "~/.emacs.d/auto-install/")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)

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
(setq ac-quick-help-prefer-x t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq-default ac-sources 'ac-source-words-in-same-mode-buffers)
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

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
;; flyspell (spell check)

(setq-default flyspell-mode t)
(setq ispell-program-name "aspell")
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;(add-hook 'find-file-hook 'flyspell-mode)
;(add-hook 'find-file-hook 'flyspell-buffer)

;; > sudo port install aspell
;; > sudo port install aspell-dict-en

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
       (load "~/dotfiles/cnf-webservice.el")
       (load "~/dotfiles/cnf-program.el")
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
(push '("*grep*" :noselect t) popwin:special-display-config)
(push '("*quickrun*" :height 15) popwin:special-display-config)
(push '("*ruby*" :height 15) popwin:special-display-config)
(push '("*Ilist*" :height 15) popwin:special-display-config)
(push '("*wclock*" :height 5) popwin:special-display-config)
(push '(" *undo-tree*" :width 0.2 :position right) popwin:special-display-config)
(push '("*comment-tags*" :height 15) popwin:special-display-config) ;; not work
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
;; smart-mode-line
;; basic modeline setting in cnf-basics.el

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
;; custom-set-faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "color-28"))))
 '(bm-fringe-face ((t (:background "color-28"))))
 '(diff-added ((((type tty)) (:foreground "green"))))
 '(diff-removed ((((type tty)) (:foreground "red"))))
 '(dired-header ((t (:background "BrightBlue" :foreground "white"))))
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
 '(font-lock-doc-face ((t (:foreground "green"))))
 '(fringe ((t (:background "Gray12" :foreground "blue"))))
 '(helm-buffer-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-buffer-process ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-ff-directory ((t (:background "Gray25" :foreground "orange"))))
 '(helm-ff-dotted-directory ((t (:background "Gray25" :foreground "white"))))
 '(helm-ff-executable ((t (:inherit font-lock-builtin-face :foreground "cyan"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(helm-ff-symlink ((t (:inherit font-lock-builtin-face :foreground "magenta"))))
 '(helm-match ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "Gray30"))))
 '(helm-source-header ((t (:background "BrightBlue" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "Gray25"))))
 '(hl-line ((t (:background "color-236"))))
 '(holiday ((t (:background "pink"))))
 '(isearch ((t (:background "LightPink" :foreground "black"))))
 '(japanese-holiday-saturday ((t (:background "cyan"))))
 '(lazy-highlight ((t (:background "LightCyan" :foreground "black"))))
 '(link ((t (:foreground "blue"))))
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
 '(magit-log-author ((t (:foreground "magenta"))))
 '(magit-section-highlight ((t (:background "Gray23"))))
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold))))
 '(markdown-pre-face ((t (:foreground "ivory"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(outline-1 ((t (:background "BrightBlue" :foreground "white"))))
 '(outline-2 ((t (:foreground "cyan"))))
 '(outline-3 ((t (:foreground "blue"))))
 '(outline-4 ((t (:foreground "goldenrod"))))
 '(package-name ((t (:foreground "blue"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "red" :foreground "white"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "white"))))
 '(region ((t (:background "Gray40"))))
 '(tool-bar ((t (:foreground "cyan"))))
 '(web-mode-comment-face ((t (:foreground "green"))))
 '(web-mode-css-at-rule-face ((t (:foreground "magenta"))))
 '(web-mode-css-pseudo-class ((t (:foreground "blue"))))
 '(web-mode-css-selector-face ((t (:foreground "blue"))))
 '(web-mode-doctype-face ((t (:foreground "glay"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "white"))))
 '(web-mode-html-attr-name-face ((t (:foreground "LightBlue"))))
 '(web-mode-html-attr-value-face ((t (:foreground "yellow"))))
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
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
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
    (helm-flyspell howdoi google-translate xah-lookup osx-trash japanese-holidays dired-subtree dired-narrow w3m smart-mode-line which-key scratch-pop shell-pop multi-term popwin elscreen emamux magit-find-file magit helm-projectile projectile yagist qiita helm-c-yasnippet yasnippet-snippets restclient-helm restclient helm-bm bm helm-descbinds helm-gtags helm-ag helm-smex imenu-list imenu-anywhere imenus flycheck-popup-tip flycheck elpy jedi python-mode yaml-mode tss typescript-mode json-mode js2-refactor php-eldoc web-mode rainbow-delimiters rainbow-mode comment-tags undo-tree foreign-regexp highlight-symbol expand-region anzu ac-helm ac-php ac-js2 ac-html quickrun editorconfig sequential-command fuzzy avy pos-tip auto-complete package-utils exec-path-from-shell 0xc)))
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
