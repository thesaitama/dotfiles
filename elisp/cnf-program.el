;;; cnf-program.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; elisp-mode

(defun setup-lisp-mode ()
  "Setup lisp-mode."
  (flyspell-prog-mode)
  )
(add-hook 'emacs-lisp-mode-hook #'setup-lisp-mode)
(add-hook 'lisp-mode-hook #'setup-lisp-mode)

;; ------------------------------------------------------------------------
;; editorconfig-mode

(editorconfig-mode 1)
(set-variable 'editorconfig-get-properties-function
              'editorconfig-core-get-properties-hash)

;; if you need editorconfig executable
;; > sudo port install readline editorconfig-core-c
;; (setq edconf-exec-path "/opt/local/bin/editorconfig")

;; ------------------------------------------------------------------------
;; webservers

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

;; ------------------------------------------------------------------------
;; eldoc-mode

(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; ------------------------------------------------------------------------
;; dockerfile-mode

(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ------------------------------------------------------------------------
;; comment-tags

(set-variable 'comment-tags-keymap-prefix (kbd "C-c t"))
(set-variable 'comment-tags-keyword-faces
              `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
                ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
                ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
                ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
                ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
                ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
                ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
                ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
(set-variable 'comment-tags-comment-start-only t)
(set-variable 'comment-tags-require-colon t)
(set-variable 'comment-tags-case-sensitive t)
(set-variable 'comment-tags-show-faces t)
(set-variable 'comment-tags-lighter nil)
(add-hook 'prog-mode-hook 'comment-tags-mode)

;; ------------------------------------------------------------------------
;; projectile

(set-variable 'projectile-keymap-prefix (kbd "C-c p"))
(set-variable 'projectile-completion-system 'helm)
(set-variable 'projectile-switch-project-action 'projectile-dired)
;; (set-variable 'projectile-switch-project-action 'helm-projectile)
(set-variable 'projectile-enable-caching t)
(projectile-global-mode)

;; ------------------------------------------------------------------------
;; helm-projectile

(when (require 'helm-projectile nil t)
  (helm-projectile-on)
  (set-variable
   'helm-mini-default-sources '(helm-source-buffers-list
                                helm-source-recentf
                                helm-source-projectile-files-list)))
(define-key global-map (kbd "C-c h") 'helm-mini)

;; ------------------------------------------------------------------------
;; debugger

(add-hook 'gdb-mode-hook #'(lambda () (gud-tooltip-mode t)))
(defvar gdb-many-windows t)
(defvar gdb-use-separate-io-buffer t)
(defvar gud-tooltip-echo-area nil)
(defvar gdb-command-name "ggdb")

;; install on macOS need codesign
;; > codesign -s gdb-cert /opt/local/bin/ggdb
;;
;; basic usage
;; 1. compile with -g option
;; > gcc -g -o test test.c
;; 2. run gdb
;; > ggdb -i=mi test

;; ------------------------------------------------------------------------
;; imenu-list

(defvar imenu-list-position "below")

;; ------------------------------------------------------------------------
;; flycheck

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(set-variable 'flycheck-idle-change-delay 3)  ; important
(set-variable 'flycheck-display-errors-delay 0.5)
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'html-tidy 'web-mode)

;; ------------------------------------------------------------------------
;; cc-mode (built-in)

(setq auto-mode-alist
      (append
       '(("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ("\\.c\\+\\+$" . c++-mode)
         ("\\.cpp$". c++-mode)
         ("\\.cc$" . c++-mode)
         ("\\.hh$" . c++-mode))
       auto-mode-alist))
(add-hook 'c-mode-common-hook
 #'(lambda ()
    (c-set-style "gnu")
    (c-set-offset 'case-label 2)
    (setq c-tab-always-indent t)
    (setq compilation-window-height 8)
    (define-key c-mode-map "\C-cd" 'gdb)
    (define-key c-mode-map "\C-cc" 'compile)
    ))

;; ------------------------------------------------------------------------
;; php-mode, ac-php + php-eldoc

(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\(\\.php\\|\\.tpl\\)$" . php-mode))
(defun setup-php-mode ()
  "Setup php-mode."
  (set-variable 'php-mode-force-pear t)
  (set-variable 'php-manual-url 'ja)
  (set-variable 'php-mode-coding-style 'psr2)
  (set-variable 'php-template-compatibility nil)
  (flycheck-mode t)
  (auto-complete-mode t)
  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")
  (when (require 'ac-php nil t)
    (setq ac-sources '(ac-source-php
                       ac-source-filename
                       ac-source-words-in-same-mode-buffers)))
  (when (require 'php-eldoc nil t)
    (php-eldoc-enable))
  (when (eq 0 (buffer-size))
    (insert "<?php\n\n"))
  )

(add-hook 'php-mode-hook #'setup-php-mode)

;; manual install
;; > wget http://jp.php.net/get/php_manual_ja.tar.gz/from/this/mirror -O php_manual_ja.tar.gz
;; > tar xfvz php_manual_ja.tar.gz
;; > sudo mkdir -p /usr/local/share/php/doc/
;; > sudo cp -r php-chunked-xhtml /usr/local/share/php/doc/
;; > rm -rf php-chunked-xhtml

;; ------------------------------------------------------------------------
;; cperl-mode

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; ------------------------------------------------------------------------
;; web-mode, emmet-mode + ac-emmet

;; (when (require 'web-mode nil t))
(autoload 'web-mode "web-mode" nil t)
(setq auto-mode-alist
      (append
       '(("\\.as[cp]x$" . web-mode)
         ("\\.iht$" . web-mode)
         ("\\.[agj]sp$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.[xp]?html?$" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\(\\.sass\\|\\.s?css\\)$" . web-mode)
         )
       auto-mode-alist))
(setq web-mode-engines-alist
      '(("php" . "\\.phtml\\'")
        ;; ("django" . "\\.html\\'")
        ("blade" . "\\.blade\\."))
      )
(setq-default web-mode-ac-sources-alist
              '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
                ("css" . (ac-source-css-property ac-source-emmet-css-snippets))
                ("html" . (ac-source-emmet-html-aliases
                           ac-source-emmet-html-snippets
                           ac-source-words-in-buffer
                           ac-source-abbrev))))
(defun web-mode-setup ()
  "Hooks for Web mode."
  (hs-minor-mode -1)
  (set-variable 'web-mode-markup-indent-offset 2)
  (set-variable 'web-mode-code-indent-offset 2)
  (set-variable 'web-mode-css-indent-offset 2)
  (set-variable 'web-mode-auto-close-style 2)
  (set-variable 'web-mode-enable-auto-pairing t)
  (set-variable 'web-mode-enable-auto-closing t)
  (set-variable 'web-mode-enable-current-element-highlight t)
  (set-variable 'web-mode-enable-current-column-highlight t)
  ;; (set-variable 'web-mode-enable-css-colorization t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  ;; (auto-complete-mode t)
  (add-node-modules-path)
  (emmet-mode)
  (ac-emmet-html-setup)
  (cond
     ;; ((string= web-mode-content-type "html")
     ;;  (when (executable-find "tidy") (flycheck-select-checker 'html-tidy)))
     ((or (string= web-mode-content-type "javascript") (string= web-mode-content-type "jsx"))
      (when (executable-find "eslint") (flycheck-select-checker 'javascript-eslint))
      (prettier-js-mode)))
  )
(add-hook 'web-mode-hook 'web-mode-setup)
(add-hook 'web-mode-before-auto-complete-hooks
          #'(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; > sudo port install tidy

;; ------------------------------------------------------------------------
;; web-beautify

(setq-default web-beautify-args
       '("-f"
         "-"
         "--indent-size 2"
         "--end-with-newline"))

;; > sudo npm install -g install js-beautify

;; ------------------------------------------------------------------------
;; js2-mode

;; (when (require 'js2-mode nil t))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (js2-refactor-mode)
            (ac-js2-mode)
            (add-node-modules-path)
            )
          )
(defvar ac-js2-evaluate-calls t)

;; > sudo npm install -g eslint babel-eslint

;; ------------------------------------------------------------------------
;; tern

(defvar tern-command '("tern" "--no-port-file"))
(add-hook 'js2-mode-hook (lambda ()(tern-mode t)))

(eval-after-load 'tern
  '(progn
     (when (require 'tern-auto-complete nil t)
       (tern-ac-setup))
     ))

;; > sudo npm install -g tern

;; ------------------------------------------------------------------------
;; json-mode

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(defun setup-json-mode ()
  "Setup json-mode."
  (flycheck-mode t)
  (flyspell-prog-mode)
  )
(add-hook 'json-mode-hook #'setup-json-mode)

;; ------------------------------------------------------------------------
;; yaml-mode

(add-to-list 'auto-mode-alist '("\\.y[a]?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . yaml-mode))
(defun setup-yaml-mode ()
  "Setup yaml-mode."
  (flycheck-mode t)
  (flyspell-prog-mode)
  (highlight-indentation-mode)
  (highlight-indentation-current-column-mode)
  (setq highlight-indentation-set-offset 2)
  )
(add-hook 'yaml-mode-hook #'setup-yaml-mode)

;; ------------------------------------------------------------------------
;; typescript (tide + company-mode)

;; > sudo npm install tslint typescript
;; > sudo npm install -g clausreinke/typescript-tools

(add-to-list 'auto-mode-alist '("\\.ts$'" . typescript-mode))
(defun setup-tide-mode ()
  "Setup tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ------------------------------------------------------------------------
;; ruby-mode, inf-ruby + ruby-electric-mode

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append
       '(("\\.rb$" . ruby-mode)
         ("\\Capfile$" . ruby-mode)
         ("\\Gemfile$" . ruby-mode)
         ("\\[Rr]akefile$" . ruby-mode))
       auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(defun setup-ruby-mode ()
  "Setup 'ruby-mode'."
  (set-variable 'inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (setq inf-ruby-first-prompt-patternf "^\\[[0-9]+\\] pry\\((.*)\\)> *")
  (setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")
  (ruby-electric-mode t)
  (inf-ruby-minor-mode)
  )
(add-hook 'ruby-mode-hook #'setup-ruby-mode)
(eval-after-load 'auto-complete '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (ac-inf-ruby-enable)
            )
          )

;; > gem install pry pry-doc method_source
;; > gem install ruby-lint

;; ------------------------------------------------------------------------
;; robe

(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; ------------------------------------------------------------------------
;; python-mode

(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(set-variable 'gud-pdb-command-name "python -m pdb")
(set-variable 'python-shell-interpreter-args "--simple-prompt --pprint")
(defun setup-python-mode ()
  "Setup python mode."
  ;; disable elpy modules
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'highlight-indentation-mode)
  (package-initialize)
  (elpy-enable)
  (elpy-mode)
  (set-variable 'py-use-font-lock-doc-face-p t)
  (set-variable 'elpy-modules (quote (elpy-module-eldoc elpy-module-yasnippet)))
  (set-variable 'elpy-rpc-backend "jedi")
  (set-variable 'jedi:complete-on-dot t) ; optional
  ;; auto-complete
  (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  (jedi:setup)
  )
(add-hook 'python-mode-hook #'setup-python-mode)

;; M-x jedi:install-server

;; > sudo port -f install py27-ipython py36-ipython
;; > sudo port select --set ipython3 py36-ipython
;; > sudo port install py-rope py36-rope
;; > sudo pip install jedi elpy autopep8 flake8 importmagic
;; > sudo pip-3.6 install jedi elpy autopep8 flake8 importmagic

;; ------------------------------------------------------------------------
;; R

;; see. https://y-mattu.hatenablog.com/entry/rstudio_emacs

(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload 'R "ess-site" "start R" t)
(defvar ess-loaded-p nil)
(defun ess-load-hook (&optional from-iess-p)
  "Load R environment."
  (ad-deactivate 'font-lock-mode)
  (defvar ess-indent-level 2)
  (defvar ess-arg-function-offset-new-line (list ess-indent-level))
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)
  (when (not ess-loaded-p)
    ;; auto-complete-acr
    (when (require 'auto-complete-acr nil t)
      ;; auto-complete source
      (set-variable 'ess-use-auto-complete t)
      (defvar ac-sources '(ac-source-acr
                           ac-source-R
                           ac-source-filename
                           ac-source-yasnippet))
      )
    (when (require 'helm-myR nil t)
      (define-key ess-mode-map (kbd "C-c h") 'helm-for-R)
      (define-key inferior-ess-mode-map (kbd "C-c h") 'helm-for-R)
      (setq inlineR-re-funcname ".*plot.*\\|.*gg.*")
      )
    (when (require 'inlineR nil t)
      (setq inlineR-re-funcname ".*plot.*\\|.*gg.*")
      )
    (set-variable 'ess-use-ido nil)
    (set-variable 'ess-eldoc-show-on-symbol t)
    (set-variable 'ess-ask-for-ess-directory nil)
    (set-variable 'ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      (when (one-window-p)
        (split-window-below)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf))
        )
      (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
              '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
      (add-to-list 'ac-modes 'ess-mode)
      ))
  (if from-iess-p
      (if (> (length ess-process-name-list) 0)
          (when (one-window-p)
            (split-window-horizontally)
            (other-window 1)))
    (ess-force-buffer-current "Process to load into: ")))

(add-hook 'R-mode-hook 'ess-load-hook)

;; > sudo port install R
;; > sudo port install ess

;; ------------------------------------------------------------------------
;; go-mode

(defun setup-go-mode ()
  "Setup go-mode."
  (flycheck-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (go-eldoc-setup)
  (local-set-key (kbd "M-.") 'godef-jump)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (defvar c-basic-offset 4)
  )
(add-hook 'go-mode-hook #'setup-go-mode)
(eval-after-load 'go-mode
  '(progn
     (require 'go-autocomplete)))

;; > go get -u github.com/nsf/gocode

;; ------------------------------------------------------------------------
;; rust-mode

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(eval-after-load 'rust-mode
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-rust-setup)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)))

;; ------------------------------------------------------------------------
;; haskell-mode

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))
(defun setup-haskell-mode ()
  "Setup haskell-mode."
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (font-lock-mode)
  (imenu-add-menubar-index)
  (setq haskell-program-name "/usr/bin/stack ghci") ;; stack
  (inf-haskell-mode)
  (ghc-init)
  (flycheck-mode))
(add-hook 'haskell-mode-hook #'setup-haskell-mode)

;; ------------------------------------------------------------------------
;; scheme

(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "/opt/local/bin/gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run Gauche on other window."
  (interactive)
  (split-window-below (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window (get-buffer-create buf-name))))

;; > sudo port install gauche

;; ------------------------------------------------------------------------
;; clojure-mode

(add-hook 'cider-mode-hook #'clj-refactor-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook (lambda ()
                                  (company-mode)
                                  (eldoc-mode)))
(defvar nrepl-log-messages t)
(defvar cider-repl-display-in-current-window t)
(defvar cider-repl-use-clojure-font-lock t)
(defvar cider-prompt-save-file-on-load 'always-save)
(defvar cider-font-lock-dynamically '(macro core function var))
(defvar cider-overlays-use-font-lock t)
;; (cider-repl-toggle-pretty-printing)

;; > sudo port install clojure leiningen

;; ------------------------------------------------------------------------
;; ocaml

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code." t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger." t)

;; > sudo port install opam
;; > opam init

;; ------------------------------------------------------------------------
;; omnisharp-mode

(eval-after-load 'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  "Enable ominisharp mode."
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (defvar c-basic-offset 4)
  (defvar c-syntactic-indentation t)
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (setq tab-width 4)
  (c-set-style "ellemtel")
  ;; csharp-mode README.md recommends this too
  ;; (electric-pair-mode 1)       ;; Emacs 24
  ;; (electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; M-x omnisharp-install-server

;; ------------------------------------------------------------------------
;; bat-mode

;; (when (require 'dostbat nil t)
(autoload 'bat-mode "bat-mode" "bat-mode" t)
(add-to-list 'auto-mode-alist '("\\(\\.bat\\|\\.cmd\\)$" . bat-mode))
;; )

;; ------------------------------------------------------------------------
;; power-shell-mode

(defun setup-powershell-mode ()
  "Setup PowerShell Mode."
  (interactive)
  (company-mode +1))

(add-hook 'powershell-mode-hook #'setup-powershell-mode)

;; ------------------------------------------------------------------------
;; visual-basic-mode

;; (when (require 'visual-basic-mode nil t))
(autoload 'visual-basic-mode "visual-basic-mode" nil t)
(add-to-list
 'auto-mode-alist '("\\(\\.frm\\|\\.bas\\|\\.vb[as]\\|\\.cls\\)$" . visual-basic-mode))

(defun setup-visual-basic-mode ()
  "Setup visual-basic-mode."
  (auto-complete-mode t)
  (set-variable 'visual-basic-mode-indent 4)
  (when (require 'vbasense nil t)
    ;; (add-to-list
    ;;  'vbasense-tli-files "C:/Program Files/Common Files/System/ado/msado21.tlb")
    ;; (add-to-list
    ;;  'vbasense-tli-files "C:/Program Files/Common Files/Microsoft Shared/DAO/dao360.dll")
    (vbasense-config-default))
  (if (fboundp 'clean-mode-line)
      (clean-mode-line))
  )
(add-hook 'visual-basic-mode-hook #'setup-visual-basic-mode)

;; ------------------------------------------------------------------------
;; abap-mode

;; https://github.com/qianmarv

;; (when (require 'abap-mode nil t))
(autoload 'abap-mode "abap-mode" nil t)
(add-to-list 'auto-mode-alist '("\\(\\.abap\\)$" . abap-mode))
(defun setup-abap-mode ()
  "Setup abap-mode."
  (if (fboundp 'clean-mode-line)
      (clean-mode-line))
  (when (require 'abap nil t))
  (when (require 'abap-flycheck nil t))
  (hs-minor-mode -1)
  )
(add-hook 'abap-mode-hook #'setup-abap-mode)

;; ------------------------------------------------------------------------
;; helm-gtags

;; (when (require 'helm-gtags nil t))
(autoload 'helm-gtags "helm-gtags" nil t)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'go-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode-hook 'helm-gtags-mode)
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'typescript-mode-hook 'helm-gtags-mode)
(defvar helm-gtags-path-style 'root)
(defvar helm-gtags-auto-update t)
(defun setup-helm-gtags-mode ()
  "Setup helm-gtags-mode."
  (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
  (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
  (local-set-key (kbd "C-c >") 'helm-gtags-next-history)
  (local-set-key (kbd "C-x c g") 'helm-gtags-select))
(add-hook 'helm-gtags-mode-hook #'setup-helm-gtags-mode)

;; > sudo port install ctags
;; > pip-2.7 install pygments
;; > sudo port install global
;; > cp -p /opt/local/share/gtags/gtags.conf ~/.globalrc
;; > sed -i -e "s/exuberant-ctags\.la/exuberant-ctags.so/g" ~/.globalrc
;; > sed -i -e "s/pygments-parser\.la/pygments-parser.so/g" ~/.globalrc

;; ------------------------------------------------------------------------
;; nxml-mode

(eval-after-load 'nxml-mode
  '(progn
     (when (require 'auto-complete-nxml nil t))
     ))
(add-to-list 'auto-mode-alist '("web\\.config$" . nxml-mode))

;; ------------------------------------------------------------------------
;; toml-mode

(add-to-list 'auto-mode-alist '("Pipfile$" . toml-mode))
(add-to-list 'auto-mode-alist '("\\.flake8$" . toml-mode))

;; ------------------------------------------------------------------------
;; plantuml-mode

(add-to-list 'auto-mode-alist '("\\(\\.pu\\|\\.uml\\|\\.plantuml\\)$" . plantuml-mode))
;; (set-variable 'plantuml-jar-path "")  ;; depends on OS
(set-variable 'plantuml-java-args "")
(set-variable 'plantuml-jar-args "-charset UTF-8")
;; (setq plantuml-output-type "svg")

;; flycheck-plantuml
(eval-after-load 'flycheck
  '(progn
     (when (require 'flycheck-plantuml nil t)
       (flycheck-plantuml-setup))
     ))

;; ------------------------------------------------------------------------
;; textile-mode

;; (when (require 'textile-mode) nil t))
(autoload 'textile-mode "textile-mode" nil t)
(add-to-list 'auto-mode-alist '("\\(\\.textile\\)$" . textile-mode))

;; ------------------------------------------------------------------------
;; markdown-mode

(set-variable 'markdown-italic-underscore nil)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(defun setup-markdown-mode ()
  "Setup markdown-mode."
  ;; (add-hook 'before-save-hook 'markdown-toc-refresh-toc)
  ;; (set-variable 'markdown-toc-header-toc-start "<!-- TOC -->")
  ;; (set-variable 'markdown-toc-header-toc-end "<!-- /TOC -->")
  (set-variable 'markdown-fontify-code-blocks-natively t)
  (flycheck-mode t)
  (flyspell-prog-mode)
  (set (make-local-variable 'whitespace-action) nil)
  (set-variable 'markdown-preview-stylesheets (list "~/dotfiles/github.css"))
  )
(add-hook 'markdown-mode-hook #'setup-markdown-mode)

;; ------------------------------------------------------------------------
;; rst-mode

(when (require 'rst-mode nil t))
;; (autoload 'rst-mode "rst-mode" nil t)
(add-to-list 'auto-mode-alist '("\\(\\.rst\\|\\.rest\\)$" . rst-mode))

;; ------------------------------------------------------------------------
;; yasnippet

(yas-global-mode 1)
;; (setq yas-prompt-functions '(yas-ido-prompt))

;; (when (require 'helm-c-yasnippet nil t))
(autoload 'helm-c-yasnippet "helm-c-yasnippet" nil t)
(setq helm-yas-space-match-any-greedy t)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;; ------------------------------------------------------------------------
;; disable vc

(set-variable 'vc-handled-backends '(Git))
(eval-after-load 'vc '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;; ------------------------------------------------------------------------
;; magit

(global-set-key (kbd "C-x g") 'magit-status)
(set-variable 'smerge-refine-ignore-whitespace nil)
(set-variable 'magit-merge-arguments '("--no-ff"))
(set-variable 'magit-diff-refine-hunk t)
;; performance reason for Windows
;; > git config --global core.preloadindex true   # default since v2.1
;; > git config --global core.fscache true        # default since v2.8
;; > git config --global gc.auto 256
(when (eq system-type 'windows-nt)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (set-variable 'magit-commit-show-diff nil)
  (set-variable 'magit-revert-buffers 1)
)

;; ------------------------------------------------------------------------
;; magit-find-file

;; (when (require 'magit-find-file nil t))
(autoload 'magit-find-file "magit-find-file" nil t)

;; ------------------------------------------------------------------------
;; git-gutter

(global-git-gutter-mode t)

;; ------------------------------------------------------------------------
;; quickrun

(autoload 'quickrun "quickrun" nil t)
(global-set-key (kbd "<f5>") 'quickrun)

;; ------------------------------------------------------------------------
;; electric-operator

(eval-after-load 'electric-operator
  '(progn
     (electric-operator-add-rules-for-mode 'ruby-mode
                                           (cons "===" " === "))
     (electric-operator-add-rules-for-mode 'ess-mode
                                           (cons "%" nil)
                                           (cons "<-" " <- "))
     ))
(add-hook 'c-mode-hook  #'electric-operator-mode)
(add-hook 'c++-mode-hook #'electric-operator-mode)
(add-hook 'go-mode-hook #'electric-operator-mode)
(add-hook 'js2-mode-hook #'electric-operator-mode)
(add-hook 'typescript-mode-hook #'electric-operator-mode)
(add-hook 'php-mode-hook #'electric-operator-mode)
(add-hook 'python-mode-hook #'electric-operator-mode)
(add-hook 'ruby-mode-hook #'electric-operator-mode)
(add-hook 'R-mode-hook #'electric-operator-mode)

;; ------------------------------------------------------------------------
;; dumb-jump-mode

(set-variable 'dumb-jump-mode t)
(set-variable 'dumb-jump-default-project "") ; prevent search in home
(set-variable 'dumb-jump-selector 'helm)

;; ------------------------------------------------------------------------
;; git-complete

;; (when (require 'git-complete nil t))
(autoload 'git-complete "git-complete" nil t)
(global-set-key (kbd "M-g y") 'git-complete)

;; ------------------------------------------------------------------------
;; yasnippets

(set-variable 'yas-snippet-dirs '("~/.emacs.d/snippets"))

;; ------------------------------------------------------------------------
;; org-babel

(defun org-babel-init ()
  "init org-mode."
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 ;; (powershell . t)
                                 (python . t)
                                 (ruby . t)
                                 (emacs-lisp . t)
                                 )))
(add-hook 'org-mode-hook 'org-babel-init)

;; ------------------------------------------------------------------------

(provide 'cnf-program.el)
;;; cnf-program.el ends here
