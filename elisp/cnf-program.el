;;; cnf-program.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; editorconfig-mode

(editorconfig-mode 1)
(setq editorconfig-get-properties-function
      'editorconfig-core-get-properties-hash)

;; if you need editorconfig executable
;; > sudo port install editorconfig-core-c
;; (setq edconf-exec-path "/opt/local/bin/editorconfig")

;; ------------------------------------------------------------------------
;; comment-tags

(setq comment-tags-keymap-prefix (kbd "C-c t"))
(with-eval-after-load "comment-tags"
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("KLUDGE" . ,(list :weight 'bold :foreground "#E8B71A"))
          ("XXX" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil))

(add-hook 'prog-mode-hook 'comment-tags-mode)

;; ------------------------------------------------------------------------
;; projectile

(setq projectile-keymap-prefix (kbd "C-c p"))
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-mode-line
      '(:eval (if (projectile-project-p)
                  (format " Pj" (projectile-project-name)) "")))
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )

(projectile-global-mode)

;; ------------------------------------------------------------------------
;; helm-projectile

(when (require 'helm-projectile nil t)
  (helm-projectile-on)
  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-recentf
                                 helm-source-projectile-files-list))))
(define-key global-map (kbd "C-c h") 'helm-mini)

;; ------------------------------------------------------------------------
;; debugger

(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(setq gud-tooltip-echo-area nil)
(setq gdb-command-name "ggdb")

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

(setq imenu-list-position "below")

;; ------------------------------------------------------------------------
;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-idle-change-delay 3)  ; important

;; ------------------------------------------------------------------------
;; cc-mode (built-in)

(setq auto-mode-alist
      (append
       '(
         ("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ("\\.c\\+\\+$" . c++-mode)
         ("\\.cpp$". c++-mode)
         ("\\.cc$" . c++-mode)
         ("\\.hh$" . c++-mode)
         )
       auto-mode-alist))

(add-hook 'c-mode-common-hook
 '(lambda ()
    (c-set-style "gnu")
    (setq c-tab-always-indent t)
    (define-key c-mode-map "\C-cc" 'compile)
    (setq compilation-window-height 8)
    (define-key c-mode-map "\C-cd" 'gdb)
    (c-set-offset 'case-label 2)
    ))

;; ------------------------------------------------------------------------
;; php-mode

(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\(\\.php\\|\\.tpl\\)$" . php-mode))
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
  '(lambda ()
     (setq php-manual-path "/usr/local/share/php/doc/php-chunked-xhtml")
     (setq php-search-url "http://www.phppro.jp/")
     (setq php-manual-url "http://www.phppro.jp/phpmanual")
   )
  )

;; manual install
;; > wget http://jp.php.net/get/php_manual_ja.tar.gz/from/this/mirror -O php_manual_ja.tar.gz
;; > tar xfvz php_manual_ja.tar.gz
;; > sudo mkdir -p /usr/local/share/php/doc/
;; > sudo cp -r php-chunked-xhtml /usr/local/share/php/doc/
;; > rm -rf php-chunked-xhtml

;; ------------------------------------------------------------------------
;; ac-php

;; M-x install-elisp-from-emacswiki php-completion.el

(add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (when (require 'ac-php nil t))
               (setq ac-sources  '(ac-source-php
                                   ac-source-php-completion
                                   ac-source-filename))
               )
            )

;; ------------------------------------------------------------------------
;; php-eldoc

(defun php-mode-options ()
  (php-eldoc-enable)
  (cond
    ((string-match-p "^/my-project-folder")
     (php-eldoc-probe-load "http://my-project.com/probe.php?secret=sesame"))
    ((string-match-p "^/other-project-folder")
     (php-eldoc-probe-load "http://localhost/otherproject/probe.php?secret=sesame"))))
(add-hook 'php-mode-hook 'php-mode-options)

;; ------------------------------------------------------------------------
;; cperl-mode

(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode))  auto-mode-alist ))
(setq interpreter-mode-alist (append '(("perl" . cperl-mode))
                                     interpreter-mode-alist))

;; ------------------------------------------------------------------------
;; web-mode + emmet-mode, ac-emmet

;; (when (require 'web-mode nil t))
(autoload 'web-mode "web-mode" nil t)
(setq auto-mode-alist
      (append
       '(
         ("\\.as[cp]x$" . web-mode)
         ("\\.iht$" . web-mode)
         ("\\.[agj]sp$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.[xp]?html?$" . web-mode)
         ("\\(\\.sass\\|\\.s?css\\)$" . web-mode)
         )
       auto-mode-alist))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-html-offset 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset 2)
  (setq web-mode-java-offset 2)
  (setq web-mode-asp-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  ;; (auto-complete-mode t)
  (emmet-mode)
  (ac-emmet-html-setup))
(add-hook 'web-mode-hook 'web-mode-hook)
(setq web-mode-auto-close-style 2)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
;; (setq web-mode-enable-css-colorization t)

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets ac-source-words-in-buffer ac-source-abbrev))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
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
            )
          )
(setq ac-js2-evaluate-calls t)

;; > sudo npm install -g eslint babel-eslint

;; ------------------------------------------------------------------------
;; tern

(setq tern-command '("tern" "--no-port-file"))
(add-hook 'js2-mode-hook (lambda ()(tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; > sudo npm install -g tern

;; ------------------------------------------------------------------------
;; json-mode

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; ------------------------------------------------------------------------
;; yaml-mode

(add-to-list 'auto-mode-alist '("\\.y[a]?ml$" . yaml-mode))

;; ------------------------------------------------------------------------
;; typescript

;; > sudo npm install tslint typescript
;; > sudo npm install -g clausreinke/typescript-tools

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; ------------------------------------------------------------------------
;; typescript (tide + company-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)

;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ------------------------------------------------------------------------
;; ruby-mode

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append
       '(
         ("\\.rb$" . ruby-mode)
         ("\\Capfile$" . ruby-mode)
         ("\\Gemfile$" . ruby-mode)
         ("\\[Rr]akefile$" . ruby-mode)
         )
       auto-mode-alist))

(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))

;; ------------------------------------------------------------------------
;; inf-ruby, ruby-electric-mode

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook
  (lambda ()
    (ruby-electric-mode t)
    (inf-ruby-minor-mode)
    (inf-ruby-switch-setup)
    (inf-ruby-keys)
  )
  )

(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

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

;; for Ipython
(setq python-shell-interpreter-args "--simple-prompt --pprint")

;; -----------------------------------------------------------------------
;; elpy (python-mode)

(add-hook 'python-mode-hook
          '(lambda ()
             (package-initialize)
             (elpy-enable)
             (elpy-mode)
             (setq elpy-modules (quote (elpy-module-eldoc elpy-module-yasnippet)))
             ;; auto-complete
             (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
             (add-to-list 'ac-sources 'ac-source-jedi-direct)
             ))

;; M-x jedi:install-server
(setq elpy-rpc-backend "jedi")
(setq jedi:complete-on-dot t) ; optional

(add-hook 'python-mode-hook 'jedi:setup)

;; see python special settings (cnf-osx.el)

;; disable flymake
(remove-hook 'elpy-modules 'elpy-module-flymake)

;; disable indent highlight
(remove-hook 'elpy-modules 'highlight-indentation-mode)

;; > sudo port -f install py27-ipython py36-ipython
;; > sudo port select --set ipython3 py36-ipython
;; > sudo port install py-rope py36-rope
;; > sudo pip install jedi elpy autopep8 flake8 importmagic
;; > sudo pip-3.6 install jedi elpy autopep8 flake8 importmagic

;; ------------------------------------------------------------------------
;; R
;; see. https://y-mattu.hatenablog.com/entry/rstudio_emacs

;; (when (require 'ess-site nil t))

(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload 'R "ess-site" "start R" t)

(setq ess-loaded-p nil)

(defun ess-load-hook (&optional from-iess-p)
  (setq ess-indent-level 2)
  (setq ess-arg-function-offset-new-line (list ess-indent-level))
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)

  (when (not ess-loaded-p)
    (setq ess-use-auto-complete t)
    ;; disable ido
    (setq ess-use-ido nil)
    (setq ess-eldoc-show-on-symbol t)
    (setq ess-ask-for-ess-directory nil)
    (setq ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      (when (one-window-p)
        (split-window-below)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf)))
      (when (and ess-use-auto-complete (require 'auto-complete nil t))
        (add-to-list 'ac-modes 'ess-mode)
        (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
                '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
        ;; auto-complete source
        (setq ac-sources '(ac-source-acr
                           ac-source-R
                           ac-source-filename
                           ac-source-yasnippet)))))

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
;; auto-complete-acr

(when (require 'auto-complete-acr nil t))
;; (autoload 'auto-complete-acr "auto-complete-acr" nil t)

;; ------------------------------------------------------------------------
;; go-mode

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (go-eldoc-setup)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (setq indent-tabs-mode nil)
                          (setq c-basic-offset 4)
                          (setq tab-width 4)))

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)))

;; > go get -u github.com/nsf/gocode

;; ------------------------------------------------------------------------
;; rust-mode

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(eval-after-load "rust-mode"
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

(add-hook 'haskell-mode-hook (lambda ()
                               (turn-on-haskell-indentation)
                               (turn-on-haskell-doc-mode)
                               (font-lock-mode)
                               (imenu-add-menubar-index)
                               (setq haskell-program-name "/usr/bin/stack ghci") ;; stack
                               (inf-haskell-mode)
                               (ghc-init)
                               (flycheck-mode)))

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
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

;; > sudo port install gauche

;; ------------------------------------------------------------------------
;; clojure-mode

(add-hook 'cider-mode-hook #'clj-refactor-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook (lambda ()
                                  (company-mode)
                                  (eldoc-mode)))
(setq nrepl-log-messages t
      cider-repl-display-in-current-window t
      cider-repl-use-clojure-font-lock t
      cider-prompt-save-file-on-load 'always-save
      cider-font-lock-dynamically '(macro core function var)
      cider-overlays-use-font-lock t)
;; (cider-repl-toggle-pretty-printing)

;; > sudo port install clojure leiningen

;; ------------------------------------------------------------------------
;; omnisharp-mode

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

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

;; (when (require 'visual-basic-mode nil t)
(autoload 'visual-basic-mode "visual-basic-mode" nil t)
(add-to-list
 'auto-mode-alist '("\\(\\.frm\\|\\.bas\\|\\.vb[as]\\|\\.cls\\)$" . visual-basic-mode))

(add-hook 'visual-basic-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (setq visual-basic-mode-indent 4)
             (require 'vbasense)
             ;; (add-to-list 'vbasense-tli-files "C:/Program Files/Common Files/System/ado/msado21.tlb")
             ;; (add-to-list 'vbasense-tli-files "C:/Program Files/Common Files/Microsoft Shared/DAO/dao360.dll")
             (vbasense-config-default)
             )
          )
;; )

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

(setq helm-gtags-path-style 'root)
;; (setq helm-gtags-ignore-case t)
(setq helm-gtags-auto-update t)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
              (local-set-key (kbd "C-c >") 'helm-gtags-next-history)
              (local-set-key (kbd "C-x c g") 'helm-gtags-select)
              ))

;; > sudo port install ctags
;; > pip-2.7 install pygments
;; > sudo port install global
;; > cp -p /opt/local/share/gtags/gtags.conf ~/.globalrc
;; > sed -i -e "s/exuberant-ctags\.la/exuberant-ctags.so/g" ~/.globalrc
;; > sed -i -e "s/pygments-parser\.la/pygments-parser.so/g" ~/.globalrc

;; ------------------------------------------------------------------------
;; auto-complete-nxml

(eval-after-load 'nxml-mode
  '(progn
     (when (require 'auto-complete-nxml nil t))
     ))

;; ------------------------------------------------------------------------
;; plantuml-mode

(setq auto-mode-alist
      (append '(("\\.\\(pu\\)$" . plantuml-mode))  auto-mode-alist ))
;; (setq plantuml-jar-path "")  ;; depends on OS
(setq plantuml-java-options "")
;; (setq plantuml-output-type "svg")
(setq plantuml-options "-charset UTF-8")

;; flycheck-plantuml
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; ------------------------------------------------------------------------
;; textile-mode

;; (when (require 'textile-mode) nil t))
(autoload 'textile-mode "textile-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; ------------------------------------------------------------------------
;; rst-mode

;; (when (require 'rst-mode nil t))
(autoload 'rst-mode "rst-mode" nil t)
(add-to-list 'auto-mode-alist '("\\(\\.rst\\|\\.rest\\)$" . rst-mode))

;; ------------------------------------------------------------------------
;; yasnippet

(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))

;; (when (require 'helm-c-yasnippet nil t))
(autoload 'helm-c-yasnippet "helm-c-yasnippet" nil t)
(setq helm-yas-space-match-any-greedy t)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;; ------------------------------------------------------------------------
;; magit

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk t)
(setq magit-merge-arguments '("--no-ff"))
(setq smerge-refine-ignore-whitespace nil)

;; ------------------------------------------------------------------------
;; magit-find-file

;; (when (require 'magit-find-file nil t))
(autoload 'magit-find-file "magit-find-file" nil t)

;; ------------------------------------------------------------------------
;; quickrun

(when (require 'quickrun nil t)
  (global-set-key (kbd "<f5>") 'quickrun)
  )

;; ------------------------------------------------------------------------
;; electric-operator

(add-hook 'prog-mode-hook #'electric-operator-mode)
;; (add-hook 'c++-mode-hook #'electric-operator-mode)
;; (add-hook 'c-mode-hook  #'electric-operator-mode)
;; (add-hook 'go-mode-hook #'electric-operator-mode)
;; (add-hook 'js2-mode-hook #'electric-operator-mode)
;; (add-hook 'php-mode-hook #'electric-operator-mode)
;; (add-hook 'python-mode-hook #'electric-operator-mode)
;; (add-hook 'ruby-mode-hook #'electric-operator-mode)
;; (add-hook 'typescript-mode-hook #'electric-operator-mode)

;; ------------------------------------------------------------------------
;; dumb-jump-mode

(setq dumb-jump-mode t)

;; ------------------------------------------------------------------------
;; git-complete

;; (when (require 'git-complete nil t))
(autoload 'git-complete "git-complete" nil t)
(global-set-key (kbd "M-g y") 'git-complete)

;; ------------------------------------------------------------------------

(provide 'cnf-program.el)
;;; cnf-program.el ends here
