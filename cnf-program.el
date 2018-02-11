;;; cnf-program.el --- thesaitama Emacs configuration

;;; Commentary:
;;
;; This file is part of thesaitama Emacs configuration

;;; Code:

;; ------------------------------------------------------------------------
;; editorconfig

(editorconfig-mode 1)
(setq editorconfig-get-properties-function
      'editorconfig-core-get-properties-hash)

;; if you need editorconfig excutables
;; > sudo port install editorconfig-core-c
;(setq edconf-exec-path "/opt/local/bin/editorconfig")

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

(add-hook 'c-mode-hook  'comment-tags-mode)
(add-hook 'python-mode-hook 'comment-tags-mode)
(add-hook 'php-mode-hook 'comment-tags-mode)
(add-hook 'js2-mode-hook 'comment-tags-mode)
(add-hook 'typescript-mode-hook 'comment-tags-mode)

;; ------------------------------------------------------------------------
;; projectile

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-mode-line
      '(:eval (if (projectile-project-p)
                  (format " Pj" (projectile-project-name)) "")))
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )

;; ------------------------------------------------------------------------
;; helm-projectile

(when (and (require 'helm-projectile))
  (helm-projectile-on)
  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-recentf
                                 helm-source-projectile-files-list))))
(define-key global-map (kbd "C-c h") 'helm-mini)

;; ------------------------------------------------------------------------
;; debugger

(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-use-separate-io-buffer t)
(setq gud-tooltip-echo-area nil)

;; ------------------------------------------------------------------------
;; imenu-list

(setq imenu-list-position "below")

;; ------------------------------------------------------------------------
;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

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
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
  '(lambda ()
     (setq php-manual-path "/usr/share/doc/php/html")
     (setq php-search-url "http://www.phppro.jp/")
     (setq php-manual-url "http://www.phppro.jp/phpmanual")
   )
  )

;; ------------------------------------------------------------------------
;; ac-php

;; M-x install-elisp-from-emacswiki php-completion.el

(add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
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

(require 'web-mode)
(setq auto-mode-alist
      (append
       '(
         ("\\.as[cp]x$" . web-mode)
         ("\\.iht$" . web-mode)
         ("\\.jsp$" . web-mode)
         ("\\.x?html?$" . web-mode)
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
  ;;(auto-complete-mode t)
  (emmet-mode)
  (ac-emmet-html-setup))
(add-hook 'web-mode-hook 'web-mode-hook)
(setq web-mode-auto-close-style 2)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
;;(setq web-mode-enable-css-colorization t)

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("css" . (ac-source-css-property))
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

;; > npm -g install js-beautify

;; ------------------------------------------------------------------------
;; js2-mode

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; ------------------------------------------------------------------------
;; json-mode

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; ------------------------------------------------------------------------
;; yaml-mode

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y[a]?ml$" . yaml-mode))

;; ------------------------------------------------------------------------
;; typescript

;; > sudo npm install tslint
;; > sudo npm install typescript
;; > sudo npm install -g clausreinke/typescript-tools

(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; dot use tss-config-default
(defun typescript-setup ()
  (setq typescript-indent-level 2)
  (flycheck-mode t)
  ;;(flycheck-typescript-tslint-setup)
  (tss-setup-current-buffer))

(add-hook 'typescript-mode-hook 'typescript-setup)
(add-hook 'kill-buffer-hook 'tss--delete-process t)

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
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(add-hook 'ruby-mode-hook
  (lambda ()
    (ruby-electric-mode t)
    (inf-ruby-minor-mode)
    (inf-ruby-switch-setup)
    (inf-ruby-keys)
  )
  )

(setq ruby-electric-expand-delimiters-list nil)

;; ------------------------------------------------------------------------
;; robe

(add-hook 'ruby-mode-hook 'robe-mode)
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; > gem install pry pry-doc method_source
;; > gem install ruby-lint

;; ------------------------------------------------------------------------
;; python-mode

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; elpy

(add-hook 'python-mode-hook
          '(lambda ()
             (package-initialize)
             (elpy-enable)
             (elpy-mode)
             ))

(setq elpy-rpc-backend "jedi")

;M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional

;; ------------------------------------------------------------------------
;; go-mode

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook' 'gofmt-before-save)
                          (add-hook 'go-mode-hook 'go-eldoc-setup)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (setq indent-tabs-mode nil)
                          (setq c-basic-offset 4)
                          (setq tab-width 4)))

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)))

;; > go get -u github.com/nsf/gocode

;; ------------------------------------------------------------------------
;; helm-gtags

(require 'helm-gtags)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode 'helm-gtags-mode)
(add-hook 'typescript-mode 'helm-gtags-mode)

(setq helm-gtags-path-style 'root)
(setq helm-gtags-ignore-case t)
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
;; yasnippet

(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)

;; ------------------------------------------------------------------------
;; magit

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk t)
(setq smerge-refine-ignore-whitespace nil)

;; ------------------------------------------------------------------------
;; magit-find-file

(require 'magit-find-file)

;; ------------------------------------------------------------------------
;; quickrun

(require 'quickrun)
(global-set-key (kbd "<f5>") 'quickrun)

;; ------------------------------------------------------------------------
;; electric-operator

(add-hook 'c-mode-hook  #'electric-operator-mode)
(add-hook 'python-mode-hook #'electric-operator-mode)
(add-hook 'php-mode-hook #'electric-operator-mode)
(add-hook 'ruby-mode-hook #'electric-operator-mode)
(add-hook 'js2-mode-hook #'electric-operator-mode)
(add-hook 'typescript-mode-hook #'electric-operator-mode)

;; ------------------------------------------------------------------------

(provide 'cnf-program.el)
;;; cnf-program.el ends here
