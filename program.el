
;; ------------------------------------------------------------------------
;; editorconfig

(require 'editorconfig)
(editorconfig-mode 1)
(setq editorconfig-get-properties-function
      'editorconfig-core-get-properties-hash)

;; if you need editorconfig excutables
;; > sudo port install editorconfig-core-c
;(setq edconf-exec-path "/opt/local/bin/editorconfig")

;; ------------------------------------------------------------------------
;; projectile

(require 'projectile)
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
;; eldoc-extension)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
   (require 'eldoc-extension)
   (eldoc-mode t)
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
;; yaml-mode

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y[a]?ml$" . yaml-mode))

;; ------------------------------------------------------------------------
;; web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\(\\.x?html?\\|\\.iht\\)\\([.]?\\w+\\)*$". web-mode))
(add-to-list 'auto-mode-alist '("\\(\\.sass\\|\\.s?css\\)\\([.]?\\w+\\)*$". web-mode))
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset 2)
  (setq web-mode-java-offset 2)
  (setq web-mode-asp-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)
;;(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; ------------------------------------------------------------------------
;; json-mode

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; ------------------------------------------------------------------------
;; js2-mode

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-hook 'js2-mode-hook #'js2-refactor-mode)

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
;; nxml-mode

(add-hook 'nxml-mode-hook
  (lambda ()
    (setq nxml-slash-auto-complete-flag t)
    (setq nxml-child-indent 1)
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
  )
)

;; ------------------------------------------------------------------------
;; python-mode

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional

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
;; neotree

(require 'neotree)
(global-set-key (kbd "<f10>") 'neotree-toggle)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)
(setq neo-keymap-style 'concise)
(setq neo-dont-be-alone t)
(setq neo-window-fixed-size nil)
;(setq neo-smart-open t)
(setq neo-vc-integration '(face char))
;; popwin
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook (lambda () (setq neo-persist-show t))))
;; helm project
(defadvice helm-projectile-find-file (after helm-projectile-find-file activate)
  (neotree-dir default-directory))

;; ------------------------------------------------------------------------
;; quickrun

(require 'quickrun)
(global-set-key (kbd "<f5>") 'quickrun)


