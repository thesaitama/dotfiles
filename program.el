
;; ------------------------------------------------------------------------
;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

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
;; web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\(\\.x?html?\\|.iht\\|.css\\)\\([.]?\\w+\\)*$". web-mode))
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
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)

;; ------------------------------------------------------------------------
;; js2-mode

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

;; ------------------------------------------------------------------------
;; psgml

(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; ------------------------------------------------------------------------
;; xml-mode (RELAX, RELAX NG, iht)

(setq auto-mode-alist
      (append '(("\\.\\(xml\\|rlx\\|pml\\|rng\\)$" . xml-mode))
       auto-mode-alist))

;; ------------------------------------------------------------------------
;; python-mode

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) ; optional

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
;; neotree

(require 'neotree)
(global-set-key (kbd "<f10>") 'neotree-toggle)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)
(setq neo-keymap-style 'concise)
(setq neo-smart-open t)
;(setq neo-vc-integration '(face char))
;; popwin
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook (lambda () (setq neo-persist-show t))))

