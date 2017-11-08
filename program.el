

;; ------------------------------------------------------------------------
;; flycheck

(global-flycheck-mode)

;; ------------------------------------------------------------------------
;; projectile

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

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
;; mmm-mode (html, php)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-css)
(mmm-add-classes
 '((embedded-js
    :submode javascript-mode
    :front "<script[^>]*>"
    :back "</script>")))
(mmm-add-mode-ext-class nil "\\(\\.x?html?\\|php\\)(\\..+\\)?$" 'embedded-js)

;; ------------------------------------------------------------------------
;; js2-mode

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ------------------------------------------------------------------------
;; css-mode

(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; ------------------------------------------------------------------------
;; psgml

(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; ------------------------------------------------------------------------
;; xml-mode (RELAX, RELAX NG, iht)

(setq auto-mode-alist
      (append '(("\\.\\(xml\\|rlx\\|pml\\|rng\\)$" . xml-mode))
       auto-mode-alist))

;; ------------------------------------------------------------------------
;; html-mode (xhtml, html)

(setq auto-mode-alist
      (append '(("\\(\\.x?html?\\|iht\\)\\([.]?\\w+\\)*$" . html-mode))
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
;; popwin との共存
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook (lambda () (setq neo-persist-show t))))

