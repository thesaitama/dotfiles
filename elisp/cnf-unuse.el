
;; ------------------------------------------------------------------------
;; auto-install

(require 'auto-install)
(setq auto-install-use-wget t)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; ------------------------------------------------------------------------
;; eldoc-extension)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
   (require 'eldoc-extension)
   (eldoc-mode t)
))

;; ------------------------------------------------------------------------
;; iflipb

(global-set-key (kbd "<f8>") 'iflipb-next-buffer)
(global-set-key (kbd "<f7>") 'iflipb-previous-buffer)

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

 ;; '(neo-dir-link-face ((t (:background "Gray25" :foreground "orange"))))
 ;; '(neo-file-link-face ((t (:foreground "ivory"))))
 ;; '(neo-header-face ((t (:foreground "white"))))
 ;; '(neo-root-dir-face ((t (:background "BrightBlue" :foreground "white"))))
 ;; '(neo-vc-default-face ((t (:foreground "ivory"))))
 ;; '(neo-vc-edited-face ((t (:foreground "green"))))
 ;; '(neo-vc-removed-face ((t (:foreground "red"))))
 ;; '(neo-vc-up-to-date-face ((t (:foreground "ivory"))))

;; ------------------------------------------------------------------------
;; html-mode (xhtml, html)

(setq auto-mode-alist
      (append '(("\\(\\.x?html?\\|iht\\)\\([.]?\\w+\\)*$" . html-mode))
       auto-mode-alist))

;; ------------------------------------------------------------------------
;; css-mode

(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; ------------------------------------------------------------------------
;; mmm-mode (html, php)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;(set-face-background 'mmm-default-submode-face nil)
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
;; tabbar

(require 'tabbar)
(tabbar-mode 0)
(tabbar-mwheel-mode nil)                  ;; disable mouse wheel
(setq tabbar-buffer-groups-function nil)  ;; disable groups
(setq tabbar-use-images nil)              ;; do not use images
(global-set-key (kbd "<f7>") 'tabbar-backward-tab)
(global-set-key (kbd "<f8>") 'tabbar-forward-tab)
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
(setq tabbar-separator '(2.0))
(set-face-attribute
 'tabbar-default nil
 :background "black" :foreground "white")
(set-face-attribute
 'tabbar-selected nil
 :background "white" :foreground "black" :box nil
 )
(set-face-attribute
 'tabbar-modified nil
 :background "black"  :foreground "red" :box nil
 )
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b) ; show *scratch*
                     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; hide other * buffer
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; ------------------------------------------------------------------------
;; elscreen

(setq elscreen-prefix-key (kbd "M-z"))
(elscreen-start)
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
;; psgml

(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; ------------------------------------------------------------------------
;; xml-mode (RELAX, RELAX NG, iht)

(setq auto-mode-alist
      (append '(("\\.\\(xml\\|rlx\\|pml\\|rng\\)$" . xml-mode))
              auto-mode-alist))

;; ------------------------------------------------------------------------
;; helm-qiita

(setq helm-qiita-username (my-lisp-load "helm-qiita-username"))
(setq helm-qiita-access-token (my-lisp-load "helm-qiita-access-token"))
(helm-qiita-initialize)

;; ------------------------------------------------------------------------
;; typescript (tss + auto-complete)

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

(defun typescript-setup ()
  "Typescript setup."
  (tss-config-default)
  (setq typescript-indent-level 2)
  (flycheck-mode t)
  (eldoc-mode t)
  ;;(flycheck-typescript-tslint-setup)
  (tss-setup-current-buffer))

(add-hook 'typescript-mode-hook 'typescript-setup)
(add-hook 'kill-buffer-hook 'tss--delete-process t)

;; ------------------------------------------------------------------------

(provide 'cnf-unuse.el)
;;; cnf-unuse.el ends here
